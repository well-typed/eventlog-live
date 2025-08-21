{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (asum)
import Control.Exception (Exception (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.DList (DList)
import Data.DList qualified as D
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Machine (Process, ProcessT, await, mapping, repeatedly, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Data.Void (Void)
import Data.Word (Word32, Word64)
import GHC.Eventlog.Live (EventlogSocket, runWithEventlogSocket)
import GHC.Eventlog.Live.Machines (Attr, AttrValue (..), MemReturnData (..), Metric (..), ThreadStateSpan (..), Tick, WithStartTime (..))
import GHC.Eventlog.Live.Machines qualified as ELM
import GHC.Eventlog.Live.Options
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..), ThreadId)
import Lens.Family2 ((&), (.~), (^.))
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Network.GRPC.Common.Protobuf qualified as G
import Options.Applicative qualified as O
import Options.Applicative.Extra qualified as O
import PackageInfo_eventlog_live_otelcol qualified as EventlogLive
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as OTS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OT
import System.IO qualified as IO
import Text.Printf (printf)

main :: IO ()
main = do
  Options{..} <- O.execParser options
  let OpenTelemetryCollectorOptions{..} = openTelemetryCollectorOptions
  let OpenTelemetryExporterOptions{..} = openTelemetryExporterOptions
  let attrServiceName = ("service.name", maybe AttrNull (AttrText . (.serviceName)) maybeServiceName)
  -- Construct the metrics pipeline
  let metricsPipelineWith conn =
        fanout
          [ processHeapEvents maybeHeapProfBreakdown
          , processCapabilityUsage
          ]
          ~> mapping D.toList
          ~> asScopeMetrics
            [ OM.scope .~ eventlogLiveScope
            ]
          ~> asResourceMetric []
          ~> asExportMetricServiceRequest
          ~> otelcolResourceMetricsExporter conn
          ~> displayExceptions
  -- Construct the traces pipelin
  let tracesPipelineWith conn =
        processThreadStateSpans
          ~> asScopeSpans
            [ OT.scope .~ eventlogLiveScope
            ]
          ~> asResourceSpan
            [ OT.resource
                .~ messageWith
                  [ OM.attributes .~ mapMaybe toMaybeKeyValue [attrServiceName]
                  ]
            ]
          ~> asExportTraceServiceRequest
          ~> otelcolResourceSpansExporter conn
          ~> displayExceptions
  -- Run all requested pipelines in parallel
  let pipelinesFor conn =
        [metricsPipelineWith conn | exportMetrics]
          <> [tracesPipelineWith conn | exportTraces]
  G.withConnection G.def openTelemetryCollectorServer $ \conn -> do
    runWithEventlogSocket batchInterval Nothing eventlogSocket maybeEventlogLogFile $
      ELM.liftTick ELM.withStartTime ~> fanout (pipelinesFor conn)

displayExceptions :: (MonadIO m, Exception e) => ProcessT m e Void
displayExceptions = repeatedly $ await >>= liftIO . IO.hPutStrLn IO.stderr . displayException

--------------------------------------------------------------------------------
-- Thread Events
--------------------------------------------------------------------------------

processCapabilityUsage ::
  Process (Tick (WithStartTime Event)) (DList OM.Metric)
processCapabilityUsage =
  ELM.liftTick (ELM.processCapabilityUsage ~> asNumberDataPoint)
    ~> ELM.batchByTickList
    ~> asGauge
    ~> asMetric
      [ OM.name .~ "CapabilityUsage"
      , OM.description .~ "Report the usage by capability."
      , OM.unit .~ "10^0"
      ]
    ~> mapping D.singleton

processThreadStateSpans ::
  (MonadIO m) =>
  ProcessT m (Tick (WithStartTime Event)) [OT.Span]
processThreadStateSpans =
  ELM.sortByBatchTick (.value.evTime)
    ~> ELM.liftTick (ELM.processThreadStateSpans ~> asSpan)
    ~> ELM.batchByTickList

--------------------------------------------------------------------------------
-- Heap Events
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapEvents maybeHeapProfBreakdown =
  fanout
    [ processHeapAllocated
    , processBlocksSize
    , processHeapSize
    , processHeapLive
    , processMemReturn
    , processHeapProfSample maybeHeapProfBreakdown
    ]

--------------------------------------------------------------------------------
-- HeapAllocated

processHeapAllocated :: Process (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapAllocated =
  ELM.liftTick (ELM.processHeapAllocatedData ~> asNumberDataPoint)
    ~> ELM.batchByTickList
    ~> asSum
      [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
      , OM.isMonotonic .~ True
      ]
    ~> asMetric
      [ OM.name .~ "HeapAllocated"
      , OM.description .~ "Report when a new chunk of heap has been allocated by the indicated capability set."
      , OM.unit .~ "By"
      ]
    ~> mapping D.singleton

--------------------------------------------------------------------------------
-- HeapSize

processHeapSize :: Process (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapSize =
  ELM.liftTick (ELM.processHeapSizeData ~> asNumberDataPoint)
    ~> ELM.batchByTickList
    ~> asGauge
    ~> asMetric
      [ OM.name .~ "HeapSize"
      , OM.description .~ "Report the current heap size, calculated by the allocated number of megablocks."
      , OM.unit .~ "By"
      ]
    ~> mapping D.singleton

--------------------------------------------------------------------------------
-- BlocksSize

processBlocksSize :: Process (Tick (WithStartTime Event)) (DList OM.Metric)
processBlocksSize =
  ELM.liftTick (ELM.processBlocksSizeData ~> asNumberDataPoint)
    ~> ELM.batchByTickList
    ~> asGauge
    ~> asMetric
      [ OM.name .~ "BlocksSize"
      , OM.description .~ "Report the current heap size, calculated by the allocated number of blocks."
      , OM.unit .~ "By"
      ]
    ~> mapping D.singleton

--------------------------------------------------------------------------------
-- HeapLive

processHeapLive :: Process (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapLive =
  ELM.liftTick (ELM.processHeapLiveData ~> asNumberDataPoint)
    ~> ELM.batchByTickList
    ~> asGauge
    ~> asMetric
      [ OM.name .~ "HeapLive"
      , OM.description .~ "Report the current heap size, calculated by the allocated number of megablocks."
      , OM.unit .~ "By"
      ]
    ~> mapping D.singleton

--------------------------------------------------------------------------------
-- MemReturn

processMemReturn :: Process (Tick (WithStartTime Event)) (DList OM.Metric)
processMemReturn =
  ELM.liftTick ELM.processMemReturnData
    ~> ELM.batchByTickList
    ~> fanout
      [ mapping (fmap (toNumberDataPoint . fmap (.current)))
          ~> asGauge
          ~> asMetric
            [ OM.name .~ "MemCurrent"
            , OM.description .~ "Report the number of megablocks currently allocated."
            , OM.unit .~ "{mblock}"
            ]
          ~> mapping D.singleton
      , mapping (fmap (toNumberDataPoint . fmap (.needed)))
          ~> asGauge
          ~> asMetric
            [ OM.name .~ "MemNeeded"
            , OM.description .~ "Report the number of megablocks currently needed."
            , OM.unit .~ "{mblock}"
            ]
          ~> mapping D.singleton
      , mapping (fmap (toNumberDataPoint . fmap (.returned)))
          ~> asGauge
          ~> asMetric
            [ OM.name .~ "MemReturned"
            , OM.description .~ "Report the number of megablocks currently being returned to the OT."
            , OM.unit .~ "{mblock}"
            ]
          ~> mapping D.singleton
      ]

--------------------------------------------------------------------------------
-- HeapProfSample

processHeapProfSample ::
  (MonadIO m) =>
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapProfSample maybeHeapProfBreakdown =
  ELM.liftTick (ELM.processHeapProfSampleData maybeHeapProfBreakdown ~> asNumberDataPoint)
    ~> ELM.batchByTickList
    ~> asGauge
    ~> asMetric
      [ OM.name .~ "HeapProfSample"
      , OM.description .~ "Report a heap profile sample."
      , OM.unit .~ "By"
      ]
    ~> mapping D.singleton

--------------------------------------------------------------------------------
-- Machines
--------------------------------------------------------------------------------

eventlogLiveScope :: OC.InstrumentationScope
eventlogLiveScope =
  messageWith
    [ OC.name .~ T.pack EventlogLive.name
    , OC.version .~ T.pack (showVersion EventlogLive.version)
    ]

asExportMetricsServiceRequest :: Process [OM.ResourceMetrics] OMS.ExportMetricsServiceRequest
asExportMetricsServiceRequest = mapping $ (defMessage &) . (OM.resourceMetrics .~)

asExportTracesServiceRequest :: Process [OT.ResourceSpans] OTS.ExportTraceServiceRequest
asExportTracesServiceRequest = mapping $ (defMessage &) . (OTS.resourceSpans .~)

asExportMetricServiceRequest :: Process OM.ResourceMetrics OMS.ExportMetricsServiceRequest
asExportMetricServiceRequest = mapping (: []) ~> asExportMetricsServiceRequest

asExportTraceServiceRequest :: Process OT.ResourceSpans OTS.ExportTraceServiceRequest
asExportTraceServiceRequest = mapping (: []) ~> asExportTracesServiceRequest

asResourceMetrics :: [OM.ResourceMetrics -> OM.ResourceMetrics] -> Process [OM.ScopeMetrics] OM.ResourceMetrics
asResourceMetrics mod = mapping $ \scopeMetrics ->
  messageWith ((OM.scopeMetrics .~ scopeMetrics) : mod)

asResourceSpans :: [OT.ResourceSpans -> OT.ResourceSpans] -> Process [OT.ScopeSpans] OT.ResourceSpans
asResourceSpans mod = mapping $ \scopeSpans ->
  messageWith ((OT.scopeSpans .~ scopeSpans) : mod)

asResourceSpan :: [OT.ResourceSpans -> OT.ResourceSpans] -> Process OT.ScopeSpans OT.ResourceSpans
asResourceSpan mod = mapping (: []) ~> asResourceSpans mod

asResourceMetric :: [OM.ResourceMetrics -> OM.ResourceMetrics] -> Process OM.ScopeMetrics OM.ResourceMetrics
asResourceMetric mod = mapping (: []) ~> asResourceMetrics mod

asScopeSpans :: [OT.ScopeSpans -> OT.ScopeSpans] -> Process [OT.Span] OT.ScopeSpans
asScopeSpans mod = mapping $ \spans ->
  messageWith ((OT.spans .~ spans) : mod)

asScopeMetrics :: [OM.ScopeMetrics -> OM.ScopeMetrics] -> Process [OM.Metric] OM.ScopeMetrics
asScopeMetrics mod = mapping $ \metrics ->
  messageWith ((OM.metrics .~ metrics) : mod)

asSpan :: Process ThreadStateSpan OT.Span
asSpan = mapping toSpan

asMetric :: [OM.Metric -> OM.Metric] -> Process OM.Metric'Data OM.Metric
asMetric mod = mapping $ \metric'data ->
  messageWith ((OM.maybe'data' .~ Just metric'data) : mod)

asGauge :: Process [OM.NumberDataPoint] OM.Metric'Data
asGauge =
  repeatedly $
    await >>= \case
      dataPoints
        | null dataPoints -> pure ()
        | otherwise -> yield . OM.Metric'Gauge . messageWith $ [OM.dataPoints .~ dataPoints]

asSum :: [OM.Sum -> OM.Sum] -> Process [OM.NumberDataPoint] OM.Metric'Data
asSum mod =
  repeatedly $
    await >>= \case
      dataPoints
        | null dataPoints -> pure ()
        | otherwise -> yield . OM.Metric'Sum . messageWith $ (OM.dataPoints .~ dataPoints) : mod

asNumberDataPoint :: (IsNumberDataPoint'Value v) => Process (Metric v) OM.NumberDataPoint
asNumberDataPoint = mapping toNumberDataPoint

--------------------------------------------------------------------------------
-- Interpret data
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Interpret spans

toSpan :: ThreadStateSpan -> OT.Span
toSpan ThreadStateSpan{..} =
  messageWith
    [ OT.traceId .~ toTraceId thread
    , OT.spanId .~ toSpanId spanId
    , OT.name .~ T.pack (show threadState)
    , OT.kind .~ OT.Span'SPAN_KIND_INTERNAL
    , OT.startTimeUnixNano .~ startTimeUnixNano
    , OT.endTimeUnixNano .~ endTimeUnixNano
    , OT.status
        .~ messageWith
          [ OT.code .~ OT.Status'STATUS_CODE_OK
          ]
    ]

toSpanId :: Word64 -> ByteString
toSpanId = BS.toStrict . BSB.toLazyByteString . BSB.word64BE

-- TODO: use a hash function
toTraceId :: ThreadId -> ByteString
toTraceId thread =
  BS.toStrict . BSB.toLazyByteString . mconcat . fmap BSB.word32BE $
    [0, 0, 0, thread]

--------------------------------------------------------------------------------
-- Interpret metrics

class IsNumberDataPoint'Value v where
  toNumberDataPoint'Value :: v -> OM.NumberDataPoint'Value

instance IsNumberDataPoint'Value Double where
  toNumberDataPoint'Value :: Double -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsDouble

instance IsNumberDataPoint'Value Word32 where
  toNumberDataPoint'Value :: Word32 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

-- | __Warning__: This instance may cause overflow.
instance IsNumberDataPoint'Value Word64 where
  toNumberDataPoint'Value :: Word64 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

toNumberDataPoint :: (IsNumberDataPoint'Value v) => Metric v -> OM.NumberDataPoint
toNumberDataPoint i =
  messageWith
    [ OM.maybe'value .~ Just (toNumberDataPoint'Value i.value)
    , OM.timeUnixNano .~ fromMaybe 0 i.maybeTimeUnixNano
    , OM.startTimeUnixNano .~ fromMaybe 0 i.maybeStartTimeUnixNano
    , OM.attributes .~ mapMaybe toMaybeKeyValue i.attr
    ]

toMaybeKeyValue :: Attr -> Maybe OC.KeyValue
toMaybeKeyValue (k, v) =
  toMaybeAnyValue v <&> \v ->
    messageWith
      [ OC.key .~ k
      , OC.value .~ v
      ]

toMaybeAnyValue :: AttrValue -> Maybe OC.AnyValue
toMaybeAnyValue = \case
  AttrInt v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt8 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt16 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt32 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt64 v -> Just $ messageWith [OC.intValue .~ v]
  AttrWord v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord8 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord16 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord32 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord64 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrDouble v -> Just $ messageWith [OC.doubleValue .~ v]
  AttrText v -> Just $ messageWith [OC.stringValue .~ v]
  AttrNull -> Nothing

--------------------------------------------------------------------------------
-- DSL for writing messages

-- | Construct a message with a list of modifications applied.
messageWith :: (Message msg) => [msg -> msg] -> msg
messageWith = foldr ($) defMessage

--------------------------------------------------------------------------------
-- OpenTelemetry Collector exporters
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- OpenTelemetry Collector Trace Exporter

data ExportSpansError
  = ExportSpansError
  { rejectedSpans :: Int64
  , errorMessage :: Text
  }
  deriving (Show)

instance Exception ExportSpansError where
  displayException :: ExportSpansError -> String
  displayException ExportSpansError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedSpans errorMessage

otelcolResourceSpansExporter :: G.Connection -> ProcessT IO OTS.ExportTraceServiceRequest ExportSpansError
otelcolResourceSpansExporter conn =
  repeatedly $
    await >>= \exportTraceServiceRequest -> do
      G.Proto resp <- liftIO (G.nonStreaming conn (G.rpc @(Protobuf OTS.TraceService "export")) (G.Proto exportTraceServiceRequest))
      unless (resp ^. OTS.partialSuccess . OTS.rejectedSpans == 0) $ do
        yield
          ExportSpansError
            { rejectedSpans = resp ^. OTS.partialSuccess . OTS.rejectedSpans
            , errorMessage = resp ^. OTS.partialSuccess . OTS.errorMessage
            }

type instance G.RequestMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- OpenTelemetry Collector Metrics Exporter

data ExportMetricsError
  = ExportMetricsError
  { rejectedDataPoints :: Int64
  , errorMessage :: Text
  }
  deriving (Show)

instance Exception ExportMetricsError where
  displayException :: ExportMetricsError -> String
  displayException ExportMetricsError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedDataPoints errorMessage

otelcolResourceMetricsExporter :: G.Connection -> ProcessT IO OMS.ExportMetricsServiceRequest ExportMetricsError
otelcolResourceMetricsExporter conn =
  repeatedly $
    await >>= \exportMetricsServiceRequest -> do
      G.Proto resp <- liftIO (G.nonStreaming conn (G.rpc @(Protobuf OMS.MetricsService "export")) (G.Proto exportMetricsServiceRequest))
      unless (resp ^. OMS.partialSuccess . OMS.rejectedDataPoints == 0) $ do
        yield
          ExportMetricsError
            { rejectedDataPoints = resp ^. OMS.partialSuccess . OMS.rejectedDataPoints
            , errorMessage = resp ^. OMS.partialSuccess . OMS.errorMessage
            }

type instance G.RequestMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

options :: O.ParserInfo Options
options =
  O.info
    ( optionsParser
        O.<**> O.helperWith (O.long "help" <> O.help "Show this help text.")
        O.<**> O.simpleVersioner (showVersion EventlogLive.version)
    )
    O.idm

data Options = Options
  { batchInterval :: Int
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , maybeServiceName :: Maybe ServiceName
  , eventlogSocket :: EventlogSocket
  , openTelemetryCollectorOptions :: OpenTelemetryCollectorOptions
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> batchIntervalParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> O.optional serviceNameParser
    <*> eventlogSocketParser
    <*> openTelemetryCollectorOptionsParser

--------------------------------------------------------------------------------
-- Service Name

newtype ServiceName = ServiceName {serviceName :: Text}

serviceNameParser :: O.Parser ServiceName
serviceNameParser =
  ServiceName
    <$> O.strOption
      ( O.long "service-name"
          <> O.metavar "STRING"
          <> O.help "The name of the profiled service."
      )

--------------------------------------------------------------------------------
-- OpenTelemetry Collector configuration

data OpenTelemetryCollectorOptions = OpenTelemetryCollectorOptions
  { openTelemetryCollectorServer :: G.Server
  , openTelemetryExporterOptions :: OpenTelemetryExporterOptions
  }

openTelemetryCollectorOptionsParser :: O.Parser OpenTelemetryCollectorOptions
openTelemetryCollectorOptionsParser =
  O.parserOptionGroup "OpenTelemetry Collector Server Options" $
    OpenTelemetryCollectorOptions
      <$> otelcolServerParser
      <*> otelcolExporterOptionsParsers

data OpenTelemetryExporterOptions = OpenTelemetryExporterOptions
  { exportMetrics :: Bool
  , exportTraces :: Bool
  }

otelcolExporterOptionsParsers :: O.Parser OpenTelemetryExporterOptions
otelcolExporterOptionsParsers =
  OpenTelemetryExporterOptions
    <$> exportMetricsParser
    <*> exportTracesParser
 where
  exportMetricsParser =
    -- flipped from O.switch to match negated semantics
    O.flag True False . mconcat $
      [ O.long "otelcol-no-metrics"
      , O.help "Disable metrics exporter."
      ]
  exportTracesParser =
    -- flipped from O.switch to match negated semantics
    O.flag True False . mconcat $
      [ O.long "otelcol-no-traces"
      , O.help "Disable traces exporter."
      ]

otelcolServerParser :: O.Parser G.Server
otelcolServerParser =
  makeServer
    <$> otelcolAddressParser
    <*> O.switch (O.long "otelcol-ssl" <> O.help "Use SSL.")
    <*> otelcolServerValidationParser
    <*> otelcolSslKeyLogParser
 where
  makeServer :: G.Address -> Bool -> G.ServerValidation -> G.SslKeyLog -> G.Server
  makeServer address ssl serverValidation sslKeyLog
    | ssl = G.ServerSecure serverValidation sslKeyLog address
    | otherwise = G.ServerInsecure address

otelcolAddressParser :: O.Parser G.Address
otelcolAddressParser =
  G.Address
    <$> O.strOption
      ( O.long "otelcol-host"
          <> O.metavar "HOTT"
          <> O.help "Server hostname."
      )
    <*> O.option
      O.auto
      ( O.long "otelcol-port"
          <> O.metavar "PORT"
          <> O.help "Server TCP port."
          <> O.value 4317
      )
    <*> O.optional
      ( O.strOption
          ( O.long "otelcol-authority"
              <> O.metavar "HOTT"
              <> O.help "Server authority."
          )
      )

otelcolServerValidationParser :: O.Parser G.ServerValidation
otelcolServerValidationParser =
  asum
    [ G.ValidateServer <$> otelcolCertificateStoreSpecParser
    , pure G.NoServerValidation
    ]
 where
  otelcolCertificateStoreSpecParser :: O.Parser G.CertificateStoreSpec
  otelcolCertificateStoreSpecParser =
    makeCertificateStoreSpec
      <$> O.optional
        ( O.strOption
            ( O.long "otelcol-certificate-store"
                <> O.metavar "FILE"
                <> O.help "Store for certificate validation."
            )
        )
   where
    makeCertificateStoreSpec :: Maybe FilePath -> G.CertificateStoreSpec
    makeCertificateStoreSpec = maybe G.certStoreFromSystem G.certStoreFromPath

otelcolSslKeyLogParser :: O.Parser G.SslKeyLog
otelcolSslKeyLogParser =
  asum
    [ G.SslKeyLogPath
        <$> O.strOption
          ( O.long "otelcol-ssl-key-log"
              <> O.metavar "FILE"
              <> O.help "Use file to log SSL keys."
          )
    , O.flag
        G.SslKeyLogNone
        G.SslKeyLogFromEnv
        ( O.long "otelcol-ssl-key-log-from-env"
            <> O.help "Use SSLKEYLOGFILE to log SSL keys."
        )
    ]
