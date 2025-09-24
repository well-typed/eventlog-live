{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol (
  main,
) where

import Control.Applicative (asum)
import Control.Exception (Exception (..), catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Either (partitionEithers)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Machine (Process, ProcessT, asParts, await, construct, filtered, mapping, repeatedly, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Data.Void (Void)
import Data.Word (Word32, Word64)
import GHC.Eventlog.Live.Machines (Attr, AttrValue (..), CapabilityUsageSpan, MemReturnData (..), Metric (..), ThreadStateSpan (..), Tick, WithStartTime (..), (~=))
import GHC.Eventlog.Live.Machines qualified as ELM
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Socket (runWithEventlogSocket)
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..), ThreadId)
import Lens.Family2 ((&), (.~), (^.))
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Network.GRPC.Common.Protobuf qualified as G
import Options.Applicative qualified as O
import Options.Applicative.Extra qualified as O
import Paths_eventlog_live_otelcol qualified as EventlogLive
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
import System.Random (StdGen, initStdGen)
import System.Random.Compat (uniformByteString)
import Text.Printf (printf)

{- |
The main function for @eventlog-live-otelcol@.
-}
main :: IO ()
main = do
  Options{..} <- O.execParser options
  let OpenTelemetryCollectorOptions{..} = openTelemetryCollectorOptions
  let OpenTelemetryExporterOptions{..} = openTelemetryExporterOptions
  let attrServiceName = ("service.name", maybe AttrNull (AttrText . (.serviceName)) maybeServiceName)
  G.withConnection G.def openTelemetryCollectorServer $ \conn -> do
    runWithEventlogSocket
      eventlogSocket
      eventlogSocketTimeout
      eventlogSocketTimeoutExponent
      batchInterval
      Nothing
      maybeEventlogLogFile
      $ ELM.liftTick ELM.withStartTime
        ~> fanout
          [ processHeapEvents verbosity maybeHeapProfBreakdown
              ~> mapping (fmap Left)
          , processThreadEvents verbosity
          ]
        ~> mapping (partitionEithers . D.toList)
        ~> fanout
          [ filtered (const exportMetrics)
              ~> mapping fst
              ~> asScopeMetrics
                [ OM.scope .~ eventlogLiveScope
                ]
              ~> asResourceMetric []
              ~> asExportMetricServiceRequest
              ~> exportResourceMetrics conn
              ~> mapping (either displayException displayException)
              ~> errorWriter
          , filtered (const exportTraces)
              ~> mapping snd
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
              ~> exportResourceSpans conn
              ~> mapping (either displayException displayException)
              ~> errorWriter
          ]

errorWriter :: (MonadIO m) => ProcessT m String Void
errorWriter = repeatedly $ await >>= liftIO . IO.hPutStrLn IO.stderr

-- dumpBatch :: (MonadIO m, Show a) => ProcessT m [a] [a]
-- dumpBatch = traversing (\as -> for_ as (liftIO . print) >> pure as)

--------------------------------------------------------------------------------
-- processThreadEvents
--------------------------------------------------------------------------------

data OneOf a b c = A !a | B !b | C !c

processThreadEvents ::
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (Tick (WithStartTime Event)) (DList (Either OM.Metric OT.Span))
processThreadEvents verbosity =
  ELM.sortByBatchTick (.value.evTime)
    ~> ELM.liftTick
      ( fanout
          [ ELM.processGCSpans verbosity
              ~> mapping (D.singleton . A)
          , ELM.processThreadStateSpans' ELM.tryGetTimeUnixNano (.value) ELM.setWithStartTime'value verbosity
              ~> fanout
                [ ELM.asMutatorSpans' (.value) ELM.setWithStartTime'value
                    ~> mapping (D.singleton . B)
                , mapping (D.singleton . C)
                ]
          ]
      )
    ~> ELM.liftTick
      ( asParts
          ~> mapping repackCapabilityUsageSpanOrThreadStateSpan
      )
    ~> fanout
      [ ELM.liftTick
          ( mapping leftToMaybe
              ~> asParts
          )
          ~> fanout
            [ ELM.liftTick
                ( ELM.processCapabilityUsageMetrics
                    ~> asNumberDataPoint
                )
                ~> ELM.batchByTickList
                ~> asSum
                  [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
                  , OM.isMonotonic .~ True
                  ]
                ~> asMetric
                  [ OM.name .~ "CapabilityUsage"
                  , OM.description .~ "Report the duration of each capability usage span."
                  , OM.unit .~ "ns"
                  ]
                ~> mapping (D.singleton . Left)
            , ELM.liftTick
                ( ELM.dropStartTime
                    ~> asSpan
                    ~> mapping (D.singleton . Right)
                )
                ~> ELM.batchByTick
            ]
      , ELM.liftTick
          ( mapping rightToMaybe
              ~> asParts
              ~> asSpan
              ~> mapping (D.singleton . Right)
          )
          ~> ELM.batchByTick
      ]
 where
  repackCapabilityUsageSpanOrThreadStateSpan = \case
    A i -> Left $ fmap Left i
    B i -> Left $ fmap Right i
    C i -> Right i.value

{- |
Internal helper.
Get the `Left` value, if any.
-}
leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

{- |
Internal helper.
Get the `Right` value, if any.
-}
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

--------------------------------------------------------------------------------
-- processHeapEvents
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Verbosity ->
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapEvents verbosity maybeHeapProfBreakdown =
  fanout
    [ processHeapAllocated
    , processBlocksSize
    , processHeapSize
    , processHeapLive
    , processMemReturn
    , processHeapProfSample verbosity maybeHeapProfBreakdown
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
      [ ELM.liftBatch (asMemCurrent ~> asNumberDataPoint)
          ~> asGauge
          ~> asMetric
            [ OM.name .~ "MemCurrent"
            , OM.description .~ "Report the number of megablocks currently allocated."
            , OM.unit .~ "{mblock}"
            ]
          ~> mapping D.singleton
      , ELM.liftBatch (asMemNeeded ~> asNumberDataPoint)
          ~> asGauge
          ~> asMetric
            [ OM.name .~ "MemNeeded"
            , OM.description .~ "Report the number of megablocks currently needed."
            , OM.unit .~ "{mblock}"
            ]
          ~> mapping D.singleton
      , ELM.liftBatch (asMemReturned ~> asNumberDataPoint)
          ~> asGauge
          ~> asMetric
            [ OM.name .~ "MemReturned"
            , OM.description .~ "Report the number of megablocks currently being returned to the OS."
            , OM.unit .~ "{mblock}"
            ]
          ~> mapping D.singleton
      ]

asMemCurrent :: Process (Metric MemReturnData) (Metric Word32)
asMemCurrent = mapping (fmap (.current))

asMemNeeded :: Process (Metric MemReturnData) (Metric Word32)
asMemNeeded = mapping (fmap (.needed))

asMemReturned :: Process (Metric MemReturnData) (Metric Word32)
asMemReturned = mapping (fmap (.returned))

--------------------------------------------------------------------------------
-- HeapProfSample

processHeapProfSample ::
  (MonadIO m) =>
  Verbosity ->
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapProfSample verbosity maybeHeapProfBreakdown =
  ELM.liftTick (ELM.processHeapProfSampleData verbosity maybeHeapProfBreakdown ~> asNumberDataPoint)
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

-- 2025-09-22:
-- Once `cabal2nix` supports Cabal 3.12, this can once again use the value from:
-- `PackageInfo_eventlog_live_otelcol.name`.
eventlogLiveName :: String
eventlogLiveName = "eventlog-live-otelcol"

eventlogLiveScope :: OC.InstrumentationScope
eventlogLiveScope =
  messageWith
    [ OC.name .~ T.pack eventlogLiveName
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

asMetric :: [OM.Metric -> OM.Metric] -> Process OM.Metric'Data OM.Metric
asMetric mod = mapping $ toMetric mod

toMetric :: [OM.Metric -> OM.Metric] -> OM.Metric'Data -> OM.Metric
toMetric mod metric'data = messageWith ((OM.maybe'data' .~ Just metric'data) : mod)

asGauge :: Process [OM.NumberDataPoint] OM.Metric'Data
asGauge =
  repeatedly $
    await >>= \case
      dataPoints
        | null dataPoints -> pure ()
        | otherwise -> yield . OM.Metric'Gauge . messageWith $ [OM.dataPoints .~ dataPoints]

asSum :: [OM.Sum -> OM.Sum] -> Process [OM.NumberDataPoint] OM.Metric'Data
asSum mod = repeatedly $ await >>= \dataPoints -> for_ (toSum mod dataPoints) yield

toSum :: [OM.Sum -> OM.Sum] -> [OM.NumberDataPoint] -> Maybe OM.Metric'Data
toSum mod dataPoints
  | null dataPoints = Nothing
  | otherwise = Just . OM.Metric'Sum . messageWith $ (OM.dataPoints .~ dataPoints) : mod

asNumberDataPoint :: (IsNumberDataPoint'Value v) => Process (Metric v) OM.NumberDataPoint
asNumberDataPoint = mapping toNumberDataPoint

--------------------------------------------------------------------------------
-- Interpret data
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Interpret spans

class AsSpan v where
  -- | The `Key` type is used to index a `HashMap` in the default definition of `asSpan`.
  type Key v

  -- | The `toKey` function extracts a `Key` from the input value.
  toKey ::
    -- | The input value.
    v ->
    Key v

  toSpan ::
    -- | The input value.
    v ->
    -- | The trace ID.
    ByteString ->
    -- | The span ID.
    ByteString ->
    OT.Span

  -- | The `asSpan` machine processes values @v@ into OpenTelemetry spans `OT.Span`.
  asSpan :: (MonadIO m) => ProcessT m v OT.Span
  default asSpan :: (MonadIO m, Hashable (Key v)) => ProcessT m v OT.Span
  asSpan = construct $ go (mempty, Nothing)
   where
    -- go :: (HashMap (Key v) ByteString, Maybe StdGen) -> PlanT (Is v) OT.Span m Void
    go (traceIds, maybeGen) = do
      -- Ensure the StdGen is initialised
      gen0 <- maybe (liftIO initStdGen) pure maybeGen
      -- Receive the next value
      i <- await
      -- Ensure the next value has a trace ID
      let ensureTraceId :: Maybe ByteString -> ((ByteString, StdGen), Maybe ByteString)
          ensureTraceId = wrap . maybe (uniformByteString 16 gen0) (,gen0)
           where
            wrap out@(traceId, _gen) = (out, Just traceId)
      let ((traceId, gen1), traceIds') = M.alterF ensureTraceId (toKey i) traceIds
      -- Ensure the next value has a span ID
      let (spanId, gen2) = uniformByteString 8 gen1
      -- Yield a span
      yield $ toSpan i traceId spanId
      -- Continue
      go (traceIds', Just gen2)

--------------------------------------------------------------------------------
-- Interpret capability usage spans

instance AsSpan CapabilityUsageSpan where
  type Key CapabilityUsageSpan = Int

  toKey :: CapabilityUsageSpan -> Int
  toKey = (.cap)

  toSpan :: CapabilityUsageSpan -> ByteString -> ByteString -> OT.Span
  toSpan i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ ELM.showCapabilityUserCategory user
      , OT.kind .~ OT.Span'SPAN_KIND_INTERNAL
      , OT.startTimeUnixNano .~ i.startTimeUnixNano
      , OT.endTimeUnixNano .~ i.endTimeUnixNano
      , OT.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "capability" ~= i.cap
            , "user" ~= user
            ]
      , OT.status
          .~ messageWith
            [ OT.code .~ OT.Status'STATUS_CODE_OK
            ]
      ]
   where
    user = ELM.capabilityUser i

--------------------------------------------------------------------------------
-- Interpret thread state spans

instance AsSpan ThreadStateSpan where
  type Key ThreadStateSpan = ThreadId

  toKey :: ThreadStateSpan -> ThreadId
  toKey = (.thread)

  toSpan :: ThreadStateSpan -> ByteString -> ByteString -> OT.Span
  toSpan i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ ELM.showThreadStateCategory i.threadState
      , OT.kind .~ OT.Span'SPAN_KIND_INTERNAL
      , OT.startTimeUnixNano .~ i.startTimeUnixNano
      , OT.endTimeUnixNano .~ i.endTimeUnixNano
      , OT.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "capability" ~= ELM.threadStateCap i.threadState
            , "status" ~= (show <$> ELM.threadStateStatus i.threadState)
            ]
      , OT.status
          .~ messageWith
            [ OT.code .~ OT.Status'STATUS_CODE_OK
            ]
      ]

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

data ExportTraceError
  = ExportTraceError
  { rejectedSpans :: Int64
  , errorMessage :: Text
  }
  deriving (Show)

instance Exception ExportTraceError where
  displayException :: ExportTraceError -> String
  displayException ExportTraceError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedSpans errorMessage

sendResourceSpans :: G.Connection -> OTS.ExportTraceServiceRequest -> IO (Maybe (Either G.GrpcError ExportTraceError))
sendResourceSpans conn exportTraceServiceRequest =
  fmap (fmap Right) doGrpc `catch` handleGrpcError
 where
  doGrpc :: IO (Maybe ExportTraceError)
  doGrpc = do
    G.nonStreaming conn (G.rpc @(Protobuf OTS.TraceService "export")) (G.Proto exportTraceServiceRequest) >>= \case
      G.Proto resp
        | resp ^. OTS.partialSuccess . OTS.rejectedSpans == 0 -> pure Nothing
        | otherwise -> pure $ Just ExportTraceError{..}
       where
        rejectedSpans = resp ^. OTS.partialSuccess . OTS.rejectedSpans
        errorMessage = resp ^. OTS.partialSuccess . OTS.errorMessage
  handleGrpcError :: G.GrpcError -> IO (Maybe (Either G.GrpcError ExportTraceError))
  handleGrpcError = pure . pure . Left

exportResourceSpans :: G.Connection -> ProcessT IO OTS.ExportTraceServiceRequest (Either G.GrpcError ExportTraceError)
exportResourceSpans conn =
  repeatedly $
    await >>= \exportTraceServiceRequest ->
      liftIO (sendResourceSpans conn exportTraceServiceRequest)
        >>= traverse_ yield

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

sendResourceMetrics :: G.Connection -> OMS.ExportMetricsServiceRequest -> IO (Maybe (Either G.GrpcError ExportMetricsError))
sendResourceMetrics conn exportMetricsServiceRequest =
  fmap (fmap Right) doGrpc `catch` handleGrpcError
 where
  doGrpc :: IO (Maybe ExportMetricsError)
  doGrpc = do
    G.nonStreaming conn (G.rpc @(Protobuf OMS.MetricsService "export")) (G.Proto exportMetricsServiceRequest) >>= \case
      G.Proto resp
        | resp ^. OMS.partialSuccess . OMS.rejectedDataPoints == 0 -> pure Nothing
        | otherwise -> pure $ Just ExportMetricsError{..}
       where
        rejectedDataPoints = resp ^. OMS.partialSuccess . OMS.rejectedDataPoints
        errorMessage = resp ^. OMS.partialSuccess . OMS.errorMessage
  handleGrpcError :: G.GrpcError -> IO (Maybe (Either G.GrpcError ExportMetricsError))
  handleGrpcError = pure . pure . Left

exportResourceMetrics :: G.Connection -> ProcessT IO OMS.ExportMetricsServiceRequest (Either G.GrpcError ExportMetricsError)
exportResourceMetrics conn =
  repeatedly $
    await >>= \exportMetricsServiceRequest ->
      liftIO (sendResourceMetrics conn exportMetricsServiceRequest)
        >>= traverse_ yield

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
  { eventlogSocket :: EventlogSocket
  , eventlogSocketTimeout :: Double
  , eventlogSocketTimeoutExponent :: Double
  , batchInterval :: Int
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , maybeServiceName :: Maybe ServiceName
  , verbosity :: Verbosity
  , openTelemetryCollectorOptions :: OpenTelemetryCollectorOptions
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSocketParser
    <*> eventlogSocketTimeoutParser
    <*> eventlogSocketTimeoutExponentParser
    <*> batchIntervalParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> O.optional serviceNameParser
    <*> verbosityParser
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
          <> O.metavar "HOST"
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
              <> O.metavar "HOST"
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
