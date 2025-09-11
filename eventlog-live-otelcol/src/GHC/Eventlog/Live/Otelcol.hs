{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Otelcol (
  main,
) where

import Control.Applicative (Alternative (..), asum)
import Control.Exception (Exception (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.DList (DList)
import Data.DList qualified as D
import Data.Either (partitionEithers)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Machine (Process, ProcessT, await, construct, filtered, mapping, repeatedly, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Version (showVersion)
import Data.Void (Void)
import Data.Word (Word32, Word64)
import GHC.Eventlog.Live (EventlogSocket, runWithEventlogSocket)
import GHC.Eventlog.Live.Machines (Attr, AttrValue (..), CapabilityUsageSpan, MemReturnData (..), Metric (..), ThreadStateSpan (..), Tick, Verbosity, WithStartTime (..), (~=))
import GHC.Eventlog.Live.Machines qualified as ELM
import GHC.Eventlog.Live.Options
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..), ThreadId)
import GHC.Records (HasField (..))
import Lens.Family2 (Lens', (&), (.~), (^.))
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Network.GRPC.Common.Protobuf qualified as G
import Network.GRPC.Spec qualified as GS
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
import System.Random (StdGen, initStdGen, uniformByteString)
import Text.Printf (printf)

main :: IO ()
main = do
  Options{..} <- O.execParser options
  let OpenTelemetryCollectorOptions{..} = openTelemetryCollectorOptions
  let OpenTelemetryExporterOptions{..} = openTelemetryExporterOptions
  let attrServiceName = ("service.name", maybe AttrNull (AttrText . (.serviceName)) maybeServiceName)
  G.withConnection G.def openTelemetryCollectorServer $ \conn -> do
    runWithEventlogSocket batchInterval Nothing eventlogSocket maybeEventlogLogFile $
      ELM.liftTick ELM.withStartTime
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
              ~> displayExceptions
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
              ~> displayExceptions
          ]

displayExceptions :: (MonadIO m, Exception e) => ProcessT m e Void
displayExceptions = repeatedly $ await >>= liftIO . IO.hPutStrLn IO.stderr . displayException

-- dumpBatch :: (MonadIO m, Show a) => ProcessT m [a] [a]
-- dumpBatch = traversing (\as -> for_ as (liftIO . print) >> pure as)

--------------------------------------------------------------------------------
-- processThreadEvents
--------------------------------------------------------------------------------

processThreadEvents ::
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (Tick (WithStartTime Event)) (DList (Either OM.Metric OT.Span))
processThreadEvents verbosity =
  ELM.sortByBatchTick (.value.evTime)
    ~> fanout
      [ ELM.liftTick (ELM.processCapabilityUsageSpans verbosity)
          ~> fanout
            [ ELM.liftTick (ELM.processCapabilityUsageMetrics ~> asNumberDataPoint)
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
                ~> mapping Left
                ~> mapping D.singleton
            , ELM.liftTick
                ( ELM.dropStartTime
                    ~> asSpan
                    ~> mapping (D.singleton . Right)
                )
                ~> ELM.batchByTick
            ]
      , ELM.liftTick
          ( ELM.processThreadStateSpans verbosity
              ~> asSpan
              ~> mapping (D.singleton . Right)
          )
          ~> ELM.batchByTick
      ]

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
            , OM.description .~ "Report the number of megablocks currently being returned to the OT."
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

exportResourceSpans :: G.Connection -> ProcessT IO OTS.ExportTraceServiceRequest ExportSpansError
exportResourceSpans conn =
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

sendResourceMetrics :: G.Connection -> OMS.ExportMetricsServiceRequest -> IO OMS.ExportMetricsServiceResponse
sendResourceMetrics conn req = do
  G.Proto resp <- G.nonStreaming conn (G.rpc @(Protobuf OMS.MetricsService "export")) (G.Proto req)
  pure resp

exportResourceMetrics :: G.Connection -> ProcessT IO OMS.ExportMetricsServiceRequest ExportMetricsError
exportResourceMetrics conn = repeatedly go
 where
  go =
    await >>= \exportMetricsServiceRequest -> do
      resp <- liftIO $ sendResourceMetrics conn exportMetricsServiceRequest
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
-- Sized Message Splitter

{- |
This machine splits its inputs until they are smaller than the given
maximum size, or passes them on unsplit if they are unsplittable.
-}
splitUntilSmallerThan :: (Sizeable a b, Split a) => Int64 -> Process a b
splitUntilSmallerThan maxSizeBytes =
  repeatedly $
    await >>= \a ->
      maybeSplitUntilSmallerThan maxSizeBytes a
        & maybe (yield (toSized a)) (traverse_ yield)

maybeSplitUntilSmallerThan :: (Sizeable a b, Split a) => Int64 -> a -> Maybe [b]
maybeSplitUntilSmallerThan maxSizeBytes = fmap D.toList . go
 where
  go a
    | sizeBytes b <= maxSizeBytes = Just . D.singleton $ b
    | otherwise = maybeSplit a >>= bitraverse go go <&> uncurry (<>)
   where
    b = toSized a

--------------------------------------------------------------------------------
-- Sized Messages

class Sized a where
  sizeBytes :: a -> Int64

instance Sized BSL.ByteString where
  sizeBytes :: BSL.ByteString -> Int64
  sizeBytes = BSL.length

class (Sized b) => Sizeable a b | a -> b where
  toSized :: a -> b

instance Sizeable OMS.ExportMetricsServiceRequest BSL.ByteString where
  toSized :: OMS.ExportMetricsServiceRequest -> BSL.ByteString
  toSized = GS.rpcSerializeInput (G.Proxy @(GS.Protobuf OMS.MetricsService "export")) . G.Proto

instance Sizeable OTS.ExportTraceServiceRequest BSL.ByteString where
  toSized :: OTS.ExportTraceServiceRequest -> BSL.ByteString
  toSized = GS.rpcSerializeInput (G.Proxy @(GS.Protobuf OTS.TraceService "export")) . G.Proto

--------------------------------------------------------------------------------
-- Splitting Message

class Split a where
  maybeSplit :: a -> Maybe (a, a)

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

maybeSplitBy :: (Split a) => Lens' s a -> s -> Maybe (s, s)
maybeSplitBy = maybeSplitBy' maybeSplit

maybeSplitBy' :: (a -> Maybe (a, a)) -> Lens' s a -> s -> Maybe (s, s)
maybeSplitBy' splitter fld s = fmap (both (flip (fld .~) s)) $ splitter $ s ^. fld

maybeSplitEach :: (Split a) => [a] -> Maybe ([a], [a])
maybeSplitEach = maybeSplitEach' maybeSplit

maybeSplitEach' :: (a -> Maybe (a, a)) -> [a] -> Maybe ([a], [a])
maybeSplitEach' splitter as
  | null as2 = Nothing
  | otherwise = Just (as2, as1)
 where
  (as2, as1) = first D.toList (traverse splitWriter as)
  splitWriter a = maybe (D.empty, a) (first D.singleton) (splitter a)

instance (Split a) => Split (Maybe a) where
  maybeSplit :: Maybe a -> Maybe (Maybe a, Maybe a)
  maybeSplit = maybe Nothing (fmap (both Just) . maybeSplit)

instance Split (Vector a) where
  maybeSplit :: Vector a -> Maybe (Vector a, Vector a)
  maybeSplit v
    | V.length v <= 1 = Nothing
    | otherwise = Just (V.splitAt (V.length v `div` 2) v)

instance Split OM.Gauge where
  maybeSplit :: OM.Gauge -> Maybe (OM.Gauge, OM.Gauge)
  maybeSplit = maybeSplitBy OM.vec'dataPoints

instance Split OM.Sum where
  maybeSplit :: OM.Sum -> Maybe (OM.Sum, OM.Sum)
  maybeSplit = maybeSplitBy OM.vec'dataPoints

instance Split OM.Histogram where
  maybeSplit :: OM.Histogram -> Maybe (OM.Histogram, OM.Histogram)
  maybeSplit = maybeSplitBy OM.vec'dataPoints

instance Split OM.ExponentialHistogram where
  maybeSplit :: OM.ExponentialHistogram -> Maybe (OM.ExponentialHistogram, OM.ExponentialHistogram)
  maybeSplit = maybeSplitBy OM.vec'dataPoints

instance Split OM.Summary where
  maybeSplit :: OM.Summary -> Maybe (OM.Summary, OM.Summary)
  maybeSplit = maybeSplitBy OM.vec'dataPoints

instance Split OM.Metric'Data where
  maybeSplit :: OM.Metric'Data -> Maybe (OM.Metric'Data, OM.Metric'Data)
  maybeSplit = \case
    OM.Metric'Gauge gauge ->
      both OM.Metric'Gauge <$> maybeSplit gauge
    OM.Metric'Sum sum ->
      both OM.Metric'Sum <$> maybeSplit sum
    OM.Metric'Histogram histogram ->
      both OM.Metric'Histogram <$> maybeSplit histogram
    OM.Metric'ExponentialHistogram exponentialhistogram ->
      both OM.Metric'ExponentialHistogram <$> maybeSplit exponentialhistogram
    OM.Metric'Summary summary ->
      both OM.Metric'Summary <$> maybeSplit summary

instance Split OM.Metric where
  maybeSplit :: OM.Metric -> Maybe (OM.Metric, OM.Metric)
  maybeSplit = maybeSplitBy OM.maybe'data'

instance Split OM.ScopeMetrics where
  maybeSplit :: OM.ScopeMetrics -> Maybe (OM.ScopeMetrics, OM.ScopeMetrics)
  maybeSplit scopeMetrics =
    maybeSplitBy OM.vec'metrics scopeMetrics
      <|> maybeSplitBy' maybeSplitEach OM.metrics scopeMetrics

instance Split OM.ResourceMetrics where
  maybeSplit :: OM.ResourceMetrics -> Maybe (OM.ResourceMetrics, OM.ResourceMetrics)
  maybeSplit resourceMetrics =
    maybeSplitBy OM.vec'scopeMetrics resourceMetrics
      <|> maybeSplitBy' maybeSplitEach OM.scopeMetrics resourceMetrics

instance Split OMS.ExportMetricsServiceRequest where
  maybeSplit :: OMS.ExportMetricsServiceRequest -> Maybe (OMS.ExportMetricsServiceRequest, OMS.ExportMetricsServiceRequest)
  maybeSplit exportMetricsServiceRequest =
    maybeSplitBy OM.vec'resourceMetrics exportMetricsServiceRequest
      <|> maybeSplitBy' maybeSplitEach OM.resourceMetrics exportMetricsServiceRequest

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
  , verbosity :: Verbosity
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
  , maxRecvMsgSize :: MaxRecvMsgSize
  }

otelcolExporterOptionsParsers :: O.Parser OpenTelemetryExporterOptions
otelcolExporterOptionsParsers =
  OpenTelemetryExporterOptions
    <$> exportMetricsParser
    <*> exportTracesParser
    <*> maxRecvMsgSizeParser
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

newtype MaxRecvMsgSize = MaxRecvMsgSize {mebibytes :: Int64}

instance HasField "bytes" MaxRecvMsgSize Int64 where
  getField :: MaxRecvMsgSize -> Int64
  getField maxRecvMsgSize = (2 ^ (20 :: Int64)) * maxRecvMsgSize.mebibytes

maxRecvMsgSizeParser :: O.Parser MaxRecvMsgSize
maxRecvMsgSizeParser =
  MaxRecvMsgSize
    <$> O.option
      O.auto
      ( O.long "otelcol-max-recv-msg-size-mib"
          <> O.help "The maximum message size in MiB."
          <> O.value 4
      )

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
