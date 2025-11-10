{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol (
  main,
) where

import Control.Applicative (Alternative (..), asum)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Default (Default (..))
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Machine (MachineT, Process, ProcessT, asParts, await, construct, mapping, repeatedly, stopped, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Last (..), Sum (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Version (showVersion)
import Data.Word (Word32, Word64)
import Data.Yaml qualified as Y
import GHC.Debug.Stub.Compat (MyGhcDebugSocket (..), withMyGhcDebug)
import GHC.Eventlog.Live.Data.Attribute
import GHC.Eventlog.Live.Data.Group (Group, GroupBy, GroupedBy)
import GHC.Eventlog.Live.Data.Group qualified as DG
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Logger (logDebug)
import GHC.Eventlog.Live.Machine.Analysis.Capability (CapabilityUsageSpan)
import GHC.Eventlog.Live.Machine.Analysis.Capability qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Heap (MemReturnData (..))
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Thread (ThreadStateSpan (..))
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Otelcol.Config (Config)
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Default.Raw (defaultConfigString)
import GHC.Eventlog.Live.Otelcol.Exporter (exportResourceMetrics, exportResourceSpans)
import GHC.Eventlog.Live.Otelcol.Stats (Stat (..), eventCountTick, processStats)
import GHC.Eventlog.Live.Socket (runWithEventlogSource)
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.Eventlog.Socket qualified as Eventlog.Socket
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..), ThreadId)
import GHC.Records (HasField)
import Lens.Family2 ((&), (.~))
import Network.GRPC.Client qualified as G
import Network.GRPC.Common qualified as G
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra qualified as OE
import Paths_eventlog_live_otelcol qualified as EventlogLive
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as OTS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OT
import System.Random (StdGen, initStdGen)
import System.Random.Compat (uniformByteString)

{- |
The main function for @eventlog-live-otelcol@.
-}
main :: IO ()
main = do
  Options{..} <- O.execParser options

  -- Instument THIS PROGRAM with eventlog-socket and/or ghc-debug.
  let MyDebugOptions{..} = myDebugOptions
  withMyEventlogSocket maybeMyEventlogSocket
  withMyGhcDebug verbosity maybeMyGhcDebugSocket $ do
    --
    -- Read the configuration file.
    config <- flip (maybe (pure def)) maybeConfigFile $ \configFile -> do
      logDebug verbosity $ "Reading configuration file from " <> T.pack configFile
      config <- C.readConfig configFile
      logDebug verbosity $ "Configuration file:\n" <> (TE.decodeUtf8Lenient . Y.encode $ config)
      pure config

    -- Create the service name attribute.
    let attrServiceName = ("service.name", maybe AttrNull (AttrText . (.serviceName)) maybeServiceName)

    -- Open a connection to the OpenTelemetry Collector.
    let OpenTelemetryCollectorOptions{..} = openTelemetryCollectorOptions
    G.withConnection G.def openTelemetryCollectorServer $ \conn -> do
      runWithEventlogSource
        verbosity
        eventlogSocket
        eventlogSocketTimeout
        eventlogSocketTimeoutExponent
        batchInterval
        Nothing
        maybeEventlogLogFile
        $ fanout
          [ M.validateInput verbosity 10
          , eventCountTick ~> mapping (D.singleton . EventCountStat)
          , M.liftTick M.withStartTime
              ~> fanout
                [ processHeapEvents config verbosity maybeHeapProfBreakdown
                    ~> mapping (fmap Left)
                , processThreadEvents config verbosity
                ]
              ~> mapping (partitionEithers . D.toList)
              ~> fanout
                [ runIf (shouldExportMetrics config) $
                    mapping fst
                      ~> fanout
                        [ asScopeMetrics
                            [ OM.scope .~ eventlogLiveScope
                            ]
                            ~> asResourceMetric []
                            ~> asExportMetricServiceRequest
                            ~> exportResourceMetrics conn
                            ~> mapping (D.singleton . ExportMetricsResultStat)
                        ]
                , runIf (shouldExportSpans config) $
                    mapping snd
                      ~> fanout
                        [ asScopeSpans
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
                            ~> mapping (D.singleton . ExportTraceResultStat)
                        ]
                ]
          ]
          ~> asParts
          ~> processStats verbosity stats batchInterval 10

{- |
Internal helper.
Determine whether or not any spans should be exported.
-}
shouldExportMetrics :: Config -> Bool
shouldExportMetrics config =
  or
    [ C.processorEnabled (.metrics) (.heapAllocated) config
    , C.processorEnabled (.metrics) (.blocksSize) config
    , C.processorEnabled (.metrics) (.heapSize) config
    , C.processorEnabled (.metrics) (.heapLive) config
    , C.processorEnabled (.metrics) (.memCurrent) config
    , C.processorEnabled (.metrics) (.memNeeded) config
    , C.processorEnabled (.metrics) (.memReturned) config
    , C.processorEnabled (.metrics) (.heapProfSample) config
    , C.processorEnabled (.metrics) (.capabilityUsage) config
    ]

{- |
Internal helper.
Determine whether or not any spans should be exported.
-}
shouldExportSpans :: Config -> Bool
shouldExportSpans config =
  C.processorEnabled (.spans) (.capabilityUsage) config
    || C.processorEnabled (.spans) (.threadState) config

--------------------------------------------------------------------------------
-- processThreadEvents
--------------------------------------------------------------------------------

data OneOf a b c = A !a | B !b | C !c

processThreadEvents ::
  (MonadIO m) =>
  Config ->
  Verbosity ->
  ProcessT m (Tick (WithStartTime Event)) (DList (Either OM.Metric OT.Span))
processThreadEvents config verbosity =
  runIf (shouldProcessThreadEvents config) $
    M.sortByBatchTick (.value.evTime)
      ~> M.liftTick
        ( fanout
            [ M.validateOrder verbosity (.value.evTime)
            , runIf (shouldComputeCapabilityUsageSpan config) $
                M.processGCSpans verbosity
                  ~> mapping (D.singleton . A)
            , runIf (shouldComputeThreadStateSpan config) $
                M.processThreadStateSpans' M.tryGetTimeUnixNano (.value) M.setWithStartTime'value verbosity
                  ~> fanout
                    [ M.asMutatorSpans' (.value) M.setWithStartTime'value
                        ~> mapping (D.singleton . B)
                    , mapping (D.singleton . C)
                    ]
            ]
        )
      ~> M.liftTick
        ( asParts
            ~> mapping repackCapabilityUsageSpanOrThreadStateSpan
        )
      ~> fanout
        [ M.liftTick
            ( mapping leftToMaybe
                ~> asParts
            )
            ~> fanout
              [ runIf (C.processorEnabled (.metrics) (.capabilityUsage) config) $
                  M.liftTick M.processCapabilityUsageMetrics
                    ~> aggregate viaSum (C.processorAggregationStrategy (.metrics) (.capabilityUsage) config)
                    ~> mapping (fmap toNumberDataPoint)
                    ~> asSum
                      [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
                      , OM.isMonotonic .~ True
                      ]
                    ~> asMetricWith config (.capabilityUsage) [OM.unit .~ "ns"]
                    ~> mapping (D.singleton . Left)
              , runIf (C.processorEnabled (.spans) (.capabilityUsage) config) $
                  M.liftTick
                    ( M.dropStartTime
                        ~> asSpan config
                        ~> mapping (D.singleton . Right)
                    )
                    ~> M.batchByTick
              ]
        , runIf (C.processorEnabled (.spans) (.threadState) config) $
            M.liftTick
              ( mapping rightToMaybe
                  ~> asParts
                  ~> asSpan config
                  ~> mapping (D.singleton . Right)
              )
              ~> M.batchByTick
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

{- |
Internal helper.
Determine whether or not any thread events should be processed at all.
-}
shouldProcessThreadEvents :: Config -> Bool
shouldProcessThreadEvents config =
  C.processorEnabled (.metrics) (.capabilityUsage) config
    || C.processorEnabled (.spans) (.capabilityUsage) config
    || C.processorEnabled (.spans) (.threadState) config

{- |
Internal helper.
Determine whether or not the capability usage spans should be computed.
-}
shouldComputeCapabilityUsageSpan :: Config -> Bool
shouldComputeCapabilityUsageSpan config =
  C.processorEnabled (.spans) (.capabilityUsage) config
    || C.processorEnabled (.metrics) (.capabilityUsage) config

{- |
Internal helper.
Determine whether or not the thread state spans should be computed.
-}
shouldComputeThreadStateSpan :: Config -> Bool
shouldComputeThreadStateSpan config =
  C.processorEnabled (.spans) (.threadState) config
    || shouldComputeCapabilityUsageSpan config

--------------------------------------------------------------------------------
-- processHeapEvents
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Config ->
  Verbosity ->
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapEvents config verbosity maybeHeapProfBreakdown =
  fanout
    [ processHeapAllocated config
    , processBlocksSize config
    , processHeapSize config
    , processHeapLive config
    , processMemReturn config
    , processHeapProfSample config verbosity maybeHeapProfBreakdown
    ]

--------------------------------------------------------------------------------
-- Metric Aggregation

data Aggregators a b = Aggregators
  { nothing :: Process (Tick a) b
  , byBatch :: Process (Tick a) b
  }

{- |
Internal helper.
Aggregate items based on the provided aggregators and aggregation strategy.
-}
aggregate :: Aggregators a b -> Maybe C.AggregationStrategy -> Process (Tick a) b
aggregate Aggregators{..} = \case
  Nothing -> nothing
  Just C.AggregationStrategyByBatch -> byBatch

{- |
Internal helper.
Metric aggregators via the `Semigroup` instance for `Sum`.
-}
viaSum :: forall a. (Num a) => Aggregators (Metric a) [Metric a]
viaSum =
  Aggregators
    { nothing = M.batchByTickList
    , byBatch =
        -- TODO: emit group sample counts as separate metric
        byBatchVia (Proxy @(Metric (Sum a)))
          ~> mapping (fmap (.representative))
    }

{- |
Internal helper.
Metric aggregators via the `Semigroup` instance for `Last`.
-}
viaLast :: forall a. (GroupBy a) => Aggregators a [a]
viaLast =
  Aggregators
    { nothing = M.batchByTickList
    , byBatch =
        -- TODO: emit group sample counts as separate metric
        byBatchVia (Proxy @(Last a))
          ~> mapping (fmap (.representative))
    }

{- |
Internal helper.
This function aggregates items via a `Semigroup` instance and grouped by the `GroupBy` instance.
-}
byBatchVia ::
  forall a b.
  (Coercible a b, GroupBy b, Semigroup b) =>
  Proxy b ->
  Process (Tick a) [Group a]
byBatchVia (Proxy :: Proxy b) =
  mapping (fmap DG.singleton . coerce @(Tick a) @(Tick b))
    ~> M.aggregateByTick @(GroupedBy b)
    ~> mapping (coerce @[Group b] @[Group a] . DG.groups)

--------------------------------------------------------------------------------
-- HeapAllocated

processHeapAllocated :: Config -> Process (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapAllocated config =
  runIf (C.processorEnabled (.metrics) (.heapAllocated) config) $
    M.liftTick M.processHeapAllocatedData
      ~> aggregate viaSum (C.processorAggregationStrategy (.metrics) (.heapAllocated) config)
      ~> mapping (fmap toNumberDataPoint)
      ~> asSum
        [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
        , OM.isMonotonic .~ True
        ]
      ~> asMetricWith config (.heapAllocated) [OM.unit .~ "By"]
      ~> mapping D.singleton

--------------------------------------------------------------------------------
-- HeapSize

processHeapSize :: Config -> Process (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapSize config =
  runIf (C.processorEnabled (.metrics) (.heapSize) config) $
    M.liftTick M.processHeapSizeData
      ~> aggregate viaLast (C.processorAggregationStrategy (.metrics) (.heapSize) config)
      ~> mapping (fmap toNumberDataPoint)
      ~> asGauge
      ~> asMetricWith config (.heapSize) [OM.unit .~ "By"]
      ~> mapping D.singleton

--------------------------------------------------------------------------------
-- BlocksSize

processBlocksSize :: Config -> Process (Tick (WithStartTime Event)) (DList OM.Metric)
processBlocksSize config =
  runIf (C.processorEnabled (.metrics) (.blocksSize) config) $
    M.liftTick M.processBlocksSizeData
      ~> aggregate viaLast (C.processorAggregationStrategy (.metrics) (.blocksSize) config)
      ~> mapping (fmap toNumberDataPoint)
      ~> asGauge
      ~> asMetricWith config (.blocksSize) [OM.unit .~ "By"]
      ~> mapping D.singleton

--------------------------------------------------------------------------------
-- HeapLive

processHeapLive :: Config -> Process (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapLive config =
  runIf (C.processorEnabled (.metrics) (.heapLive) config) $
    M.liftTick M.processHeapLiveData
      ~> aggregate viaLast (C.processorAggregationStrategy (.metrics) (.heapLive) config)
      ~> mapping (fmap toNumberDataPoint)
      ~> asGauge
      ~> asMetricWith config (.heapLive) [OM.unit .~ "By"]
      ~> mapping D.singleton

--------------------------------------------------------------------------------
-- MemReturn

processMemReturn :: Config -> Process (Tick (WithStartTime Event)) (DList OM.Metric)
processMemReturn config =
  runIf (shouldComputeMemReturn config) $
    M.liftTick M.processMemReturnData
      ~> fanout
        [ runIf (C.processorEnabled (.metrics) (.memCurrent) config) $
            M.liftTick asMemCurrent
              ~> aggregate viaLast (C.processorAggregationStrategy (.metrics) (.memCurrent) config)
              ~> mapping (fmap toNumberDataPoint)
              ~> asGauge
              ~> asMetricWith config (.memCurrent) [OM.unit .~ "{mblock}"]
              ~> mapping D.singleton
        , runIf (C.processorEnabled (.metrics) (.memNeeded) config) $
            M.liftTick asMemNeeded
              ~> aggregate viaLast (C.processorAggregationStrategy (.metrics) (.memNeeded) config)
              ~> mapping (fmap toNumberDataPoint)
              ~> asGauge
              ~> asMetricWith config (.memNeeded) [OM.unit .~ "{mblock}"]
              ~> mapping D.singleton
        , runIf (C.processorEnabled (.metrics) (.memReturned) config) $
            M.liftTick asMemReturned
              ~> aggregate viaLast (C.processorAggregationStrategy (.metrics) (.memReturned) config)
              ~> mapping (fmap toNumberDataPoint)
              ~> asGauge
              ~> asMetricWith config (.memReturned) [OM.unit .~ "{mblock}"]
              ~> mapping D.singleton
        ]

{- |
Internal helper.
Determine whether the MemReturn data should be computed.
-}
shouldComputeMemReturn :: Config -> Bool
shouldComputeMemReturn config =
  C.processorEnabled (.metrics) (.memCurrent) config
    || C.processorEnabled (.metrics) (.memNeeded) config
    || C.processorEnabled (.metrics) (.memReturned) config

asMemCurrent :: Process (Metric MemReturnData) (Metric Word32)
asMemCurrent = mapping (fmap (.current))

asMemNeeded :: Process (Metric MemReturnData) (Metric Word32)
asMemNeeded = mapping (fmap (.needed))

asMemReturned :: Process (Metric MemReturnData) (Metric Word32)
asMemReturned = mapping (fmap (.returned))

--------------------------------------------------------------------------------
-- HeapProfSample

heapProfSampleDataAggregators :: Aggregators M.HeapProfSampleData [Metric Word64]
heapProfSampleDataAggregators = Aggregators{..}
 where
  nothing :: Process (Tick M.HeapProfSampleData) [Metric Word64]
  nothing =
    M.batchByTickList
      ~> mapping (concatMap M.heapProfSamples)
  byBatch :: Process (Tick M.HeapProfSampleData) [Metric Word64]
  byBatch =
    -- TODO: emit group sample counts as separate metric
    mapping (fmap (DG.singleton . Last))
      ~> M.aggregateByTick @(GroupedBy (Last M.HeapProfSampleData))
      ~> mapping (concatMap (M.heapProfSamples . getLast) . DG.elems)

processHeapProfSample ::
  (MonadIO m) =>
  Config ->
  Verbosity ->
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithStartTime Event)) (DList OM.Metric)
processHeapProfSample config verbosity maybeHeapProfBreakdown =
  runIf (C.processorEnabled (.metrics) (.heapProfSample) config) $
    M.liftTick (M.processHeapProfSampleData verbosity maybeHeapProfBreakdown)
      ~> aggregate heapProfSampleDataAggregators (C.processorAggregationStrategy (.metrics) (.heapProfSample) config)
      ~> mapping (fmap toNumberDataPoint)
      ~> asGauge
      ~> asMetricWith config (.heapProfSample) [OM.unit .~ "By"]
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

asMetricWith ::
  (Default a, HasField "description" a (Maybe Text), HasField "name" a Text) =>
  Config ->
  (C.Metrics -> Maybe a) ->
  [OM.Metric -> OM.Metric] ->
  Process OM.Metric'Data OM.Metric
asMetricWith config field mod =
  asMetric $
    [ OM.name .~ C.processorName (.metrics) field config
    , maybe id (OM.description .~) $ C.processorDescription (.metrics) field config
    ]
      <> mod

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
    -- | The configuration.
    Config ->
    -- | The input value.
    v ->
    -- | The trace ID.
    ByteString ->
    -- | The span ID.
    ByteString ->
    OT.Span

  -- | The `asSpan` machine processes values @v@ into OpenTelemetry spans `OT.Span`.
  asSpan :: (MonadIO m) => Config -> ProcessT m v OT.Span
  default asSpan :: (MonadIO m, Hashable (Key v)) => Config -> ProcessT m v OT.Span
  asSpan config = construct $ go (mempty, Nothing)
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
      yield $ toSpan config i traceId spanId
      -- Continue
      go (traceIds', Just gen2)

--------------------------------------------------------------------------------
-- Interpret capability usage spans

instance AsSpan CapabilityUsageSpan where
  type Key CapabilityUsageSpan = Int

  toKey :: CapabilityUsageSpan -> Int
  toKey = (.cap)

  toSpan :: Config -> CapabilityUsageSpan -> ByteString -> ByteString -> OT.Span
  toSpan config i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ C.processorName (.spans) (.capabilityUsage) config <> " " <> M.showCapabilityUserCategory user
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
    user = M.capabilityUser i

--------------------------------------------------------------------------------
-- Interpret thread state spans

instance AsSpan ThreadStateSpan where
  type Key ThreadStateSpan = ThreadId

  toKey :: ThreadStateSpan -> ThreadId
  toKey = (.thread)

  toSpan :: Config -> ThreadStateSpan -> ByteString -> ByteString -> OT.Span
  toSpan config i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ C.processorName (.spans) (.threadState) config <> " " <> M.showThreadStateCategory i.threadState
      , OT.kind .~ OT.Span'SPAN_KIND_INTERNAL
      , OT.startTimeUnixNano .~ i.startTimeUnixNano
      , OT.endTimeUnixNano .~ i.endTimeUnixNano
      , OT.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "capability" ~= M.threadStateCap i.threadState
            , "status" ~= (show <$> M.threadStateStatus i.threadState)
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
    , OM.attributes .~ mapMaybe toMaybeKeyValue (toList i.attrs)
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
-- Options
--------------------------------------------------------------------------------

options :: O.ParserInfo Options
options =
  O.info
    ( optionsParser
        O.<**> defaultsPrinter
        O.<**> OE.helperWith (O.long "help" <> O.help "Show this help text.")
        O.<**> OC.simpleVersioner (showVersion EventlogLive.version)
    )
    O.idm

data Options = Options
  { eventlogSocket :: EventlogSource
  , eventlogSocketTimeout :: Double
  , eventlogSocketTimeoutExponent :: Double
  , batchInterval :: Int
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , maybeServiceName :: Maybe ServiceName
  , verbosity :: Verbosity
  , stats :: Bool
  , maybeConfigFile :: Maybe FilePath
  , openTelemetryCollectorOptions :: OpenTelemetryCollectorOptions
  , myDebugOptions :: MyDebugOptions
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSourceParser
    <*> eventlogSocketTimeoutParser
    <*> eventlogSocketTimeoutExponentParser
    <*> batchIntervalParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> O.optional serviceNameParser
    <*> verbosityParser
    <*> statsParser
    <*> O.optional configFileParser
    <*> openTelemetryCollectorOptionsParser
    <*> myDebugOptionsParser

--------------------------------------------------------------------------------
-- Debug Options

data MyDebugOptions = MyDebugOptions
  { maybeMyEventlogSocket :: Maybe MyEventlogSocket
  , maybeMyGhcDebugSocket :: Maybe MyGhcDebugSocket
  }

myDebugOptionsParser :: O.Parser MyDebugOptions
myDebugOptionsParser =
  OC.parserOptionGroup "Debug Options" $
    MyDebugOptions
      <$> O.optional myEventlogSocketParser
      <*> O.optional myGhcDebugSocketParser

--------------------------------------------------------------------------------
-- My Eventlog Socket

newtype MyEventlogSocket
  = MyEventlogSocketUnix FilePath

myEventlogSocketParser :: O.Parser MyEventlogSocket
myEventlogSocketParser =
  MyEventlogSocketUnix
    <$> O.strOption
      ( O.long "enable-my-eventlog-socket-unix"
          <> O.metavar "SOCKET"
          <> O.help "Enable the eventlog socket for this program on the given Unix socket."
      )

{- |
Set @eventlog-socket@ as the eventlog writer.
-}
withMyEventlogSocket :: Maybe MyEventlogSocket -> IO ()
withMyEventlogSocket maybeMyEventlogSocket =
  for_ maybeMyEventlogSocket $ \(MyEventlogSocketUnix myEventlogSocket) ->
    Eventlog.Socket.startWait myEventlogSocket

--------------------------------------------------------------------------------
-- My GHC Debug

myGhcDebugSocketParser :: O.Parser MyGhcDebugSocket
myGhcDebugSocketParser =
  myGhcDebugSocketDefaultParser
    <|> myGhcDebugSocketUnixParser
    <|> myGhcDebugSocketTcpParser
 where
  myGhcDebugSocketDefaultParser =
    O.flag'
      MyGhcDebugSocketDefault
      ( O.long "enable-my-ghc-debug-socket"
          <> O.help "Enable ghc-debug for this program."
      )
  myGhcDebugSocketUnixParser =
    MyGhcDebugSocketUnix
      <$> O.strOption
        ( O.long "enable-my-ghc-debug-socket-unix"
            <> O.metavar "SOCKET"
            <> O.help "Enable ghc-debug for this program on the given Unix socket."
        )
  myGhcDebugSocketTcpParser =
    MyGhcDebugSocketUnix
      <$> O.strOption
        ( O.long "enable-my-ghc-debug-socket-tcp"
            <> O.metavar "ADDRESS"
            <> O.help "Enable ghc-debug for this program on the given TCP socket specified as 'host:port'."
        )

--------------------------------------------------------------------------------
-- Configuration

configFileParser :: O.Parser FilePath
configFileParser =
  O.strOption
    ( O.long "config"
        <> O.metavar "FILE"
        <> O.help "The path to a detailed configuration file."
    )

defaultsPrinter :: O.Parser (a -> a)
defaultsPrinter =
  O.infoOption defaultConfigString . mconcat $
    [ O.long "print-defaults"
    , O.help "Print default configuration options that can be used in config.yaml"
    ]

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

newtype OpenTelemetryCollectorOptions = OpenTelemetryCollectorOptions
  { openTelemetryCollectorServer :: G.Server
  }

openTelemetryCollectorOptionsParser :: O.Parser OpenTelemetryCollectorOptions
openTelemetryCollectorOptionsParser =
  OC.parserOptionGroup "OpenTelemetry Collector Server Options" $
    OpenTelemetryCollectorOptions
      <$> otelcolServerParser

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

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

runIf :: (Monad m) => Bool -> MachineT m k o -> MachineT m k o
runIf b m = if b then m else stopped
