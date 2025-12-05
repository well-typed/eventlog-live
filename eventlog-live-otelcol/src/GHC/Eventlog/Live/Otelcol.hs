{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing -fconstraint-solver-iterations=0 #-}

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
import Control.Arrow (first)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Machine (MachineT, Process, ProcessT, asParts, await, construct, echo, mapping, repeatedly, stopped, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Last (..), Sum (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Data.Word (Word32, Word64)
import GHC.Debug.Stub.Compat (MyGhcDebugSocket (..), maybeMyGhcDebugSocketParser, withMyGhcDebug)
import GHC.Eventlog.Live.Data.Attribute
import GHC.Eventlog.Live.Data.Group (Group, GroupBy, GroupedBy)
import GHC.Eventlog.Live.Data.Group qualified as DG
import GHC.Eventlog.Live.Data.LogRecord (LogRecord (..))
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Data.Severity (Severity)
import GHC.Eventlog.Live.Data.Severity qualified as DS (SeverityNumber (..), toSeverityNumber)
import GHC.Eventlog.Live.Logger (logDebug)
import GHC.Eventlog.Live.Machine.Analysis.Capability (CapabilityUsageSpan)
import GHC.Eventlog.Live.Machine.Analysis.Capability qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Heap (MemReturnData (..))
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Log qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Profile qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Thread (ThreadStateSpan (..))
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Otelcol.Config (Config, FullConfig (..))
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Default.Raw (defaultConfigJSONSchemaString, defaultConfigString)
import GHC.Eventlog.Live.Otelcol.Exporter.Logs (exportResourceLogs)
import GHC.Eventlog.Live.Otelcol.Exporter.Metrics (exportResourceMetrics)
import GHC.Eventlog.Live.Otelcol.Exporter.Profiles (exportResourceProfiles)
import GHC.Eventlog.Live.Otelcol.Exporter.Traces (exportResourceSpans)
import GHC.Eventlog.Live.Otelcol.Processor.Profiles (processCallStackData)
import GHC.Eventlog.Live.Otelcol.Stats (Stat (..), eventCountTick, processStats)
import GHC.Eventlog.Live.Socket (runWithEventlogSource)
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.Eventlog.Socket qualified as Eventlog.Socket
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..), ThreadId)
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import Lens.Family2 ((&), (.~))
import Network.GRPC.Client qualified as G
import Network.GRPC.Common qualified as G
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra qualified as OE
import Paths_eventlog_live_otelcol qualified as EventlogLive
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as OLS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService qualified as OPS
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService_Fields qualified as OPS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as OTS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as OL
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
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
    let readConfigFile configFile = do
          logDebug verbosity $ "Reading configuration file from " <> T.pack configFile
          config <- C.readConfigFile verbosity configFile
          logDebug verbosity $ "Configuration file:\n" <> C.prettyConfig config
          pure config

    -- Read the configuration file and add derived settings.
    fullConfig <-
      C.toFullConfig eventlogFlushIntervalS
        <$> maybe (pure def) readConfigFile maybeConfigFile
    logDebug verbosity $ "Batch interval is " <> T.pack (show fullConfig.batchIntervalMs) <> "ms"
    logDebug verbosity $ "Eventlog flush interval is " <> T.pack (show fullConfig.eventlogFlushIntervalX) <> "x"

    -- Determine the window size for statistics
    let windowSizeX =
          (10 *) . maximum $
            [ fullConfig.eventlogFlushIntervalX
            , C.maximumAggregationBatches fullConfig
            , C.maximumExportBatches fullConfig
            ]

    -- Create the service name attribute.
    let attrServiceName = ("service.name", maybe AttrNull (AttrText . (.serviceName)) maybeServiceName)

    -- Open a connection to the OpenTelemetry Collector.
    let OpenTelemetryCollectorOptions{..} = openTelemetryCollectorOptions
    G.withConnection G.def openTelemetryCollectorServer $ \conn -> do
      runWithEventlogSource
        verbosity
        eventlogSocket
        eventlogSocketTimeoutS
        eventlogSocketTimeoutExponent
        fullConfig.batchIntervalMs
        Nothing
        maybeEventlogLogFile
        $ M.fanoutTick
          [ -- Log a warning if no input has been received after 10 ticks.
            M.validateInput verbosity 10
          , -- Count the number of input events between each tick.
            eventCountTick
              ~> mapping (fmap (D.singleton . EventCountStat))
          , -- Process the input events.
            M.liftTick M.withStartTime
              ~> M.fanoutTick
                [ -- Process the heap events.
                  processHeapEvents verbosity maybeHeapProfBreakdown fullConfig
                    ~> mapping (fmap (fmap TelemetryData'Metric))
                , -- Process the log events.
                  processLogEvents fullConfig
                    ~> mapping (fmap (fmap TelemetryData'Log))
                , -- Process the thread events.
                  processThreadEvents verbosity fullConfig
                    ~> mapping (fmap (fmap (either TelemetryData'Metric TelemetryData'Span)))
                , -- Process the profile events.
                  processProfileEvents verbosity fullConfig
                    ~> mapping (fmap (fmap TelemetryData'Profile))
                ]
              ~> mapping (fmap (partitionTelemetryData . D.toList))
              ~> M.fanoutTick
                [ -- Export logs.
                  runIf (C.shouldExportLogs fullConfig) $
                    M.liftTick
                      ( mapping (\(logs, _metrics, _spans, _profiles) -> logs)
                          ~> asScopeLogs
                            [OL.scope .~ eventlogLiveScope]
                          ~> asResourceLog
                            [OL.resource .~ messageWith [OM.attributes .~ mapMaybe toMaybeKeyValue [attrServiceName]]]
                          ~> asExportLogServiceRequest
                      )
                      ~> exportResourceLogs conn
                      ~> mapping (fmap (D.singleton . ExportLogsResultStat))
                , -- Export metrics.
                  runIf (C.shouldExportMetrics fullConfig) $
                    M.liftTick
                      ( mapping (\(_logs, metrics, _spans, _profiles) -> metrics)
                          ~> asScopeMetrics
                            [OM.scope .~ eventlogLiveScope]
                          ~> asResourceMetric
                            [OM.resource .~ messageWith [OM.attributes .~ mapMaybe toMaybeKeyValue [attrServiceName]]]
                          ~> asExportMetricServiceRequest
                      )
                      ~> exportResourceMetrics conn
                      ~> mapping (fmap (D.singleton . ExportMetricsResultStat))
                , -- Export traces.
                  runIf (C.shouldExportTraces fullConfig) $
                    M.liftTick
                      ( mapping (\(_logs, _metrics, spans, _profiles) -> spans)
                          ~> asScopeSpans
                            [OM.scope .~ eventlogLiveScope]
                          ~> asResourceSpan
                            [OT.resource .~ messageWith [OT.attributes .~ mapMaybe toMaybeKeyValue [attrServiceName]]]
                          ~> asExportTraceServiceRequest
                      )
                      ~> exportResourceSpans conn
                      ~> mapping (fmap (D.singleton . ExportTraceResultStat))
                , -- Export profiles.
                  runIf (C.shouldExportProfiles fullConfig) $
                    M.liftTick
                      ( mapping (\(_logs, _metrics, _spans, profiles) -> profiles)
                          ~> mapping (processCallStackData (toMaybeKeyValue attrServiceName) eventlogLiveScope)
                          ~> asExportProfileServiceRequest
                      )
                      ~> exportResourceProfiles conn
                      ~> mapping (fmap (D.singleton . ExportProfileResultStat))
                ]
          ]
          -- Process the statistics
          -- TODO: windowSize should be the maximum of all aggregation and export intervals
          ~> M.liftTick (asParts ~> processStats verbosity stats eventlogFlushIntervalS windowSizeX)
          -- Validate the consistency of the tick
          ~> M.validateTicks verbosity
          ~> M.dropTick

data TelemetryData
  = TelemetryData'Log OL.LogRecord
  | TelemetryData'Metric OM.Metric
  | TelemetryData'Span OT.Span
  | TelemetryData'Profile M.CallStackData

partitionTelemetryData :: [TelemetryData] -> ([OL.LogRecord], [OM.Metric], [OT.Span], [M.CallStackData])
partitionTelemetryData = go ([], [], [], [])
 where
  go :: ([OL.LogRecord], [OM.Metric], [OT.Span], [M.CallStackData]) -> [TelemetryData] -> ([OL.LogRecord], [OM.Metric], [OT.Span], [M.CallStackData])
  go (logs, metrics, spans, profiles) = \case
    [] -> (reverse logs, reverse metrics, reverse spans, reverse profiles)
    (TelemetryData'Log log : rest) -> go (log : logs, metrics, spans, profiles) rest
    (TelemetryData'Metric metric : rest) -> go (logs, metric : metrics, spans, profiles) rest
    (TelemetryData'Span span : rest) -> go (logs, metrics, span : spans, profiles) rest
    (TelemetryData'Profile profile : rest) -> go (logs, metrics, spans, profile : profiles) rest

--------------------------------------------------------------------------------
-- processThreadEvents
--------------------------------------------------------------------------------

data OneOf a b c = A !a | B !b | C !c

processThreadEvents ::
  (MonadIO m) =>
  Verbosity ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList (Either OM.Metric OT.Span)))
processThreadEvents verbosity fullConfig =
  runIf (shouldProcessThreadEvents fullConfig) $
    M.sortByTicks (.value.evTime) fullConfig.eventlogFlushIntervalX
      ~> M.liftTick
        ( fanout
            [ M.validateOrder verbosity (.value.evTime)
            , runIf (shouldComputeCapabilityUsageSpan fullConfig) $
                M.processGCSpans verbosity
                  ~> mapping (D.singleton . A)
            , runIf (shouldComputeThreadStateSpan fullConfig) $
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
            ~> M.fanoutTick
              [ runMetricProcessor
                  MetricProcessor
                    { metricProcessorProxy = Proxy @"capabilityUsage"
                    , dataProcessor = M.processCapabilityUsageMetrics
                    , aggregators = viaSum
                    , postProcessor = echo
                    , unit = "ns"
                    , asMetric'Data =
                        asSum
                          [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
                          , OM.isMonotonic .~ True
                          ]
                    }
                  fullConfig
                  ~> mapping (fmap (fmap Left))
              , runIf (C.processorEnabled (.traces) (.capabilityUsage) fullConfig) $
                  M.liftTick
                    ( M.dropStartTime
                        ~> asSpan fullConfig
                        ~> mapping (D.singleton . Right)
                    )
                    ~> M.batchByTick
              ]
        , runIf (C.processorEnabled (.traces) (.threadState) fullConfig) $
            M.liftTick
              ( mapping rightToMaybe
                  ~> asParts
                  ~> asSpan fullConfig
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
shouldProcessThreadEvents :: FullConfig -> Bool
shouldProcessThreadEvents fullConfig =
  C.processorEnabled (.metrics) (.capabilityUsage) fullConfig
    || C.processorEnabled (.traces) (.capabilityUsage) fullConfig
    || C.processorEnabled (.traces) (.threadState) fullConfig

{- |
Internal helper.
Determine whether or not the capability usage spans should be computed.
-}
shouldComputeCapabilityUsageSpan :: FullConfig -> Bool
shouldComputeCapabilityUsageSpan fullConfig =
  C.processorEnabled (.traces) (.capabilityUsage) fullConfig
    || C.processorEnabled (.metrics) (.capabilityUsage) fullConfig

{- |
Internal helper.
Determine whether or not the thread state spans should be computed.
-}
shouldComputeThreadStateSpan :: FullConfig -> Bool
shouldComputeThreadStateSpan fullConfig =
  C.processorEnabled (.traces) (.threadState) fullConfig
    || shouldComputeCapabilityUsageSpan fullConfig

--------------------------------------------------------------------------------
-- processLogEvents
--------------------------------------------------------------------------------

processLogEvents ::
  (MonadIO m) =>
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processLogEvents fullConfig =
  M.fanoutTick
    [ processThreadLabel fullConfig
    , processUserMarker fullConfig
    , processUserMessage fullConfig
    ]

--------------------------------------------------------------------------------
-- UserMessage

processUserMessage :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processUserMessage fullConfig =
  runIf (C.processorEnabled (.logs) (.userMessage) fullConfig) $
    M.liftTick M.processUserMessageData
      ~> M.liftTick (mapping (D.singleton . toLogRecord))
      ~> M.batchByTicks (C.processorExportBatches (.logs) (.userMessage) fullConfig)

--------------------------------------------------------------------------------
-- UserMarker

processUserMarker :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processUserMarker fullConfig =
  runIf (C.processorEnabled (.logs) (.userMarker) fullConfig) $
    M.liftTick M.processUserMarkerData
      ~> M.liftTick (mapping (D.singleton . toLogRecord))
      ~> M.batchByTicks (C.processorExportBatches (.logs) (.userMarker) fullConfig)

--------------------------------------------------------------------------------
-- ThreadLabel

processThreadLabel :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processThreadLabel fullConfig =
  runIf (C.processorEnabled (.logs) (.threadLabel) fullConfig) $
    M.liftTick M.processThreadLabelData
      ~> M.liftTick (mapping (D.singleton . toLogRecord))
      ~> M.batchByTicks (C.processorExportBatches (.logs) (.threadLabel) fullConfig)

--------------------------------------------------------------------------------
-- processHeapEvents
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Verbosity ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapEvents verbosity maybeHeapProfBreakdown fullConfig =
  M.fanoutTick
    [ processHeapAllocated fullConfig
    , processBlocksSize fullConfig
    , processHeapSize fullConfig
    , processHeapLive fullConfig
    , processMemReturn fullConfig
    , processHeapProfSample verbosity maybeHeapProfBreakdown fullConfig
    ]

--------------------------------------------------------------------------------
-- HeapAllocated

processHeapAllocated :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapAllocated =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapAllocated"
      , dataProcessor = M.processHeapAllocatedData
      , aggregators = viaSum
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data =
          asSum
            [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
            , OM.isMonotonic .~ True
            ]
      }

--------------------------------------------------------------------------------
-- HeapSize

processHeapSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapSize =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapSize"
      , dataProcessor = M.processHeapSizeData
      , aggregators = viaLast
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data = asGauge
      }

--------------------------------------------------------------------------------
-- BlocksSize

processBlocksSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processBlocksSize =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"blocksSize"
      , dataProcessor = M.processBlocksSizeData
      , aggregators = viaLast
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data = asGauge
      }

--------------------------------------------------------------------------------
-- HeapLive

processHeapLive :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapLive =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapLive"
      , dataProcessor = M.processHeapLiveData
      , aggregators = viaLast
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data = asGauge
      }

--------------------------------------------------------------------------------
-- MemReturn

processMemReturn :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processMemReturn fullConfig =
  runIf (shouldComputeMemReturn fullConfig) $
    M.liftTick M.processMemReturnData
      ~> M.fanoutTick
        [ runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"memCurrent"
              , dataProcessor = mapping (fmap (.current))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "{mblock}"
              , asMetric'Data = asGauge
              }
            fullConfig
        , runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"memNeeded"
              , dataProcessor = mapping (fmap (.needed))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "{mblock}"
              , asMetric'Data = asGauge
              }
            fullConfig
        , runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"memReturned"
              , dataProcessor = mapping (fmap (.returned))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "{mblock}"
              , asMetric'Data = asGauge
              }
            fullConfig
        ]

{- |
Internal helper.
Determine whether the MemReturn data should be computed.
-}
shouldComputeMemReturn :: FullConfig -> Bool
shouldComputeMemReturn fullConfig =
  C.processorEnabled (.metrics) (.memCurrent) fullConfig
    || C.processorEnabled (.metrics) (.memNeeded) fullConfig
    || C.processorEnabled (.metrics) (.memReturned) fullConfig

--------------------------------------------------------------------------------
-- HeapProfSample

processHeapProfSample ::
  (MonadIO m) =>
  Verbosity ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapProfSample verbosity maybeHeapProfBreakdown =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapProfSample"
      , dataProcessor = M.processHeapProfSampleData verbosity maybeHeapProfBreakdown
      , aggregators = viaLast
      , postProcessor = mapping M.heapProfSamples ~> asParts
      , unit = "By"
      , asMetric'Data = asGauge
      }

--------------------------------------------------------------------------------
-- Generic Metric Processor

type MetricProcessor :: Symbol -> Type -> (Type -> Type) -> Type -> Type -> Type -> Type -> Type
data MetricProcessor metricProcessor metricProcessorConfig m a b c d
  = ( Monad m
    , HasField metricProcessor C.Metrics (Maybe metricProcessorConfig)
    , C.IsMetricProcessorConfig metricProcessorConfig
    , IsNumberDataPoint'Value d
    ) =>
  MetricProcessor
  { metricProcessorProxy :: !(Proxy metricProcessor)
  -- ^ The metric's field name in `C.Metrics`.
  , dataProcessor :: !(ProcessT m a b)
  -- ^ The metric's data processor
  , aggregators :: !(Aggregators b c)
  -- ^ The metric's aggregator
  , postProcessor :: !(Process c (Metric d))
  -- ^ The metric's post processor
  , unit :: !Text
  -- ^ The metric's unit (in UCUM format).
  , asMetric'Data :: !(Process [OM.NumberDataPoint] OM.Metric'Data)
  -- ^ A process to wrap data points as `OM.Metric'Data`.
  }

{- |
Internal helper.
Run a `MetricProcessor`.
-}
runMetricProcessor ::
  forall metricProcessor metricProcessorConfig m a b c d.
  (Default metricProcessorConfig) =>
  MetricProcessor metricProcessor metricProcessorConfig m a b c d ->
  -- | The full configuration.
  FullConfig ->
  ProcessT m (Tick a) (Tick (DList OM.Metric))
runMetricProcessor MetricProcessor{..} fullConfig =
  let metricProcessorConfig :: C.Metrics -> Maybe metricProcessorConfig
      metricProcessorConfig = getField @metricProcessor
   in runIf (C.processorEnabled (.metrics) metricProcessorConfig fullConfig) $
        M.liftTick dataProcessor
          ~> aggregate aggregators (C.processorAggregationBatches (.metrics) metricProcessorConfig fullConfig)
          ~> M.liftTick postProcessor
          ~> mapping (fmap (D.singleton . toNumberDataPoint))
          ~> M.batchByTicks (C.processorExportBatches (.metrics) metricProcessorConfig fullConfig)
          ~> M.liftTick
            ( mapping D.toList
                ~> asMetric'Data
                ~> asMetricWith fullConfig metricProcessorConfig [OM.unit .~ unit]
                ~> mapping D.singleton
            )
{-# INLINE runMetricProcessor #-}

--------------------------------------------------------------------------------
-- processProfileEvents
--------------------------------------------------------------------------------

processProfileEvents ::
  (MonadIO m) =>
  Verbosity ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processProfileEvents verbosity config =
  M.fanoutTick
    [ processStackProfSample verbosity config
    , processCosterCentreProfSample verbosity config
    ]

--------------------------------------------------------------------------------
-- StackProfSample

processStackProfSample ::
  (MonadIO m) =>
  Verbosity ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processStackProfSample verbosity config = do
  let
    postProcessor = mapping M.stackProfSamples ~> asParts
    dataProcessor = M.processStackProfSampleData verbosity
  -- aggregators = viaLast
  runIf (C.processorEnabled (.profiles) (.stackSample) config) $
    M.liftTick dataProcessor
      -- ~> aggregate aggregators (C.processorAggregationBatches (.profiles) (.stackSample) config)
      ~> M.liftTick postProcessor
      -- TODO: do something with the Metric value, right now it is completely unused
      ~> M.liftTick (mapping (D.singleton . (.value)))
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.stackSample) config)

processCosterCentreProfSample ::
  (MonadIO m) =>
  Verbosity ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processCosterCentreProfSample verbosity config = do
  let
    postProcessor = mapping M.stackProfSamples ~> asParts
    dataProcessor = M.processCosterCentreProfSampleData verbosity
  -- aggregators = viaLast
  runIf (C.processorEnabled (.profiles) (.costCentreSample) config) $
    M.liftTick dataProcessor
      ~> M.liftTick postProcessor
      ~> M.liftTick (mapping (D.singleton . (.value)))
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.costCentreSample) config)

--------------------------------------------------------------------------------
-- Aggregation
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Metric Aggregation

data Aggregators a b = Aggregators
  { nothing :: Process (Tick a) (Tick b)
  , byBatches :: Int -> Process (Tick a) (Tick b)
  }

{- |
Internal helper.
Aggregate items based on the provided aggregators and aggregation strategy.
-}
aggregate :: Aggregators a b -> Int -> Process (Tick a) (Tick b)
aggregate Aggregators{..} aggregationBatches
  | aggregationBatches >= 1 = byBatches aggregationBatches
  | otherwise = nothing

{- |
Internal helper.
Metric aggregators via the `Semigroup` instance for `Sum`.
-}
viaSum :: forall a. (Num a) => Aggregators (Metric a) (Metric a)
viaSum =
  Aggregators
    { nothing = echo
    , byBatches = \ticks ->
        -- TODO: Yield group sample counts as separate metric.
        batchByTicksVia ticks (Proxy @(Metric (Sum a)))
          ~> M.liftTick (mapping (fmap (.representative)) ~> asParts)
    }

{- |
Internal helper.
Metric aggregators via the `Semigroup` instance for `Last`.
-}
viaLast :: forall a. (GroupBy a) => Aggregators a a
viaLast =
  Aggregators
    { nothing = echo
    , byBatches = \ticks ->
        -- TODO: Yield group sample counts as separate metric.
        batchByTicksVia ticks (Proxy @(Last a))
          ~> M.liftTick (mapping (fmap (.representative)) ~> asParts)
    }

{- |
Internal helper.
This function aggregates items via a `Semigroup` instance and grouped by the `GroupBy` instance.
-}
batchByTicksVia ::
  forall a b.
  (Coercible a b, GroupBy b, Semigroup b) =>
  -- | The number of ticks per batch.
  Int ->
  Proxy b ->
  Process (Tick a) (Tick [Group a])
batchByTicksVia ticks (Proxy :: Proxy b) =
  mapping (fmap DG.singleton . coerce @(Tick a) @(Tick b))
    ~> M.batchByTicks @(GroupedBy b) ticks
    ~> M.liftTick (mapping (coerce @[Group b] @[Group a] . DG.groups))

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

asExportLogsServiceRequest :: Process [OL.ResourceLogs] OLS.ExportLogsServiceRequest
asExportLogsServiceRequest = mapping $ (defMessage &) . (OL.resourceLogs .~)

asExportMetricsServiceRequest :: Process [OM.ResourceMetrics] OMS.ExportMetricsServiceRequest
asExportMetricsServiceRequest = mapping $ (defMessage &) . (OM.resourceMetrics .~)

asExportTracesServiceRequest :: Process [OT.ResourceSpans] OTS.ExportTraceServiceRequest
asExportTracesServiceRequest = mapping $ (defMessage &) . (OTS.resourceSpans .~)

asExportLogServiceRequest :: Process OL.ResourceLogs OLS.ExportLogsServiceRequest
asExportLogServiceRequest = mapping (: []) ~> asExportLogsServiceRequest

asExportProfilesServiceRequest :: Process ([OP.ResourceProfiles], OP.ProfilesDictionary) OPS.ExportProfilesServiceRequest
asExportProfilesServiceRequest = mapping $ \(resourceProfiles, profileDict) ->
  messageWith
    [ OPS.resourceProfiles .~ resourceProfiles
    , OPS.dictionary .~ profileDict
    ]

asExportMetricServiceRequest :: Process OM.ResourceMetrics OMS.ExportMetricsServiceRequest
asExportMetricServiceRequest = mapping (: []) ~> asExportMetricsServiceRequest

asExportTraceServiceRequest :: Process OT.ResourceSpans OTS.ExportTraceServiceRequest
asExportTraceServiceRequest = mapping (: []) ~> asExportTracesServiceRequest

asResourceLogs :: [OL.ResourceLogs -> OL.ResourceLogs] -> Process [OL.ScopeLogs] OL.ResourceLogs
asResourceLogs mod = mapping $ \scopeLogs ->
  messageWith ((OL.scopeLogs .~ scopeLogs) : mod)

asExportProfileServiceRequest :: Process (OP.ResourceProfiles, OP.ProfilesDictionary) OPS.ExportProfilesServiceRequest
asExportProfileServiceRequest = mapping (first (: [])) ~> asExportProfilesServiceRequest

asResourceMetrics :: [OM.ResourceMetrics -> OM.ResourceMetrics] -> Process [OM.ScopeMetrics] OM.ResourceMetrics
asResourceMetrics mod = mapping $ \scopeMetrics ->
  messageWith ((OM.scopeMetrics .~ scopeMetrics) : mod)

asResourceSpans :: [OT.ResourceSpans -> OT.ResourceSpans] -> Process [OT.ScopeSpans] OT.ResourceSpans
asResourceSpans mod = mapping $ \scopeSpans ->
  messageWith ((OT.scopeSpans .~ scopeSpans) : mod)

asResourceLog :: [OL.ResourceLogs -> OL.ResourceLogs] -> Process OL.ScopeLogs OL.ResourceLogs
asResourceLog mod = mapping (: []) ~> asResourceLogs mod

asResourceMetric :: [OM.ResourceMetrics -> OM.ResourceMetrics] -> Process OM.ScopeMetrics OM.ResourceMetrics
asResourceMetric mod = mapping (: []) ~> asResourceMetrics mod

asResourceSpan :: [OT.ResourceSpans -> OT.ResourceSpans] -> Process OT.ScopeSpans OT.ResourceSpans
asResourceSpan mod = mapping (: []) ~> asResourceSpans mod

asScopeLogs :: [OL.ScopeLogs -> OL.ScopeLogs] -> Process [OL.LogRecord] OL.ScopeLogs
asScopeLogs mod = mapping $ \logRecords ->
  messageWith ((OL.logRecords .~ logRecords) : mod)

asScopeMetrics :: [OM.ScopeMetrics -> OM.ScopeMetrics] -> Process [OM.Metric] OM.ScopeMetrics
asScopeMetrics mod = mapping $ \metrics ->
  messageWith ((OM.metrics .~ metrics) : mod)

asScopeSpans :: [OT.ScopeSpans -> OT.ScopeSpans] -> Process [OT.Span] OT.ScopeSpans
asScopeSpans mod = mapping $ \spans ->
  messageWith ((OT.spans .~ spans) : mod)

asMetricWith ::
  (Default a, HasField "description" a (Maybe Text), HasField "name" a (Maybe Text)) =>
  FullConfig ->
  (C.Metrics -> Maybe a) ->
  [OM.Metric -> OM.Metric] ->
  Process OM.Metric'Data OM.Metric
asMetricWith fullConfig field mod =
  asMetric $
    [ OM.name .~ C.processorName (.metrics) field fullConfig
    , maybe id (OM.description .~) $ C.processorDescription (.metrics) field fullConfig
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
-- Interpret logs

class ToLogRecord v where
  toLogRecord :: v -> OL.LogRecord

instance ToLogRecord LogRecord where
  toLogRecord :: LogRecord -> OL.LogRecord
  toLogRecord i =
    messageWith
      [ OL.body .~ messageWith [OC.stringValue .~ i.body]
      , OL.timeUnixNano .~ fromMaybe 0 i.maybeTimeUnixNano
      , -- TODO: this could be set to the actual observed time in the processor.
        OL.observedTimeUnixNano .~ fromMaybe 0 i.maybeTimeUnixNano
      , OL.attributes .~ mapMaybe toMaybeKeyValue (toList i.attrs)
      , OL.severityNumber .~ toSeverityNumber i.maybeSeverity
      ]
   where
    toSeverityNumber :: Maybe Severity -> OL.SeverityNumber
    toSeverityNumber = maybe OL.SEVERITY_NUMBER_UNSPECIFIED (toEnum . (.value) . DS.toSeverityNumber)

instance ToLogRecord M.ThreadLabel where
  toLogRecord :: M.ThreadLabel -> OL.LogRecord
  toLogRecord i =
    messageWith
      [ OL.body .~ messageWith [OC.stringValue .~ i.threadlabel]
      , OL.timeUnixNano .~ i.startTimeUnixNano
      , -- TODO: this could be set to the actual observed time in the processor.
        OL.observedTimeUnixNano .~ i.startTimeUnixNano
      , OL.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "kind" ~= ("ThreadLabel" :: Text)
            , "thread" ~= i.thread
            ]
      ]

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
    FullConfig ->
    -- | The input value.
    v ->
    -- | The trace ID.
    ByteString ->
    -- | The span ID.
    ByteString ->
    OT.Span

  -- | The `asSpan` machine processes values @v@ into OpenTelemetry spans `OT.Span`.
  asSpan :: (MonadIO m) => FullConfig -> ProcessT m v OT.Span
  default asSpan :: (MonadIO m, Hashable (Key v)) => FullConfig -> ProcessT m v OT.Span
  asSpan fullConfig = construct $ go (mempty, Nothing)
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
      yield $ toSpan fullConfig i traceId spanId
      -- Continue
      go (traceIds', Just gen2)

--------------------------------------------------------------------------------
-- Interpret capability usage spans

instance AsSpan CapabilityUsageSpan where
  type Key CapabilityUsageSpan = Int

  toKey :: CapabilityUsageSpan -> Int
  toKey = (.cap)

  toSpan :: FullConfig -> CapabilityUsageSpan -> ByteString -> ByteString -> OT.Span
  toSpan fullConfig i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ C.processorName (.traces) (.capabilityUsage) fullConfig <> " " <> M.showCapabilityUserCategory user
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

  toSpan :: FullConfig -> ThreadStateSpan -> ByteString -> ByteString -> OT.Span
  toSpan fullConfig i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ C.processorName (.traces) (.threadState) fullConfig <> " " <> M.showThreadStateCategory i.threadState
      , OT.kind .~ OT.Span'SPAN_KIND_INTERNAL
      , OT.startTimeUnixNano .~ i.startTimeUnixNano
      , OT.endTimeUnixNano .~ i.endTimeUnixNano
      , OT.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "capability" ~= M.threadStateCap i.threadState
            , "thread" ~= show i.thread
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
  AttrBool v -> Just $ messageWith [OC.boolValue .~ v]
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
        O.<**> debugDefaultsPrinter
        O.<**> configJSONSchemaPrinter
        O.<**> OE.helperWith (O.long "help" <> O.help "Show this help text.")
        O.<**> OC.simpleVersioner (showVersion EventlogLive.version)
    )
    O.idm

data Options = Options
  { eventlogSocket :: EventlogSource
  , eventlogSocketTimeoutS :: Double
  , eventlogSocketTimeoutExponent :: Double
  , eventlogFlushIntervalS :: Double
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
    <*> eventlogSocketTimeoutSParser
    <*> eventlogSocketTimeoutExponentParser
    <*> eventlogFlushIntervalSParser
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
      <*> maybeMyGhcDebugSocketParser

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
    , O.help "Print default configuration options."
    ]

configJSONSchemaPrinter :: O.Parser (a -> a)
configJSONSchemaPrinter =
  O.infoOption defaultConfigJSONSchemaString . mconcat $
    [ O.long "print-config-json-schema"
    , O.help "Print JSON Schema for configuration format."
    ]

debugDefaultsPrinter :: O.Parser (a -> a)
debugDefaultsPrinter =
  O.infoOption defaultConfigDebugString . mconcat $
    [ O.long "print-defaults-debug"
    , O.help "Print default configuration options using the parsed representation."
    , O.internal
    ]
 where
  defaultConfigDebugString =
    T.unpack . C.prettyConfig $ (def :: Config)

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
