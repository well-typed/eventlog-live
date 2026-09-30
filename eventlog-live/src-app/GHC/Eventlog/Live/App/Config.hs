{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GHEventlog.Live.Otlp.Config
Description : The implementation of @eventlog-live-otlp@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.App.Config (
  -- * Configuration type
  Config (..),
  readConfigFile,
  prettyConfig,
  FullConfig (..),
  toFullConfig,

  -- ** Processor configuration types
  Processors (..),
  IsProcessorConfig,
  processorEnabled,
  processorDescription,
  processorName,

  -- *** Log processor configuration types
  Logs (..),
  IsLogProcessorConfig,
  KnownLog (..),
  logConfig,
  shouldExportLogs,
  ThreadLabelLog (..),
  UserMarkerLog (..),
  UserMessageLog (..),
  InternalLogMessageLog (..),

  -- *** Metric processor configuration types
  Metrics (..),
  IsMetricProcessorConfig,
  KnownMetric (..),
  metricConfig,
  shouldExportMetrics,
  HeapAllocatedMetric (..),
  BlocksSizeMetric (..),
  HeapSizeMetric (..),
  HeapLiveMetric (..),
  MemCurrentMetric (..),
  MemNeededMetric (..),
  MemReturnedMetric (..),
  GcCopiedMetric (..),
  GcSlopMetric (..),
  GcFragmentationMetric (..),
  HeapProfSampleMetric (..),
  CapabilityUsageMetric (..),
  ProductivityMetric (..),

  -- *** Trace processor configuration types
  Traces (..),
  IsTraceProcessorConfig,
  KnownTrace (..),
  traceConfig,
  shouldExportTraces,
  CapabilityUsageSpan (..),
  ThreadStateSpan (..),

  -- *** Profiler processor configuration types
  Profiles (..),
  KnownProfile (..),
  profileConfig,
  IsProfileProcessorConfig,
  shouldExportProfiles,
  CallStackProfile (..),
  CostCentreStackProfile (..),

  -- ** Property types

  -- *** Aggregation strategy
  AggregationStrategy (..),
  toAggregationBatches,
  processorAggregationStrategy,
  processorAggregationBatches,
  maximumAggregationBatches,

  -- *** Export strategy
  ExportStrategy (..),
  toExportBatches,
  processorExportStrategy,
  processorExportBatches,
  maximumExportBatches,

  -- *** Batch interval
  toBatchIntervalMs,
  toBatches,
) where

import Control.Exception (assert)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy qualified as BSL
import Data.Default (Default (..))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid (Any (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Data.YAML qualified as YAML
import GHC.Eventlog.Live.App.Config.Default (defaultConfig, getDefault)
import GHC.Eventlog.Live.App.Config.Types
import GHC.Eventlog.Live.Data.Metric (AggregationTemporality (..), KnownMetricKind, KnownMetricType, KnownMetricUnit, MetricKind (..), MetricUnit (..), Monotonicity (..))
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.Stack.Types (HasCallStack)
import GHC.TypeLits (KnownSymbol, Symbol)
import System.Exit (exitFailure)

{- |
Read a `Config` from a configuration file.
-}
readConfigFile ::
  Logger IO ->
  FilePath ->
  IO Config
readConfigFile logger filePath =
  readConfig logger =<< liftIO (BSL.readFile filePath)

{- |
Read a `Config` from a `BSL.ByteString`.
-}
readConfig ::
  Logger IO ->
  BSL.ByteString ->
  IO Config
readConfig logger fileContents = do
  case YAML.decode1 fileContents of
    Left (pos, errorMessage) -> do
      writeLog logger FATAL $
        T.pack $
          YAML.prettyPosWithSource pos fileContents " error" <> errorMessage
      liftIO exitFailure
    Right config -> pure config

{- |
Pretty-print a `Config` to YAML.
-}
prettyConfig :: Config -> Text
prettyConfig = TE.decodeUtf8Lenient . BSL.toStrict . YAML.encode1

{- |
Create a full configuration.
-}
toFullConfig ::
  -- | The @--eventlog-flush-interval@ in seconds.
  Double ->
  -- | The user configuration.
  Config ->
  FullConfig
toFullConfig eventlogFlushIntervalS config =
  FullConfig{..}
 where
  batchIntervalMs = toBatchIntervalMs eventlogFlushIntervalS config
  eventlogFlushIntervalX = toBatches batchIntervalMs eventlogFlushIntervalS

-------------------------------------------------------------------------------
-- Default Instances
-------------------------------------------------------------------------------

instance Default Config where
  def :: Config
  def = defaultConfig

instance Default Processors where
  def :: Processors
  def = $(getDefault @'["processors"] defaultConfig)

instance Default Logs where
  def :: Logs
  def = $(getDefault @'["processors", "logs"] defaultConfig)

instance Default Metrics where
  def :: Metrics
  def = $(getDefault @'["processors", "metrics"] defaultConfig)

instance Default Traces where
  def :: Traces
  def = $(getDefault @'["processors", "traces"] defaultConfig)

instance Default Profiles where
  def :: Profiles
  def = $(getDefault @'["processors", "profiles"] defaultConfig)

-- NOTE: This should be kept in sync with the list of logs.
--       Specifically, there should be a `Default` instance for every log.

instance Default ThreadLabelLog where
  def :: ThreadLabelLog
  def = $(getDefault @'["processors", "logs", "threadLabel"] defaultConfig)

instance Default UserMarkerLog where
  def :: UserMarkerLog
  def = $(getDefault @'["processors", "logs", "userMarker"] defaultConfig)

instance Default UserMessageLog where
  def :: UserMessageLog
  def = $(getDefault @'["processors", "logs", "userMessage"] defaultConfig)

instance Default InternalLogMessageLog where
  def :: InternalLogMessageLog
  def = $(getDefault @'["processors", "logs", "internalLogMessage"] defaultConfig)

-- NOTE: This should be kept in sync with the list of metrics.
--       Specifically, there should be a `Default` instance for every metric.

instance Default HeapAllocatedMetric where
  def :: HeapAllocatedMetric
  def = $(getDefault @'["processors", "metrics", "heapAllocated"] defaultConfig)

instance Default BlocksSizeMetric where
  def :: BlocksSizeMetric
  def = $(getDefault @'["processors", "metrics", "blocksSize"] defaultConfig)

instance Default HeapSizeMetric where
  def :: HeapSizeMetric
  def = $(getDefault @'["processors", "metrics", "heapSize"] defaultConfig)

instance Default HeapLiveMetric where
  def :: HeapLiveMetric
  def = $(getDefault @'["processors", "metrics", "heapLive"] defaultConfig)

instance Default MemCurrentMetric where
  def :: MemCurrentMetric
  def = $(getDefault @'["processors", "metrics", "memCurrent"] defaultConfig)

instance Default MemNeededMetric where
  def :: MemNeededMetric
  def = $(getDefault @'["processors", "metrics", "memNeeded"] defaultConfig)

instance Default MemReturnedMetric where
  def :: MemReturnedMetric
  def = $(getDefault @'["processors", "metrics", "memReturned"] defaultConfig)

instance Default GcCopiedMetric where
  def :: GcCopiedMetric
  def = $(getDefault @'["processors", "metrics", "gcCopied"] defaultConfig)

instance Default GcSlopMetric where
  def :: GcSlopMetric
  def = $(getDefault @'["processors", "metrics", "gcSlop"] defaultConfig)

instance Default GcFragmentationMetric where
  def :: GcFragmentationMetric
  def = $(getDefault @'["processors", "metrics", "gcFragmentation"] defaultConfig)

instance Default HeapProfSampleMetric where
  def :: HeapProfSampleMetric
  def = $(getDefault @'["processors", "metrics", "heapProfSample"] defaultConfig)

instance Default CapabilityUsageMetric where
  def :: CapabilityUsageMetric
  def = $(getDefault @'["processors", "metrics", "capabilityUsage"] defaultConfig)

instance Default ProductivityMetric where
  def :: ProductivityMetric
  def = $(getDefault @'["processors", "metrics", "productivity"] defaultConfig)

-- NOTE: This should be kept in sync with the list of traces.
--       Specifically, there should be a `Default` instance for every trace.

instance Default CapabilityUsageSpan where
  def :: CapabilityUsageSpan
  def = $(getDefault @'["processors", "traces", "capabilityUsage"] defaultConfig)

instance Default ThreadStateSpan where
  def :: ThreadStateSpan
  def = $(getDefault @'["processors", "traces", "threadState"] defaultConfig)

-- NOTE: This should be kept in sync with the list of profiles.
--       Specifically, there should be a `Default` instance for every profile.

instance Default CallStackProfile where
  def :: CallStackProfile
  def = $(getDefault @'["processors", "profiles", "callStackProfile"] defaultConfig)

instance Default CostCentreStackProfile where
  def :: CostCentreStackProfile
  def = $(getDefault @'["processors", "profiles", "costCentreStackProfile"] defaultConfig)

-------------------------------------------------------------------------------
-- KnownLog & Instances
-------------------------------------------------------------------------------

type KnownLog :: Type -> Constraint
class
  ( HasField (GetLogName log) Logs (Maybe log)
  , IsLogProcessorConfig log
  , Show log
  , Default log
  , KnownSymbol (GetLogName log)
  ) =>
  KnownLog log
  where
  type GetLogName log :: Symbol

logConfig :: forall log. (KnownLog log) => Logs -> Maybe log
logConfig = getField @(GetLogName log)
{-# INLINE logConfig #-}

instance KnownLog ThreadLabelLog where
  type GetLogName ThreadLabelLog = "threadLabel"

instance KnownLog UserMarkerLog where
  type GetLogName UserMarkerLog = "userMarker"

instance KnownLog UserMessageLog where
  type GetLogName UserMessageLog = "userMessage"

instance KnownLog InternalLogMessageLog where
  type GetLogName InternalLogMessageLog = "internalLogMessage"

-------------------------------------------------------------------------------
-- KnownMetric & Instances
-------------------------------------------------------------------------------

type KnownMetric :: Type -> Constraint
class
  ( HasField (GetMetricName metric) Metrics (Maybe metric)
  , IsMetricProcessorConfig metric
  , Show metric
  , Default metric
  , KnownSymbol (GetMetricName metric)
  , KnownMetricType (GetMetricType metric)
  , KnownMetricKind (GetMetricKind metric)
  , KnownMetricUnit (GetMetricUnit metric)
  ) =>
  KnownMetric metric
  where
  type GetMetricName metric :: Symbol
  type GetMetricType metric :: Type
  type GetMetricUnit metric :: MetricUnit
  type GetMetricKind metric :: MetricKind

metricConfig :: forall metric. (KnownMetric metric) => Metrics -> Maybe metric
metricConfig = getField @(GetMetricName metric)
{-# INLINE metricConfig #-}

-- NOTE: This should be kept in sync with the list of metrics.
--       Specifically, there should be a `KnownMetric` instance for every metric.

instance KnownMetric HeapAllocatedMetric where
  type GetMetricName HeapAllocatedMetric = "heapAllocated"
  type GetMetricType HeapAllocatedMetric = Word64
  type GetMetricUnit HeapAllocatedMetric = 'Byte
  type GetMetricKind HeapAllocatedMetric = 'Sum 'Cumulative 'Monotonic

instance KnownMetric HeapSizeMetric where
  type GetMetricName HeapSizeMetric = "heapSize"
  type GetMetricType HeapSizeMetric = Word64
  type GetMetricUnit HeapSizeMetric = 'Byte
  type GetMetricKind HeapSizeMetric = 'Gauge

instance KnownMetric BlocksSizeMetric where
  type GetMetricName BlocksSizeMetric = "blocksSize"
  type GetMetricType BlocksSizeMetric = Word64
  type GetMetricUnit BlocksSizeMetric = 'Byte
  type GetMetricKind BlocksSizeMetric = 'Gauge

instance KnownMetric HeapLiveMetric where
  type GetMetricName HeapLiveMetric = "heapLive"
  type GetMetricType HeapLiveMetric = Word64
  type GetMetricUnit HeapLiveMetric = 'Byte
  type GetMetricKind HeapLiveMetric = 'Gauge

instance KnownMetric MemCurrentMetric where
  type GetMetricName MemCurrentMetric = "memCurrent"
  type GetMetricType MemCurrentMetric = Word32
  type GetMetricUnit MemCurrentMetric = 'MegaBlock
  type GetMetricKind MemCurrentMetric = 'Gauge

instance KnownMetric MemNeededMetric where
  type GetMetricName MemNeededMetric = "memNeeded"
  type GetMetricType MemNeededMetric = Word32
  type GetMetricUnit MemNeededMetric = 'MegaBlock
  type GetMetricKind MemNeededMetric = 'Gauge

instance KnownMetric MemReturnedMetric where
  type GetMetricName MemReturnedMetric = "memReturned"
  type GetMetricType MemReturnedMetric = Word32
  type GetMetricUnit MemReturnedMetric = 'MegaBlock
  type GetMetricKind MemReturnedMetric = 'Gauge

instance KnownMetric GcCopiedMetric where
  type GetMetricName GcCopiedMetric = "gcCopied"
  type GetMetricType GcCopiedMetric = Word64
  type GetMetricUnit GcCopiedMetric = 'Byte
  type GetMetricKind GcCopiedMetric = 'Gauge

instance KnownMetric GcSlopMetric where
  type GetMetricName GcSlopMetric = "gcSlop"
  type GetMetricType GcSlopMetric = Word64
  type GetMetricUnit GcSlopMetric = 'Byte
  type GetMetricKind GcSlopMetric = 'Gauge

instance KnownMetric GcFragmentationMetric where
  type GetMetricName GcFragmentationMetric = "gcFragmentation"
  type GetMetricType GcFragmentationMetric = Word64
  type GetMetricUnit GcFragmentationMetric = 'Byte
  type GetMetricKind GcFragmentationMetric = 'Gauge

instance KnownMetric HeapProfSampleMetric where
  type GetMetricName HeapProfSampleMetric = "heapProfSample"
  type GetMetricType HeapProfSampleMetric = Word64
  type GetMetricUnit HeapProfSampleMetric = 'Byte
  type GetMetricKind HeapProfSampleMetric = 'Gauge

instance KnownMetric CapabilityUsageMetric where
  type GetMetricName CapabilityUsageMetric = "capabilityUsage"
  type GetMetricType CapabilityUsageMetric = Timestamp
  type GetMetricUnit CapabilityUsageMetric = 'NanoSecond
  type GetMetricKind CapabilityUsageMetric = 'Sum 'Cumulative 'Monotonic

instance KnownMetric ProductivityMetric where
  type GetMetricName ProductivityMetric = "productivity"
  type GetMetricType ProductivityMetric = Double
  type GetMetricUnit ProductivityMetric = 'Percent
  type GetMetricKind ProductivityMetric = 'Gauge

-------------------------------------------------------------------------------
-- KnownTrace & Instances
-------------------------------------------------------------------------------

type KnownTrace :: Type -> Constraint
class
  ( HasField (GetTraceName trace) Traces (Maybe trace)
  , IsTraceProcessorConfig trace
  , Show trace
  , Default trace
  , KnownSymbol (GetTraceName trace)
  ) =>
  KnownTrace trace
  where
  type GetTraceName trace :: Symbol

traceConfig :: forall trace. (KnownTrace trace) => Traces -> Maybe trace
traceConfig = getField @(GetTraceName trace)
{-# INLINE traceConfig #-}

instance KnownTrace CapabilityUsageSpan where
  type GetTraceName CapabilityUsageSpan = "capabilityUsage"

instance KnownTrace ThreadStateSpan where
  type GetTraceName ThreadStateSpan = "threadState"

-------------------------------------------------------------------------------
-- KnownProfile & Instances
-------------------------------------------------------------------------------

type KnownProfile :: Type -> Constraint
class
  ( HasField (GetProfileName profile) Profiles (Maybe profile)
  , IsProfileProcessorConfig profile
  , Show profile
  , Default profile
  , KnownSymbol (GetProfileName profile)
  , KnownSymbol (GetProfileMetricName profile)
  , Integral (GetProfileMetricType profile)
  , KnownSymbol (GetProfileMetricUnit profile)
  ) =>
  KnownProfile profile
  where
  type GetProfileName profile :: Symbol
  type GetProfileMetricName profile :: Symbol
  type GetProfileMetricType profile :: Type
  type GetProfileMetricUnit profile :: Symbol

profileConfig :: forall profile. (KnownProfile profile) => Profiles -> Maybe profile
profileConfig = getField @(GetProfileName profile)
{-# INLINE profileConfig #-}

instance KnownProfile CallStackProfile where
  type GetProfileName CallStackProfile = "callStackProfile"
  type GetProfileMetricName CallStackProfile = "callStack"
  type GetProfileMetricType CallStackProfile = Word8
  type GetProfileMetricUnit CallStackProfile = "count"

instance KnownProfile CostCentreStackProfile where
  type GetProfileName CostCentreStackProfile = "costCentreStackProfile"
  type GetProfileMetricName CostCentreStackProfile = "costCentreStack"
  type GetProfileMetricType CostCentreStackProfile = Word8
  type GetProfileMetricUnit CostCentreStackProfile = "count"

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------

{- |
Get the user-specified processor configuration.
-}
userProcessorConfig ::
  (Processors -> Maybe processorGroup) ->
  (processorGroup -> Maybe processorConfig) ->
  FullConfig ->
  Maybe processorConfig
userProcessorConfig group processor fullConfig =
  processor =<< group =<< fullConfig.config.processors

{- |
Get whether or not a processor is enabled.
-}
processorEnabled ::
  (HasField "export" processorConfig (Maybe ExportStrategy)) =>
  (Processors -> Maybe processorGroup) ->
  (processorGroup -> Maybe processorConfig) ->
  FullConfig ->
  Bool
processorEnabled group processor =
  isEnabled . userProcessorConfig group processor

{- |
Get the description corresponding to a processor.
-}
processorDescription ::
  forall a b.
  (Default b, HasField "description" b (Maybe Text)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Maybe Text
processorDescription group processor =
  (.description) . fromMaybe (def :: b) . userProcessorConfig group processor

{- |
Get the name corresponding to a processor.
-}
processorName ::
  forall a b.
  (HasCallStack, Show b, Default b, HasField "name" b (Maybe Text)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Text
processorName group processor =
  fromMaybe defaultName . ((.name) <=< userProcessorConfig group processor)
 where
  defaultName = fromMaybe (error errMsg) config.name
   where
    config = def :: b
    errMsg = "The default configuration has no name: " <> show config

--------------------------------------------------------------------------------
-- Aggregation Strategy

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
processorAggregationStrategy ::
  (Default b, HasField "aggregate" b (Maybe AggregationStrategy)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Maybe AggregationStrategy
processorAggregationStrategy group field =
  (.aggregate) . fromMaybe def . userProcessorConfig group field

{- |
Convert an `AggregationStrategy` to a number of batches.

__Precondition:__
If the aggregation strategy is defined in seconds,
then the batch interval should divide this duration.
-}
toAggregationBatches ::
  -- | The batch interval in milliseconds.
  Int ->
  -- | The @--eventlog-flush-interval@ in /batches/.
  Int ->
  -- | The aggregation strategy.
  Maybe AggregationStrategy ->
  Int
toAggregationBatches batchIntervalMs eventlogFlushIntervalX = \case
  -- If the setting is '60s' this means 60 seconds.
  Just (AggregationStrategyDuration DurationBySeconds{..}) -> toMilli seconds `div` batchIntervalMs
  -- If the setting is '60x' this means 60 times the /eventlog flush interval/,
  -- not the interal batch interval.
  Just (AggregationStrategyDuration DurationByBatches{..}) -> batches * eventlogFlushIntervalX
  -- If the setting is 'true' this means '1x', i.e., /eventlog flush interval/.
  Just AggregationStrategyBool{..} | isOn -> eventlogFlushIntervalX
  -- If the setting is absent or 'false' this means /do not aggregate/.
  Nothing -> 0
  Just AggregationStrategyBool{..} -> assert (not isOn) 0

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
processorAggregationBatches ::
  (Default b, HasField "aggregate" b (Maybe AggregationStrategy)) =>
  -- | The accessor for the sub-group of processors.
  (Processors -> Maybe a) ->
  -- | The accessor for the individual processor.
  (a -> Maybe b) ->
  -- | The full configuration.
  FullConfig ->
  Int
processorAggregationBatches group field fullConfig =
  toAggregationBatches fullConfig.batchIntervalMs fullConfig.eventlogFlushIntervalX $
    processorAggregationStrategy group field fullConfig

{- |
Get all aggregation strategies.
-}
allAggregationStrategies ::
  Config ->
  [AggregationStrategy]
allAggregationStrategies =
  catMaybes . with (.processors) (with (.metrics) (forEachMetricProcessor ((.aggregate) =<<)))

{- |
Get the largest aggregation strategy in batches.
-}
maximumAggregationBatches ::
  FullConfig ->
  Int
maximumAggregationBatches fullConfig =
  maximum . fmap (toAggregationBatches fullConfig.batchIntervalMs fullConfig.eventlogFlushIntervalX . Just) $
    allAggregationStrategies fullConfig.config

--------------------------------------------------------------------------------
-- Export Strategy

{- |
Get the export strategy corresponding to a processor.
-}
processorExportStrategy ::
  (Default b, HasField "export" b (Maybe ExportStrategy)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Maybe ExportStrategy
processorExportStrategy group field =
  (.export) . fromMaybe def . userProcessorConfig group field

{- |
Convert an `ExportStrategy` to a number of batches.

__Precondition:__
If the export strategy is defined in seconds,
then the batch interval should divide this duration.
-}
toExportBatches ::
  -- | The batch interval in milliseconds.
  Int ->
  -- | The @--eventlog-flush-interval@ in /batches/.
  Int ->
  -- | The export strategy.
  Maybe ExportStrategy ->
  Int
toExportBatches batchIntervalMs eventlogFlushIntervalX = \case
  -- If the setting is '60s' this means 60 seconds.
  Just (ExportStrategyDuration DurationBySeconds{..}) -> toMilli seconds `div` batchIntervalMs
  -- If the setting is '60x' this means 60 times the /eventlog flush interval/,
  -- not the interal batch interval.
  Just (ExportStrategyDuration DurationByBatches{..}) -> batches * eventlogFlushIntervalX
  -- If the setting is 'true' this means '1x', i.e., /eventlog flush interval/.
  Just ExportStrategyBool{..} | isOn -> eventlogFlushIntervalX
  -- If the setting is absent or 'false' this means /do not aggregate/.
  Nothing -> 0
  Just ExportStrategyBool{..} -> assert (not isOn) 0

{- |
Get the export strategy corresponding to processor in batches.
-}
processorExportBatches ::
  (Default b, HasField "export" b (Maybe ExportStrategy)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Int
processorExportBatches group field fullConfig =
  toExportBatches fullConfig.batchIntervalMs fullConfig.eventlogFlushIntervalX $
    processorExportStrategy group field fullConfig

{- |
Get all export strategies.
-}
allExportStrategies ::
  Config ->
  [ExportStrategy]
allExportStrategies =
  catMaybes . with (.processors) (forEachProcessor ((.export) =<<))

{- |
Get the largest export strategy in batches.
-}
maximumExportBatches ::
  FullConfig ->
  Int
maximumExportBatches fullConfig =
  maximum . fmap (toExportBatches fullConfig.batchIntervalMs fullConfig.eventlogFlushIntervalX . Just) $
    allExportStrategies fullConfig.config

-------------------------------------------------------------------------------
-- Batch Interval

{- |
Get the batch interval such that all user-specified intervals can be respected.
-}
toBatchIntervalMs ::
  -- | The @--eventlog-flush-interval@.
  Double ->
  -- | The configuration.
  Config ->
  Int
toBatchIntervalMs eventlogFlushIntervalS config =
  (.getGCD) . sconcat . fmap GCD $
    eventlogFlushIntervalMs :| aggregationIntervalsMs <> exportIntervalsMs
 where
  -- TODO: Check if any intervals round to 0ms.
  eventlogFlushIntervalMs =
    toMilli eventlogFlushIntervalS
  aggregationIntervalsMs =
    mapMaybe (fmap toMilli . toAggregationSeconds) . allAggregationStrategies $ config
  exportIntervalsMs =
    mapMaybe (fmap toMilli . toExportSeconds) . allExportStrategies $ config

{- |
Get the relevant interval in batches.

__Precondition:__ The batch interval divides the relevant interval.
-}
toBatches ::
  -- | The batch interval in milliseconds.
  Int ->
  -- | The relevant interval in seconds.
  Double ->
  Int
toBatches batchIntervalMs intervalS =
  toMilli intervalS `div` batchIntervalMs

-------------------------------------------------------------------------------
-- Exporters
-------------------------------------------------------------------------------

shouldExportLogs :: FullConfig -> Bool
shouldExportLogs =
  getAny
    . with
      (.processors)
      ( with
          (.logs)
          (mconcat . forEachLogProcessor (Any . isEnabled))
      )
    . (.config)

shouldExportMetrics :: FullConfig -> Bool
shouldExportMetrics =
  getAny
    . with
      (.processors)
      ( with
          (.metrics)
          (mconcat . forEachMetricProcessor (Any . isEnabled))
      )
    . (.config)

shouldExportTraces :: FullConfig -> Bool
shouldExportTraces =
  getAny
    . with
      (.processors)
      ( with
          (.traces)
          (mconcat . forEachTraceProcessor (Any . isEnabled))
      )
    . (.config)

shouldExportProfiles :: FullConfig -> Bool
shouldExportProfiles =
  getAny
    . with
      (.processors)
      ( with
          (.profiles)
          (mconcat . forEachProfileProcessor (Any . isEnabled))
      )
    . (.config)

-------------------------------------------------------------------------------
-- Functors for processor configurations
-------------------------------------------------------------------------------

{- |
Apply a function to each processor.
-}
forEachProcessor ::
  ( forall processorConfig.
    (IsProcessorConfig processorConfig) =>
    Maybe processorConfig -> a
  ) ->
  Processors ->
  [a]
forEachProcessor f processors =
  concatMap (fromMaybe []) $
    [ forEachLogProcessor f <$> processors.logs
    , forEachMetricProcessor f <$> processors.metrics
    , forEachTraceProcessor f <$> processors.traces
    , forEachProfileProcessor f <$> processors.profiles
    ]

{- |
Apply a function to each metric processor.
-}
forEachLogProcessor ::
  ( forall traceProcessorConfig.
    (IsLogProcessorConfig traceProcessorConfig) =>
    Maybe traceProcessorConfig -> a
  ) ->
  Logs ->
  [a]
forEachLogProcessor f logs =
  [ -- NOTE: This should be kept in sync with the list of logs.
    f logs.threadLabel
  , f logs.userMarker
  , f logs.userMessage
  , f logs.internalLogMessage
  ]

{- |
Apply a function to each metric processor.
-}
forEachMetricProcessor ::
  ( forall metricProcessorConfig.
    (IsMetricProcessorConfig metricProcessorConfig) =>
    Maybe metricProcessorConfig -> a
  ) ->
  Metrics ->
  [a]
forEachMetricProcessor f metrics =
  [ -- NOTE: This should be kept in sync with the list of metrics.
    f metrics.heapAllocated
  , f metrics.blocksSize
  , f metrics.heapSize
  , f metrics.heapLive
  , f metrics.memCurrent
  , f metrics.memNeeded
  , f metrics.memReturned
  , f metrics.heapProfSample
  , f metrics.capabilityUsage
  ]

{- |
Apply a function to each metric processor.
-}
forEachTraceProcessor ::
  ( forall traceProcessorConfig.
    (IsTraceProcessorConfig traceProcessorConfig) =>
    Maybe traceProcessorConfig -> a
  ) ->
  Traces ->
  [a]
forEachTraceProcessor f traces =
  [ -- NOTE: This should be kept in sync with the list of traces.
    f traces.capabilityUsage
  , f traces.threadState
  ]

{- |
Apply a function to each metric processor.
-}
forEachProfileProcessor ::
  ( forall profileProcessorConfig.
    (IsProfileProcessorConfig profileProcessorConfig) =>
    Maybe profileProcessorConfig -> a
  ) ->
  Profiles ->
  [a]
forEachProfileProcessor f profiles =
  [ -- NOTE: This should be kept in sync with the list of profiles.
    f profiles.callStackProfile
  , f profiles.costCentreStackProfile
  ]

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper.
-}
with :: (Foldable f, Monoid r) => (s -> f t) -> (t -> r) -> s -> r
with = flip ((.) . foldMap)

{- |
Internal helper.
Convert seconds to milliseconds.
-}
toMilli :: Double -> Int
toMilli seconds = round (seconds * 1_000)

{- |
Internal helper.
Wrapper that provides a `Semigroup` instance for `gcd`.
-}
newtype GCD a = GCD {getGCD :: a}

instance (Integral a) => Semigroup (GCD a) where
  (<>) :: GCD a -> GCD a -> GCD a
  x <> y = GCD{getGCD = x.getGCD `gcd` y.getGCD}
