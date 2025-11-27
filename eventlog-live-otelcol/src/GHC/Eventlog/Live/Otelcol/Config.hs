{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Config
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config (
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
  shouldExportLogs,
  ThreadLabel (..),
  UserMarker (..),
  UserMessage (..),
  InternalLogMessage (..),

  -- *** Metric processor configuration types
  Metrics (..),
  IsMetricProcessorConfig,
  shouldExportMetrics,
  HeapAllocatedMetric (..),
  BlocksSizeMetric (..),
  HeapSizeMetric (..),
  HeapLiveMetric (..),
  MemCurrentMetric (..),
  MemNeededMetric (..),
  MemReturnedMetric (..),
  HeapProfSampleMetric (..),
  CapabilityUsageMetric (..),

  -- *** Trace processor configuration types
  Traces (..),
  IsTraceProcessorConfig,
  shouldExportTraces,
  CapabilityUsageSpan (..),
  ThreadStateSpan (..),

  -- *** Profiler processor configuration types
  Profiles (..),
  IsProfileProcessorConfig,
  shouldExportProfiles,
  StackSampleProfile (..),

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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Monoid (Any (..), First (..))
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.YAML qualified as YAML
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Otelcol.Config.Default (defaultConfig, getDefault)
import GHC.Eventlog.Live.Otelcol.Config.Types
import GHC.Records (HasField)
import GHC.Stack.Types (HasCallStack)
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

instance Default ThreadLabel where
  def :: ThreadLabel
  def = $(getDefault @'["processors", "logs", "threadLabel"] defaultConfig)

instance Default UserMarker where
  def :: UserMarker
  def = $(getDefault @'["processors", "logs", "userMarker"] defaultConfig)

instance Default UserMessage where
  def :: UserMessage
  def = $(getDefault @'["processors", "logs", "userMessage"] defaultConfig)

instance Default InternalLogMessage where
  def :: InternalLogMessage
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

instance Default HeapProfSampleMetric where
  def :: HeapProfSampleMetric
  def = $(getDefault @'["processors", "metrics", "heapProfSample"] defaultConfig)

instance Default CapabilityUsageMetric where
  def :: CapabilityUsageMetric
  def = $(getDefault @'["processors", "metrics", "capabilityUsage"] defaultConfig)

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

instance Default StackSampleProfile where
  def :: StackSampleProfile
  def = $(getDefault @'["processors", "profiles", "stackSample"] defaultConfig)

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------

{- |
Get whether or not a processor is enabled.
-}
processorEnabled ::
  (HasField "enabled" b Bool) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Bool
processorEnabled group field =
  getAny . with (.processors) (with group (with field (Any . (.enabled)))) . (.config)

{- |
Get the description corresponding to a processor.
-}
processorDescription ::
  (Default b, HasField "description" b (Maybe Text)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Maybe Text
processorDescription group field =
  (.description) . fromMaybe def . getFirst . with (.processors) (with group (First . field)) . (.config)

{- |
Get the name corresponding to a processor.

__Warning:__ This assumes the value of @`def`.`name`@ is `Just` some `Text`.
-}
processorName ::
  forall a b.
  (HasCallStack, Default b, HasField "name" b (Maybe Text)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  FullConfig ->
  Text
processorName group field =
  fromMaybe defaultName . ((.name) <=< getFirst) . with (.processors) (with group (First . field)) . (.config)
 where
  defaultName :: (HasCallStack) => Text
  defaultName = case (def :: b).name of
    Nothing -> error "The default configuration for this metric has no name."
    Just name -> name

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
  (.aggregate) . fromMaybe def . getFirst . with (.processors) (with group (First . field)) . (.config)

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
  catMaybes . with (.processors) (with (.metrics) (forEachMetricProcessor (.aggregate)))

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
  (.export) . fromMaybe def . getFirst . with (.processors) (with group (First . field)) . (.config)

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
  catMaybes . with (.processors) (forEachProcessor (.export))

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
          (mconcat . forEachLogProcessor (Any . isEnabled . (.export)))
      )
    . (.config)

shouldExportMetrics :: FullConfig -> Bool
shouldExportMetrics =
  getAny
    . with
      (.processors)
      ( with
          (.metrics)
          (mconcat . forEachMetricProcessor (Any . isEnabled . (.export)))
      )
    . (.config)

shouldExportTraces :: FullConfig -> Bool
shouldExportTraces =
  getAny
    . with
      (.processors)
      ( with
          (.traces)
          (mconcat . forEachTraceProcessor (Any . isEnabled . (.export)))
      )
    . (.config)

shouldExportProfiles :: FullConfig -> Bool
shouldExportProfiles =
  getAny
    . with
      (.processors)
      ( with
          (.profiles)
          (mconcat . forEachProfileProcessor (Any . isEnabled . (.export)))
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
    processorConfig ->
    a
  ) ->
  Processors ->
  [a]
forEachProcessor f processors =
  mconcat
    [ forEachLogProcessor f (fromMaybe def processors.logs)
    , forEachMetricProcessor f (fromMaybe def processors.metrics)
    , forEachTraceProcessor f (fromMaybe def processors.traces)
    , forEachProfileProcessor f (fromMaybe def processors.profiles)
    ]

{- |
Apply a function to each metric processor.
-}
forEachLogProcessor ::
  ( forall traceProcessorConfig.
    (IsLogProcessorConfig traceProcessorConfig) =>
    traceProcessorConfig ->
    a
  ) ->
  Logs ->
  [a]
forEachLogProcessor f logs =
  [ -- NOTE: This should be kept in sync with the list of logs.
    f $ fromMaybe def logs.threadLabel
  , f $ fromMaybe def logs.userMarker
  , f $ fromMaybe def logs.userMessage
  , f $ fromMaybe def logs.internalLogMessage
  ]

{- |
Apply a function to each metric processor.
-}
forEachMetricProcessor ::
  ( forall metricProcessorConfig.
    (IsMetricProcessorConfig metricProcessorConfig) =>
    metricProcessorConfig ->
    a
  ) ->
  Metrics ->
  [a]
forEachMetricProcessor f metrics =
  [ -- NOTE: This should be kept in sync with the list of metrics.
    f $ fromMaybe def metrics.heapAllocated
  , f $ fromMaybe def metrics.blocksSize
  , f $ fromMaybe def metrics.heapSize
  , f $ fromMaybe def metrics.heapLive
  , f $ fromMaybe def metrics.memCurrent
  , f $ fromMaybe def metrics.memNeeded
  , f $ fromMaybe def metrics.memReturned
  , f $ fromMaybe def metrics.heapProfSample
  , f $ fromMaybe def metrics.capabilityUsage
  ]

{- |
Apply a function to each metric processor.
-}
forEachTraceProcessor ::
  ( forall traceProcessorConfig.
    (IsTraceProcessorConfig traceProcessorConfig) =>
    traceProcessorConfig ->
    a
  ) ->
  Traces ->
  [a]
forEachTraceProcessor f traces =
  [ -- NOTE: This should be kept in sync with the list of traces.
    f $ fromMaybe def traces.capabilityUsage
  , f $ fromMaybe def traces.threadState
  ]

{- |
Apply a function to each metric processor.
-}
forEachProfileProcessor ::
  ( forall profileProcessorConfig.
    (IsProfileProcessorConfig profileProcessorConfig) =>
    profileProcessorConfig ->
    a
  ) ->
  Profiles ->
  [a]
forEachProfileProcessor f profiles =
  [ -- NOTE: This should be kept in sync with the list of profiles.
    f $ fromMaybe def profiles.stackSample
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
