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

  -- ** Processor configuration types
  IsProcessorConfig,
  Processors (..),
  processorEnabled,
  processorDescription,
  processorName,

  -- *** Metric processor configuration types
  Metrics (..),
  IsMetricProcessorConfig,
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
  CapabilityUsageSpan (..),
  ThreadStateSpan (..),

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
) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy qualified as BSL
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Any (..), First (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.YAML qualified as YAML
import GHC.Eventlog.Live.Logger (logError)
import GHC.Eventlog.Live.Otelcol.Config.Default (defaultConfig, getDefault)
import GHC.Eventlog.Live.Otelcol.Config.Types
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.Records (HasField)
import GHC.Stack.Types (HasCallStack)
import System.Exit (exitFailure)

{- |
Read a `Config` from a configuration file.
-}
readConfigFile ::
  (MonadIO m) =>
  Verbosity ->
  FilePath ->
  m Config
readConfigFile verbosity filePath =
  readConfig verbosity =<< liftIO (BSL.readFile filePath)

{- |
Read a `Config` from a `BSL.ByteString`.
-}
readConfig ::
  (MonadIO m) =>
  Verbosity ->
  BSL.ByteString ->
  m Config
readConfig verbosity fileContents =
  liftIO $ do
    case YAML.decode1 fileContents of
      Left (pos, errorMessage) -> do
        logError verbosity . T.pack $
          YAML.prettyPosWithSource pos fileContents " error" <> errorMessage
        exitFailure
      Right config -> pure config

{- |
Pretty-print a `Config` to YAML.
-}
prettyConfig :: Config -> Text
prettyConfig = TE.decodeUtf8Lenient . BSL.toStrict . YAML.encode1

-------------------------------------------------------------------------------
-- Default Instances
-------------------------------------------------------------------------------

instance Default Config where
  def :: Config
  def = defaultConfig

instance Default Processors where
  def :: Processors
  def = $(getDefault @'["processors"] defaultConfig)

instance Default Metrics where
  def :: Metrics
  def = $(getDefault @'["processors", "metrics"] defaultConfig)

instance Default Traces where
  def :: Traces
  def = $(getDefault @'["processors", "traces"] defaultConfig)

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
  Config ->
  Bool
processorEnabled group field =
  getAny . with (.processors) (with group (with field (Any . (.enabled))))

{- |
Get the description corresponding to a processor.
-}
processorDescription ::
  (Default b, HasField "description" b (Maybe Text)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  Config ->
  Maybe Text
processorDescription group field =
  (.description) . fromMaybe def . getFirst . with (.processors) (with group (First . field))

{- |
Get the name corresponding to a processor.

__Warning:__ This assumes the value of @`def`.`name`@ is `Just` some `Text`.
-}
processorName ::
  forall a b.
  (HasCallStack, Default b, HasField "name" b (Maybe Text)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  Config ->
  Text
processorName group field =
  fromMaybe defaultName . ((.name) <=< getFirst) . with (.processors) (with group (First . field))
 where
  defaultName :: (HasCallStack) => Text
  defaultName = case (def :: b).name of
    Nothing -> error "The default configuration for this metric has no name."
    Just name -> name

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
processorAggregationStrategy ::
  (Default b, HasField "aggregate" b (Maybe AggregationStrategy)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  Config ->
  Maybe AggregationStrategy
processorAggregationStrategy group field =
  (.aggregate) . fromMaybe def . getFirst . with (.processors) (with group (First . field))

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
processorAggregationBatches ::
  (Default b, HasField "aggregate" b (Maybe AggregationStrategy)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  Config ->
  Int
processorAggregationBatches group field =
  toAggregationBatches . processorAggregationStrategy group field

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
maximumAggregationBatches ::
  Config ->
  Int
maximumAggregationBatches config =
  maximum
    [ -- NOTE: This should be kept in sync with the list of metrics.
      processorAggregationBatches (.metrics) (.heapAllocated) config
    , processorAggregationBatches (.metrics) (.blocksSize) config
    , processorAggregationBatches (.metrics) (.heapSize) config
    , processorAggregationBatches (.metrics) (.heapLive) config
    , processorAggregationBatches (.metrics) (.memCurrent) config
    , processorAggregationBatches (.metrics) (.memNeeded) config
    , processorAggregationBatches (.metrics) (.memReturned) config
    , processorAggregationBatches (.metrics) (.heapProfSample) config
    , processorAggregationBatches (.metrics) (.capabilityUsage) config
    ]

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
processorExportStrategy ::
  (Default b, HasField "export" b (Maybe ExportStrategy)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  Config ->
  Maybe ExportStrategy
processorExportStrategy group field =
  (.export) . fromMaybe def . getFirst . with (.processors) (with group (First . field))

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
processorExportBatches ::
  (Default b, HasField "export" b (Maybe ExportStrategy)) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  Config ->
  Int
processorExportBatches group field =
  toExportBatches . processorExportStrategy group field

{- |
Get the aggregation strategy corresponding to a metric processor.
-}
maximumExportBatches ::
  Config ->
  Int
maximumExportBatches config =
  maximum
    [ -- NOTE: This should be kept in sync with the list of metrics
      processorExportBatches (.metrics) (.heapAllocated) config
    , processorExportBatches (.metrics) (.blocksSize) config
    , processorExportBatches (.metrics) (.heapSize) config
    , processorExportBatches (.metrics) (.heapLive) config
    , processorExportBatches (.metrics) (.memCurrent) config
    , processorExportBatches (.metrics) (.memNeeded) config
    , processorExportBatches (.metrics) (.memReturned) config
    , processorExportBatches (.metrics) (.heapProfSample) config
    , processorExportBatches (.metrics) (.capabilityUsage) config
    , -- NOTE: This should be kept in sync with the list of traces.
      processorExportBatches (.traces) (.capabilityUsage) config
    , processorExportBatches (.traces) (.threadState) config
    ]

{- |
Internal helper.
-}
with :: (Foldable f, Monoid r) => (s -> f t) -> (t -> r) -> s -> r
with = flip ((.) . foldMap)
