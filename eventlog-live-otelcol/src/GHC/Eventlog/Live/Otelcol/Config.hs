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
  readConfig,
  Config (..),
  Processors (..),
  Metrics (..),
  Spans (..),
  AggregationStrategy (..),
  ExportStrategy (..),
  HeapAllocatedMetric (..),
  BlocksSizeMetric (..),
  HeapSizeMetric (..),
  HeapLiveMetric (..),
  MemCurrentMetric (..),
  MemNeededMetric (..),
  MemReturnedMetric (..),
  HeapProfSampleMetric (..),
  CapabilityUsageMetric (..),
  CapabilityUsageSpan (..),
  ThreadStateSpan (..),

  -- * Configuration types
  IsProcessorConfig,
  IsMetricProcessorConfig,
  IsSpanProcessorConfig,

  -- * Accessors
  processorEnabled,
  processorDescription,
  processorName,
  processorAggregationStrategy,
  processorAggregationBatches,
  maximumAggregationBatches,
  processorExportStrategy,
  processorExportBatches,
  maximumExportBatches,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (Default (..))
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any (..), First (..))
import Data.Text (Text)
import Data.Yaml qualified as Y
import GHC.Eventlog.Live.Otelcol.Config.Default (defaultConfig, getDefault)
import GHC.Eventlog.Live.Otelcol.Config.Types
import GHC.Records (HasField)

{- |
Read a `Config` from a configuration file.
-}
readConfig :: (MonadIO m) => FilePath -> m Config
readConfig = Y.decodeFileThrow

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

instance Default Spans where
  def :: Spans
  def = $(getDefault @'["processors", "spans"] defaultConfig)

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

instance Default CapabilityUsageSpan where
  def :: CapabilityUsageSpan
  def = $(getDefault @'["processors", "spans", "capabilityUsage"] defaultConfig)

instance Default ThreadStateSpan where
  def :: ThreadStateSpan
  def = $(getDefault @'["processors", "spans", "threadState"] defaultConfig)

-------------------------------------------------------------------------------
-- Configuration types
-------------------------------------------------------------------------------

{- |
The structural type of processor configurations.
-}
type IsProcessorConfig :: Type -> Constraint
type IsProcessorConfig config =
  ( Default config
  , HasField "description" config (Maybe Text)
  , HasField "enabled" config Bool
  , HasField "export" config (Maybe ExportStrategy)
  , HasField "name" config Text
  )

{- |
The structural type of metric processor configurations.
-}
type IsMetricProcessorConfig :: Type -> Constraint
type IsMetricProcessorConfig config =
  ( IsProcessorConfig config
  , HasField "aggregate" config (Maybe AggregationStrategy)
  )

{- |
The structural type of span processor configurations.
-}
type IsSpanProcessorConfig :: Type -> Constraint
type IsSpanProcessorConfig config =
  (IsProcessorConfig config)

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
-}
processorName ::
  (Default b, HasField "name" b Text) =>
  (Processors -> Maybe a) ->
  (a -> Maybe b) ->
  Config ->
  Text
processorName group field =
  (.name) . fromMaybe def . getFirst . with (.processors) (with group (First . field))

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
  -- NOTE: This list should be kept in sync with the fields of `Processors`.
  -- TODO: This is not checked for completeness, which is a shame.
  maximum
    [ processorAggregationBatches (.metrics) (.heapAllocated) config
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
  -- NOTE: This list should be kept in sync with the fields of `Processors`.
  -- TODO: This is not checked for completeness, which is a shame.
  maximum
    [ processorExportBatches (.metrics) (.heapAllocated) config
    , processorExportBatches (.metrics) (.blocksSize) config
    , processorExportBatches (.metrics) (.heapSize) config
    , processorExportBatches (.metrics) (.heapLive) config
    , processorExportBatches (.metrics) (.memCurrent) config
    , processorExportBatches (.metrics) (.memNeeded) config
    , processorExportBatches (.metrics) (.memReturned) config
    , processorExportBatches (.metrics) (.heapProfSample) config
    , processorExportBatches (.metrics) (.capabilityUsage) config
    , processorExportBatches (.spans) (.capabilityUsage) config
    , processorExportBatches (.spans) (.threadState) config
    ]

{- |
Internal helper.
-}
with :: (Foldable f, Monoid r) => (s -> f t) -> (t -> r) -> s -> r
with = flip ((.) . foldMap)
