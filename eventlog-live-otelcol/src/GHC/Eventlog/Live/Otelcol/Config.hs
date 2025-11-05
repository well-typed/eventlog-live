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
  processorEnabled,
  processorDescription,
  processorName,
  processorAggregationStrategy,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (Default (..))
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
Internal helper.
-}
with :: (Foldable f, Monoid r) => (s -> f t) -> (t -> r) -> s -> r
with = flip ((.) . foldMap)
