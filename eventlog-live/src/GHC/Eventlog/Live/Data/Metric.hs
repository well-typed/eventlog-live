{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Metric
Description : Representation for metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Metric (
  Metric (..),
  AggregateMetrics (..),
  singleton,
  elems,
) where

import Control.Exception (assert)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import GHC.Eventlog.Live.Data.Attribute (Attrs, (~=))
import GHC.RTS.Events (Timestamp)

{- |
Metrics combine a measurement with a timestamp representing the time of the
measurement, a timestamp representing the earliest possible measurment, and
a list of attributes.
-}
data Metric a = Metric
  { value :: !a
  -- ^ The measurement.
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  -- ^ The time at which the measurment was taken.
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  -- ^ The earliest time at which any measurement could have been taken.
  --   Usually, this represents the start time of a process.
  , attrs :: Attrs
  -- ^ A set of attributes.
  }
  deriving (Functor, Show)

{- |
A metrics group is a set of aggregated metrics grouped by their attributes.
-}
newtype AggregateMetrics a = AggregateMetrics {metricsByAttrs :: HashMap Attrs (AggregateMetric a)}

{- |
Internal helper.
An aggregate metric and the samples of combined metrics.
-}
data AggregateMetric a = AggregateMetric {metric :: !(Metric a), samples :: !Word}

{- |
Internal helper.
Convert an `AggregateMetric` to a `Metric` with an additional `eventlog_live__samples` attribute.
-}
toMetric :: AggregateMetric a -> Metric a
toMetric AggregateMetric{..} = metric{attrs = metric.attrs <> ["eventlog_live__samples" ~= samples]}

{- |
Construct the singleton metrics group.
-}
singleton :: Metric a -> AggregateMetrics a
singleton x = AggregateMetrics{metricsByAttrs = M.singleton x.attrs AggregateMetric{metric = x, samples = 1}}

{- |
Convert a metrics group to a list of metrics.
-}
elems :: AggregateMetrics a -> [Metric a]
elems xs = fmap toMetric $ M.elems xs.metricsByAttrs

instance (Semigroup a) => Semigroup (AggregateMetrics a) where
  (<>) :: AggregateMetrics a -> AggregateMetrics a -> AggregateMetrics a
  xs <> ys = AggregateMetrics{metricsByAttrs = M.unionWith (<>) xs.metricsByAttrs ys.metricsByAttrs}

instance (Semigroup a) => Semigroup (AggregateMetric a) where
  (<>) :: AggregateMetric a -> AggregateMetric a -> AggregateMetric a
  x <> y = AggregateMetric{metric = aggregateMetrics x.metric y.metric, samples = x.samples + y.samples}

{- |
Internal helper.
Aggregate two metrics with identical attributes.
-}
aggregateMetrics :: (Semigroup a) => Metric a -> Metric a -> Metric a
aggregateMetrics x y =
  assert (x.attrs == y.attrs) $
    Metric
      { value = x.value <> y.value
      , maybeTimeUnixNano = x.maybeTimeUnixNano `max` y.maybeTimeUnixNano
      , maybeStartTimeUnixNano = x.maybeStartTimeUnixNano `min` y.maybeStartTimeUnixNano
      , attrs = x.attrs
      }
