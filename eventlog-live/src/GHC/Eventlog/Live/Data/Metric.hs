{- |
Module      : GHC.Eventlog.Live.Metric
Description : Representation for metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Metric (
  Metric (..),
  KnownMetric (..),
  MetricTypeSing (..),
  withNum,
  MetricInstrumentKind (..),
  AggregationTemporailty (..),
) where

import Control.Exception (assert)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.Eventlog.Live.Data.Group (GroupBy (..))
import GHC.RTS.Events (Timestamp)
import GHC.TypeLits (Symbol)

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

instance GroupBy (Metric a) where
  type Key (Metric a) = Attrs
  toKey :: Metric a -> Attrs
  toKey = (.attrs)

instance (Semigroup a) => Semigroup (Metric a) where
  (<>) :: Metric a -> Metric a -> Metric a
  x <> y =
    assert (x.attrs == y.attrs) $
      Metric
        { value = x.value <> y.value
        , maybeTimeUnixNano = x.maybeTimeUnixNano `max` y.maybeTimeUnixNano
        , maybeStartTimeUnixNano = x.maybeStartTimeUnixNano `min` y.maybeStartTimeUnixNano
        , attrs = x.attrs
        }

data MetricTypeSing (a :: Type) where
  MetricTypeSingFloat :: MetricTypeSing Float
  MetricTypeSingDouble :: MetricTypeSing Double
  MetricTypeSingWord :: MetricTypeSing Word
  MetricTypeSingWord8 :: MetricTypeSing Word8
  MetricTypeSingWord16 :: MetricTypeSing Word16
  MetricTypeSingWord32 :: MetricTypeSing Word32
  MetricTypeSingWord64 :: MetricTypeSing Word64
  MetricTypeSingInt :: MetricTypeSing Int
  MetricTypeSingInt8 :: MetricTypeSing Int8
  MetricTypeSingInt16 :: MetricTypeSing Int16
  MetricTypeSingInt32 :: MetricTypeSing Int32
  MetricTypeSingInt64 :: MetricTypeSing Int64

{- |
Get the `Num` instance for a metric type.
-}
withNum :: MetricTypeSing a -> ((Num a) => r) -> r
withNum = \case
  MetricTypeSingFloat -> id
  MetricTypeSingDouble -> id
  MetricTypeSingWord -> id
  MetricTypeSingWord8 -> id
  MetricTypeSingWord16 -> id
  MetricTypeSingWord32 -> id
  MetricTypeSingWord64 -> id
  MetricTypeSingInt -> id
  MetricTypeSingInt8 -> id
  MetricTypeSingInt16 -> id
  MetricTypeSingInt32 -> id
  MetricTypeSingInt64 -> id

data MetricInstrumentKind
  = Sum {aggregationTemporailty :: !AggregationTemporailty, isMonotonic :: !Bool}
  | Gauge

data AggregationTemporailty
  = Cumulative
  | Delta

class KnownMetric (metricName :: Symbol) where
  type MetricType metricName :: Type
  metricTypeSing :: Proxy metricName -> MetricTypeSing (MetricType metricName)
  metricInstrumentKind :: Proxy metricName -> MetricInstrumentKind
