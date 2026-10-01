{- |
Module      : GHC.Eventlog.Live.Metric
Description : Representation for OTLP metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Metric (
  -- * Known Metrics
  SomeMetrics (..),
  KnownMetric (..),
  metricConfig,

  -- ** Monotonicity
  Monotonicity (..),
  SMonotonicity (..),
  KnownMonotonicity (..),

  -- ** Aggregation Temporality
  AggregationTemporality (..),
  SAggregationTemporality (..),
  KnownAggregationTemporality (..),

  -- ** Metric Kind
  MetricKind (..),
  SMetricKind (..),
  KnownMetricKind (..),

  -- ** Metric Unit
  MetricUnit (..),
  SMetricUnit (..),
  KnownMetricUnit (..),
  toUCUM,

  -- ** Metric Type
  SMetricType (..),
  KnownMetricType (..),

  -- * Metric superclass
  IsMetric,
  toMetric,

  -- * Generic metric type
  Metric (..),
) where

import Control.Exception (assert)
import Data.Default (Default)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Eventlog.Live.Config
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.Eventlog.Live.Data.Group (GroupBy (..))
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)

--------------------------------------------------------------------------------
-- Known Metrics
--------------------------------------------------------------------------------

type SomeMetrics :: Type
data SomeMetrics
  = forall metric. (KnownMetric metric) => SomeMetrics !(Proxy metric) [Metric (GetMetricType metric)]

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

--------------------------------------------------------------------------------
-- Superclass for metric types
--------------------------------------------------------------------------------

{- |
A metric is any type that has the fields of the generic metric type.
-}
type IsMetric ma a =
  ( HasField "value" ma a
  , HasField "maybeTimeUnixNano" ma (Maybe Timestamp)
  , HasField "maybeStartTimeUnixNano" ma (Maybe Timestamp)
  , HasField "attrs" ma Attrs
  )

toMetric :: (IsMetric ma a) => ma -> Metric a
toMetric ma =
  Metric
    { value = ma.value
    , maybeTimeUnixNano = ma.maybeTimeUnixNano
    , maybeStartTimeUnixNano = ma.maybeStartTimeUnixNano
    , attrs = ma.attrs
    }

--------------------------------------------------------------------------------
-- Generic metric type
--------------------------------------------------------------------------------

{- |
Metrics combine a measurement with a timestamp representing the time of the
measurement, a timestamp representing the earliest possible measurement, and
a list of attributes.
-}
data Metric a = Metric
  { value :: !a
  -- ^ The measurement.
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  -- ^ The time at which the measurement was taken.
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  {- ^ The earliest time at which any measurement could have been taken.
  Usually, this represents the start time of a process.
  -}
  , attrs :: Attrs
  -- ^ A set of attributes.
  }
  deriving (Functor, Foldable, Traversable, Show)

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

--------------------------------------------------------------------------------
-- Type-Level Information for Metrics
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Monotonicity

data Monotonicity
  = Monotonic
  | NonMonotonic

data SMonotonicity monotonicity where
  SMonotonic :: SMonotonicity 'Monotonic
  SNonMonotonic :: SMonotonicity 'NonMonotonic

type KnownMonotonicity :: Monotonicity -> Constraint
class KnownMonotonicity monotonicity where
  monotonicitySing :: Proxy monotonicity -> SMonotonicity monotonicity

instance KnownMonotonicity 'Monotonic where
  monotonicitySing _proxy = SMonotonic
  {-# INLINE monotonicitySing #-}

instance KnownMonotonicity 'NonMonotonic where
  monotonicitySing _proxy = SNonMonotonic
  {-# INLINE monotonicitySing #-}

--------------------------------------------------------------------------------
-- AggregationTemporality

data AggregationTemporality
  = Cumulative
  | Delta

data SAggregationTemporality aggregationTemporality where
  SCumulative :: SAggregationTemporality 'Cumulative
  SDelta :: SAggregationTemporality 'Delta

type KnownAggregationTemporality :: AggregationTemporality -> Constraint
class KnownAggregationTemporality aggregationTemporality where
  aggregationTemporalitySing :: Proxy aggregationTemporality -> SAggregationTemporality aggregationTemporality

instance KnownAggregationTemporality 'Cumulative where
  aggregationTemporalitySing _proxy = SCumulative
  {-# INLINE aggregationTemporalitySing #-}

instance KnownAggregationTemporality 'Delta where
  aggregationTemporalitySing _proxy = SDelta
  {-# INLINE aggregationTemporalitySing #-}

--------------------------------------------------------------------------------
-- Metric Point Kinds

data MetricKind
  = Gauge
  | Sum AggregationTemporality Monotonicity

data SMetricKind metricKind where
  SGauge :: SMetricKind 'Gauge
  SSum :: SAggregationTemporality aggregationTemporality -> SMonotonicity monotonicity -> SMetricKind ('Sum aggregationTemporality monotonicity)

type KnownMetricKind :: MetricKind -> Constraint
class KnownMetricKind metricKind where
  metricKindSing :: Proxy metricKind -> SMetricKind metricKind

instance KnownMetricKind 'Gauge where
  metricKindSing _proxy = SGauge
  {-# INLINE metricKindSing #-}

instance (KnownAggregationTemporality aggregationTemporality, KnownMonotonicity monotonicity) => KnownMetricKind ('Sum aggregationTemporality monotonicity) where
  metricKindSing _proxy = SSum (aggregationTemporalitySing (Proxy @aggregationTemporality)) (monotonicitySing (Proxy @monotonicity))
  {-# INLINE metricKindSing #-}

--------------------------------------------------------------------------------
-- Metric Units

data MetricUnit
  = Byte
  | MegaBlock
  | NanoSecond
  | Percent

data SMetricUnit metricUnit where
  SByte :: SMetricUnit 'Byte
  SMegaBlock :: SMetricUnit 'MegaBlock
  SNanoSecond :: SMetricUnit 'NanoSecond
  SPercent :: SMetricUnit 'Percent

toUCUM :: SMetricUnit metricUnit -> Text
toUCUM =
  T.pack . \case
    SByte -> "By"
    SMegaBlock -> "{mblock}"
    SNanoSecond -> "ns"
    SPercent -> "%"

type KnownMetricUnit :: MetricUnit -> Constraint
class KnownMetricUnit metricUnit where
  metricUnitSing :: Proxy metricUnit -> SMetricUnit metricUnit

instance KnownMetricUnit 'Byte where
  metricUnitSing _proxy = SByte
  {-# INLINE metricUnitSing #-}

instance KnownMetricUnit 'MegaBlock where
  metricUnitSing _proxy = SMegaBlock
  {-# INLINE metricUnitSing #-}

instance KnownMetricUnit 'NanoSecond where
  metricUnitSing _proxy = SNanoSecond
  {-# INLINE metricUnitSing #-}

instance KnownMetricUnit 'Percent where
  metricUnitSing _proxy = SPercent
  {-# INLINE metricUnitSing #-}

--------------------------------------------------------------------------------
-- Metric Types

data SMetricType (a :: Type) where
  SMetricTypeFloat :: SMetricType Float
  SMetricTypeDouble :: SMetricType Double
  SMetricTypeWord :: SMetricType Word
  SMetricTypeWord8 :: SMetricType Word8
  SMetricTypeWord16 :: SMetricType Word16
  SMetricTypeWord32 :: SMetricType Word32
  SMetricTypeWord64 :: SMetricType Word64
  SMetricTypeInt :: SMetricType Int
  SMetricTypeInt8 :: SMetricType Int8
  SMetricTypeInt16 :: SMetricType Int16
  SMetricTypeInt32 :: SMetricType Int32
  SMetricTypeInt64 :: SMetricType Int64

class (Num a) => KnownMetricType a where
  metricTypeSing :: Proxy a -> SMetricType a

instance KnownMetricType Float where
  metricTypeSing :: Proxy Float -> SMetricType Float
  metricTypeSing _proxy = SMetricTypeFloat
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Double where
  metricTypeSing :: Proxy Double -> SMetricType Double
  metricTypeSing _proxy = SMetricTypeDouble
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Word where
  metricTypeSing :: Proxy Word -> SMetricType Word
  metricTypeSing _proxy = SMetricTypeWord
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Word8 where
  metricTypeSing :: Proxy Word8 -> SMetricType Word8
  metricTypeSing _proxy = SMetricTypeWord8
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Word16 where
  metricTypeSing :: Proxy Word16 -> SMetricType Word16
  metricTypeSing _proxy = SMetricTypeWord16
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Word32 where
  metricTypeSing :: Proxy Word32 -> SMetricType Word32
  metricTypeSing _proxy = SMetricTypeWord32
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Word64 where
  metricTypeSing :: Proxy Word64 -> SMetricType Word64
  metricTypeSing _proxy = SMetricTypeWord64
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Int where
  metricTypeSing :: Proxy Int -> SMetricType Int
  metricTypeSing _proxy = SMetricTypeInt
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Int8 where
  metricTypeSing :: Proxy Int8 -> SMetricType Int8
  metricTypeSing _proxy = SMetricTypeInt8
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Int16 where
  metricTypeSing :: Proxy Int16 -> SMetricType Int16
  metricTypeSing _proxy = SMetricTypeInt16
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Int32 where
  metricTypeSing :: Proxy Int32 -> SMetricType Int32
  metricTypeSing _proxy = SMetricTypeInt32
  {-# INLINE metricTypeSing #-}

instance KnownMetricType Int64 where
  metricTypeSing :: Proxy Int64 -> SMetricType Int64
  metricTypeSing _proxy = SMetricTypeInt64
  {-# INLINE metricTypeSing #-}
