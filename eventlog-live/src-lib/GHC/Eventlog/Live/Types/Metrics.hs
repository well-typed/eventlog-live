{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Metric
Description : Representation for OTLP metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Types.Metrics (
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
import Data.Aeson (KeyValue (..))
import Data.Aeson.Types (Encoding, KeyValueOmit (..), ToJSON (..), Value (..), pairs)
import Data.Coerce (coerce)
import Data.Default (Default)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Eventlog.Live.Config
import GHC.Eventlog.Live.Types.Attribute (Attrs)
import GHC.Eventlog.Live.Types.Group (GroupBy (..))
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

--------------------------------------------------------------------------------
-- Known Metrics
--------------------------------------------------------------------------------

type SomeMetrics :: Type
data SomeMetrics
  = forall metric. (KnownMetric metric) => SomeMetrics !(Proxy metric) [Metric (GetMetricType metric)]

instance ToJSON SomeMetrics where
  toJSON :: SomeMetrics -> Value
  toJSON = Object . someMetricsToKV

  toEncoding :: SomeMetrics -> Encoding
  toEncoding = pairs . someMetricsToKV

  omitField :: SomeMetrics -> Bool
  omitField (SomeMetrics _metric metrics) = null metrics

someMetricsToKV :: (KeyValueOmit e kv, Monoid kv) => SomeMetrics -> kv
someMetricsToKV (SomeMetrics (metric :: Proxy metric) metrics) =
  mconcat $
    [ "name" .= symbolVal metric
    , "metrics" .?= coerce @_ @[Metric (KnownMetricValue (GetMetricType metric))] metrics
    -- "metrics" is the unique property that identifies this type.
    ]
{-# INLINE someMetricsToKV #-}

type KnownMetric :: Symbol -> Constraint
class
  ( HasField metric Metrics (Maybe (GetMetricConf metric))
  , IsMetricProcessorConfig (GetMetricConf metric)
  , Show (GetMetricConf metric)
  , Default (GetMetricConf metric)
  , KnownSymbol metric
  , KnownMetricType (GetMetricType metric)
  , KnownMetricKind (GetMetricKind metric)
  , KnownMetricUnit (GetMetricUnit metric)
  ) =>
  KnownMetric metric
  where
  type GetMetricConf metric :: Type
  type GetMetricType metric :: Type
  type GetMetricUnit metric :: MetricUnit
  type GetMetricKind metric :: MetricKind

metricConfig :: forall metric. (KnownMetric metric) => Proxy metric -> Metrics -> Maybe (GetMetricConf metric)
metricConfig (_metric :: Proxy metric) = getField @metric
{-# INLINE metricConfig #-}

-- NOTE: This should be kept in sync with the list of metrics.
--       Specifically, there should be a `KnownMetric` instance for every metric.

instance KnownMetric "heapAllocated" where
  type GetMetricConf "heapAllocated" = HeapAllocatedMetric
  type GetMetricType "heapAllocated" = Word64
  type GetMetricUnit "heapAllocated" = 'Byte
  type GetMetricKind "heapAllocated" = 'Sum 'Cumulative 'Monotonic

instance KnownMetric "heapSize" where
  type GetMetricConf "heapSize" = HeapSizeMetric
  type GetMetricType "heapSize" = Word64
  type GetMetricUnit "heapSize" = 'Byte
  type GetMetricKind "heapSize" = 'Gauge

instance KnownMetric "blocksSize" where
  type GetMetricConf "blocksSize" = BlocksSizeMetric
  type GetMetricType "blocksSize" = Word64
  type GetMetricUnit "blocksSize" = 'Byte
  type GetMetricKind "blocksSize" = 'Gauge

instance KnownMetric "heapLive" where
  type GetMetricConf "heapLive" = HeapLiveMetric
  type GetMetricType "heapLive" = Word64
  type GetMetricUnit "heapLive" = 'Byte
  type GetMetricKind "heapLive" = 'Gauge

instance KnownMetric "memCurrent" where
  type GetMetricConf "memCurrent" = MemCurrentMetric
  type GetMetricType "memCurrent" = Word32
  type GetMetricUnit "memCurrent" = 'MegaBlock
  type GetMetricKind "memCurrent" = 'Gauge

instance KnownMetric "memNeeded" where
  type GetMetricConf "memNeeded" = MemNeededMetric
  type GetMetricType "memNeeded" = Word32
  type GetMetricUnit "memNeeded" = 'MegaBlock
  type GetMetricKind "memNeeded" = 'Gauge

instance KnownMetric "memReturned" where
  type GetMetricConf "memReturned" = MemReturnedMetric
  type GetMetricType "memReturned" = Word32
  type GetMetricUnit "memReturned" = 'MegaBlock
  type GetMetricKind "memReturned" = 'Gauge

instance KnownMetric "gcCopied" where
  type GetMetricConf "gcCopied" = GcCopiedMetric
  type GetMetricType "gcCopied" = Word64
  type GetMetricUnit "gcCopied" = 'Byte
  type GetMetricKind "gcCopied" = 'Gauge

instance KnownMetric "gcSlop" where
  type GetMetricConf "gcSlop" = GcSlopMetric
  type GetMetricType "gcSlop" = Word64
  type GetMetricUnit "gcSlop" = 'Byte
  type GetMetricKind "gcSlop" = 'Gauge

instance KnownMetric "gcFragmentation" where
  type GetMetricConf "gcFragmentation" = GcFragmentationMetric
  type GetMetricType "gcFragmentation" = Word64
  type GetMetricUnit "gcFragmentation" = 'Byte
  type GetMetricKind "gcFragmentation" = 'Gauge

instance KnownMetric "heapProfSample" where
  type GetMetricConf "heapProfSample" = HeapProfSampleMetric
  type GetMetricType "heapProfSample" = Word64
  type GetMetricUnit "heapProfSample" = 'Byte
  type GetMetricKind "heapProfSample" = 'Gauge

instance KnownMetric "capabilityUsage" where
  type GetMetricConf "capabilityUsage" = CapabilityUsageMetric
  type GetMetricType "capabilityUsage" = Timestamp
  type GetMetricUnit "capabilityUsage" = 'NanoSecond
  type GetMetricKind "capabilityUsage" = 'Sum 'Cumulative 'Monotonic

instance KnownMetric "productivity" where
  type GetMetricConf "productivity" = ProductivityMetric
  type GetMetricType "productivity" = Double
  type GetMetricUnit "productivity" = 'Percent
  type GetMetricKind "productivity" = 'Gauge

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

instance (ToJSON a) => ToJSON (Metric a) where
  toJSON :: Metric a -> Value
  toJSON = Object . metricToKV

  toEncoding :: Metric a -> Encoding
  toEncoding = pairs . metricToKV

metricToKV :: (KeyValueOmit e kv, Monoid kv, ToJSON a) => Metric a -> kv
metricToKV m =
  mconcat $
    [ "value" .= m.value
    , "time_unix_nano" .?= m.maybeTimeUnixNano
    , "start_time_unix_nano" .?= m.maybeStartTimeUnixNano
    , "attrs" .?= m.attrs
    ]
{-# INLINE metricToKV #-}

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

newtype KnownMetricValue a = KnownMetricValue a

instance (KnownMetricType a) => ToJSON (KnownMetricValue a) where
  toJSON :: KnownMetricValue a -> Value
  toJSON =
    case metricTypeSing (Proxy @a) of
      SMetricTypeFloat -> toJSON
      SMetricTypeDouble -> toJSON
      SMetricTypeWord -> toJSON
      SMetricTypeWord8 -> toJSON
      SMetricTypeWord16 -> toJSON
      SMetricTypeWord32 -> toJSON
      SMetricTypeWord64 -> toJSON
      SMetricTypeInt -> toJSON
      SMetricTypeInt8 -> toJSON
      SMetricTypeInt16 -> toJSON
      SMetricTypeInt32 -> toJSON
      SMetricTypeInt64 -> toJSON

  toEncoding :: KnownMetricValue a -> Encoding
  toEncoding =
    case metricTypeSing (Proxy @a) of
      SMetricTypeFloat -> toEncoding
      SMetricTypeDouble -> toEncoding
      SMetricTypeWord -> toEncoding
      SMetricTypeWord8 -> toEncoding
      SMetricTypeWord16 -> toEncoding
      SMetricTypeWord32 -> toEncoding
      SMetricTypeWord64 -> toEncoding
      SMetricTypeInt -> toEncoding
      SMetricTypeInt8 -> toEncoding
      SMetricTypeInt16 -> toEncoding
      SMetricTypeInt32 -> toEncoding
      SMetricTypeInt64 -> toEncoding
