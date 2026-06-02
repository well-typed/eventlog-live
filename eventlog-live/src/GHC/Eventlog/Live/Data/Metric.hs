{- |
Module      : GHC.Eventlog.Live.Metric
Description : Representation for metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Metric (
  Metric (..),

  -- * Existential wrapper
  SomeMetric (..),
  SMetricType (..),
  KnownMetricType (..),
) where

import Control.Exception (assert)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.Eventlog.Live.Data.Group (GroupBy (..))
import GHC.RTS.Events (Timestamp)

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

--------------------------------------------------------------------------------
-- Existential wrapper for Metrics
--------------------------------------------------------------------------------

data SomeMetric
  = forall metricType.
  (KnownMetricType metricType) =>
  SomeMetric
  { metricName :: String
  , metric :: Metric metricType
  }

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

instance KnownMetricType Double where
  metricTypeSing :: Proxy Double -> SMetricType Double
  metricTypeSing _proxy = SMetricTypeDouble

instance KnownMetricType Word where
  metricTypeSing :: Proxy Word -> SMetricType Word
  metricTypeSing _proxy = SMetricTypeWord

instance KnownMetricType Word8 where
  metricTypeSing :: Proxy Word8 -> SMetricType Word8
  metricTypeSing _proxy = SMetricTypeWord8

instance KnownMetricType Word16 where
  metricTypeSing :: Proxy Word16 -> SMetricType Word16
  metricTypeSing _proxy = SMetricTypeWord16

instance KnownMetricType Word32 where
  metricTypeSing :: Proxy Word32 -> SMetricType Word32
  metricTypeSing _proxy = SMetricTypeWord32

instance KnownMetricType Word64 where
  metricTypeSing :: Proxy Word64 -> SMetricType Word64
  metricTypeSing _proxy = SMetricTypeWord64

instance KnownMetricType Int where
  metricTypeSing :: Proxy Int -> SMetricType Int
  metricTypeSing _proxy = SMetricTypeInt

instance KnownMetricType Int8 where
  metricTypeSing :: Proxy Int8 -> SMetricType Int8
  metricTypeSing _proxy = SMetricTypeInt8

instance KnownMetricType Int16 where
  metricTypeSing :: Proxy Int16 -> SMetricType Int16
  metricTypeSing _proxy = SMetricTypeInt16

instance KnownMetricType Int32 where
  metricTypeSing :: Proxy Int32 -> SMetricType Int32
  metricTypeSing _proxy = SMetricTypeInt32

instance KnownMetricType Int64 where
  metricTypeSing :: Proxy Int64 -> SMetricType Int64
  metricTypeSing _proxy = SMetricTypeInt64
