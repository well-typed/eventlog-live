{- |
Module      : GHC.Eventlog.Live.Metric
Description : Representation for metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Metric (
  Metric (..),
) where

import GHC.Eventlog.Live.Data.Attribute (Attrs)
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
