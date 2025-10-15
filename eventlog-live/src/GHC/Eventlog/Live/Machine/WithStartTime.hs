{- |
Module      : GHC.Eventlog.Live.Machine.WithStartTime
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.WithStartTime (
  WithStartTime (..),
  setWithStartTime'value,
  tryGetTimeUnixNano,
  withStartTime,
  withStartTime',
  dropStartTime,
) where

import Control.Monad (forever)
import Data.Machine (Is (..), PlanT, Process, await, construct, mapping, yield)
import GHC.RTS.Events (Event (..), EventInfo, Timestamp)
import GHC.RTS.Events qualified as E

-------------------------------------------------------------------------------
-- Start time

{- |
Data decorated with a start time in nanoseconds since the Unix epoch.
-}
data WithStartTime a = WithStartTime
  { value :: !a
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  }
  deriving (Functor, Show)

{- |
Setter for the value of a t`WithStartTime`
-}
setWithStartTime'value :: WithStartTime a -> b -> WithStartTime b
setWithStartTime'value (WithStartTime _a t) b = WithStartTime b t

{- |
If the event has a start time, return `Just` the time of the event in
nanoseconds since the Unix epoch. Otherwise, return `Nothing`.
-}
tryGetTimeUnixNano :: WithStartTime Event -> Maybe Timestamp
tryGetTimeUnixNano i = (i.value.evTime +) <$> i.maybeStartTimeUnixNano

{- |
Wrap every event in t`WithStartTime`. Every event after `E.WallClockTime` will
have its start time field set to `Just` the process start time.

This machine swallows the first and only `E.WallClockTime` event.
-}
withStartTime :: Process Event (WithStartTime Event)
withStartTime = withStartTime' E.evSpec WithStartTime

{- |
Generalised version of `withStartTime` that can be adapted to work on arbitrary
types using a getter and a setter.
-}
withStartTime' :: (a -> EventInfo) -> (a -> Maybe Timestamp -> b) -> Process a b
withStartTime' getEventInfo setStartTime = construct start
 where
  start =
    await >>= \case
      value
        -- The `WallClockTime` event announces the wall-clock time at which the
        -- process was started.
        | E.WallClockTime{..} <- getEventInfo value -> do
            -- This will start overflowing on Sunday, 21 July 2554 23:34:33, UTC.
            let !startTimeNs = sec * 1_000_000_000 + fromIntegral nsec
            -- We do not re-emit the `WallClockTime` event.
            continue startTimeNs
        | otherwise ->
            yield (value `setStartTime` Nothing) >> start
  continue startTimeUnixNano =
    mappingPlan $ \value ->
      value `setStartTime` Just startTimeUnixNano

{- |
Drop the t`WithStartTime` wrapper.
-}
dropStartTime :: Process (WithStartTime a) a
dropStartTime = mapping (.value)

{- |
Internal helper. Variant of `mapping` for plans.
-}
mappingPlan :: (a -> b) -> PlanT (Is a) b m a
mappingPlan f = forever (await >>= \a -> yield (f a))
