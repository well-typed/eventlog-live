{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Machine.WithStartTime
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.WithStartTime (
  NoStartTimeError (..),
  WithStartTime (..),
  setWithStartTime'value,
  getTimeUnixNano,
  withStartTime,
  withStartTime',
  dropStartTime,
) where

import Control.Exception (Exception, throw)
import Control.Monad (forever, unless)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (for_)
import Data.Machine (Is (..), PlanT, Process, ProcessT, await, construct, mapping, yield)
import Data.Text qualified as T
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Exception (Exception (..))
import GHC.RTS.Events (Event (..), EventInfo, Timestamp)
import GHC.RTS.Events qualified as E

-------------------------------------------------------------------------------
-- Start time

{- |
Data decorated with a start time in nanoseconds since the Unix epoch.
-}
data WithStartTime a = WithStartTime
  { value :: !a
  , startTimeUnixNano :: !Timestamp
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
getTimeUnixNano :: WithStartTime Event -> Timestamp
getTimeUnixNano i = i.value.evTime + i.startTimeUnixNano

{- |
The exception thrown when a downstream user inspects the start time of an init event.
-}
data NoStartTimeError = NoStartTimeError
  deriving (Show)

instance Exception NoStartTimeError where
  displayException :: NoStartTimeError -> String
  displayException NoStartTimeError = "The code inspected the start time of an init event."

{- |
Wrap every event in t`WithStartTime`. Every event after `E.WallClockTime` will
have its start time field set to `Just` the process start time.

This machine swallows the first and only `E.WallClockTime` event.
-}
withStartTime ::
  (Monad m) =>
  Logger m ->
  ProcessT m Event (WithStartTime Event)
withStartTime = withStartTime' (const False) E.evSpec WithStartTime

{- |
Generalised version of `withStartTime` that can be adapted to work on arbitrary
types using a getter and a setter.

The first argument is a predicate that determines whether an event should have
a guaranteed start time. If an event precedes the `E.WallClockTime` event and
the predicate is true, it will be buffered until the `E.WallClockTime` event is
received. Otherwise, it will be yielded with a `startTimeUnixNano` field that
throws a `NoStartTimeError` exception when evaluated.
-}
withStartTime' ::
  (Monad m) =>
  (EventInfo -> Bool) ->
  (a -> EventInfo) ->
  (a -> Timestamp -> b) ->
  Logger m ->
  ProcessT m a b
withStartTime' shouldBuffer getEventInfo setStartTime logger = construct $ start []
 where
  start buffer =
    await >>= \value -> case getEventInfo value of
      -- The `WallClockTime` event announces the wall-clock time at which the
      -- process was started.
      E.WallClockTime{..} -> do
        -- This will start overflowing on Sunday, 21 July 2554 23:34:33, UTC.
        let !startTimeUnixNano = sec * 1_000_000_000 + fromIntegral nsec
        -- Yield all buffered events.
        for_ (reverse buffer) $ \value' ->
          yield (value' `setStartTime` startTimeUnixNano)
        -- We do not re-emit the `WallClockTime` event.
        continue startTimeUnixNano
      eventInfo -> do
        -- If the event is not expected before the WallClockTime event,
        -- log a warning...
        unless (isExpectedBeforeWallClockTime eventInfo) $
          lift . writeLog logger WARN $
            "Received unexpected event type before WallClockTime event: " <> T.pack (show eventInfo)
        -- If the user would like the event to be buffered, then...
        if shouldBuffer eventInfo
          -- ...add it to the buffer, and continue.
          then start (value : buffer)
          -- ...otherwise, yield it without start time, and continue.
          else yield (value `setStartTime` throw NoStartTimeError) >> start buffer

  continue startTimeUnixNano =
    mappingPlan $ \value ->
      value `setStartTime` startTimeUnixNano

{- |
Test whether or not an event is expected to occur before the `E.WallClockTime` event.
-}
isExpectedBeforeWallClockTime :: E.EventInfo -> Bool
isExpectedBeforeWallClockTime = \case
  E.CapsetCreate{} -> True
  E.CapCreate{} -> True
  E.CapsetAssignCap{} -> True
  E.InfoTableProv{} -> True
  E.HeapInfoGHC{} -> True
  E.OsProcessPid{} -> True
  E.OsProcessParentPid{} -> True
  E.RtsIdentifier{} -> True
  E.ProgramArgs{} -> True
  _ -> False

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
