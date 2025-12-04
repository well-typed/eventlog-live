{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : GHC.Eventlog.Live.Machine.Analysis.Thread
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Analysis.Thread (
  -- * Thread Analysis

  -- ** Thread Labels
  ThreadLabel (..),
  processThreadLabelData,

  -- ** Thread State Spans
  ThreadState (..),
  showThreadStateCategory,
  threadStateStatus,
  threadStateCap,
  ThreadStateSpan (..),
  processThreadStateSpans,
  processThreadStateSpans',
) where

import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Char (isSpace)
import Data.Machine (Is (..), PlanT, Process, ProcessT, await, construct, repeatedly, yield)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Data.Span (duration)
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Machine.Core (liftRouter)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..), tryGetTimeUnixNano)
import GHC.RTS.Events (Event (..), EventInfo, ThreadId, ThreadStopStatus (..), Timestamp)
import GHC.RTS.Events qualified as E
import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Thread Labels

{- |
The t`ThreadLabel` type represents the association of a label with a thread
starting at a given time.
-}
data ThreadLabel
  = ThreadLabel
  { thread :: !ThreadId
  , threadlabel :: !Text
  , startTimeUnixNano :: !Timestamp
  }

{- |
This machine processes `E.ThreadLabel` events and yields t`ThreadLabel` values.
-}
processThreadLabelData :: Process (WithStartTime Event) ThreadLabel
processThreadLabelData = repeatedly go
 where
  go =
    await >>= \i -> case i.value.evSpec of
      E.ThreadLabel{..}
        | Just startTimeUnixNano <- tryGetTimeUnixNano i ->
            yield ThreadLabel{..}
      _otherwise -> pure ()

-------------------------------------------------------------------------------
-- Thread State Spans

{- |
The execution states of a mutator thread.
-}
data ThreadState
  = Running {cap :: !Int}
  | Blocked {status :: !ThreadStopStatus}
  | Finished
  deriving (Show)

{- |
Pretty-print a thread state as "Running", "Blocked", or "Finished".
-}
showThreadStateCategory :: ThreadState -> Text
showThreadStateCategory = \case
  Running{} -> "Running"
  Blocked{} -> "Blocked"
  Finished{} -> "Finished"

{- |
Get the t`ThreadState` status, if the t`ThreadState` is `Blocked`.
-}
threadStateStatus :: ThreadState -> Maybe ThreadStopStatus
threadStateStatus = \case
  Running{} -> Nothing
  Blocked{status} -> Just status
  Finished{} -> Nothing

{- |
Get the t`ThreadState` capability, if the `ThreadState` is `Running`.
-}
threadStateCap :: ThreadState -> Maybe Int
threadStateCap = \case
  Running{cap} -> Just cap
  Blocked{} -> Nothing
  Finished{} -> Nothing

{- |
A span representing the state of a mutator thread.
-}
data ThreadStateSpan
  = ThreadStateSpan
  { thread :: !ThreadId
  , threadState :: !ThreadState
  , startTimeUnixNano :: !Timestamp
  , endTimeUnixNano :: !Timestamp
  }
  deriving (Show)

{-# SPECIALIZE duration :: ThreadStateSpan -> Timestamp #-}

{- |
This machine processes `E.RunThread` and `E.StopThread` events to produce
t`ThreadStateSpan` values that represent segments of time where a thread is
running, blocked, or finished.

This processor uses the following finite-state automaton:

@
      ┌─(StopThread)─┐
      │              ↓
    ┌→[   Blocked    ]─┐
    │                  │
(StopThread)       (RunThread)
    │                  │
    └─[   Running    ]←┘
      ↑              │
      └─(RunThread)──┘
@

The transitions from @Blocked@ to @Blocked@, @Blocked@ to @Running@, and
@Running@ to @Running@ yield a t`ThreadStateSpan`. There are additional
transitions (not pictured) from either state to the final `Finished` state
with a `E.StopThread` event with the `ThreadFinished` status.
-}
processThreadStateSpans ::
  (Monad m) =>
  Logger m ->
  ProcessT m (WithStartTime Event) ThreadStateSpan
processThreadStateSpans =
  processThreadStateSpans' tryGetTimeUnixNano (.value) (const id)

{- |
Generalised version of `processThreadStateSpans` that can be adapted to work
on arbitrary types using a getter and a lens.
-}
processThreadStateSpans' ::
  forall m s t.
  (Monad m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> ThreadStateSpan -> t) ->
  Logger m ->
  ProcessT m s t
processThreadStateSpans' timeUnixNano getEvent setThreadStateSpan logger =
  liftRouter measure spawn
 where
  getEventTime = (.evTime) . getEvent
  getEventInfo = (.evSpec) . getEvent
  getEventCap = (.evCap) . getEvent

  measure :: s -> Maybe ThreadId
  measure i = case getEventInfo i of
    E.RunThread{thread} -> Just thread
    E.StopThread{thread} -> Just thread
    _otherwise -> Nothing

  spawn :: ThreadId -> ProcessT m s t
  spawn thread = construct $ go Nothing
   where
    go :: Maybe s -> PlanT (Is s) t m Void
    go mi =
      await >>= \case
        j
          -- If the previous event was a `E.StopThread` event, and...
          | Just E.StopThread{status} <- getEventInfo <$> mi
          , --- ...it has the `ThreadFinished` status, then...
            isThreadFinished status ->
              -- ...ignore the current event.
              go mi
          --
          -- If the current event is a `E.RunThread` event, and...
          | E.RunThread{} <- getEventInfo j
          , -- ...the previous event was a `E.StopThread` event, then...
            Just E.StopThread{status} <- getEventInfo <$> mi
          , -- ...gather the end time of the previous event, and...
            Just startTimeUnixNano <- timeUnixNano =<< mi
          , -- ...gather the start time of the current event, and...
            Just endTimeUnixNano <- timeUnixNano j -> do
              -- ...yield a thread state span, and...
              yield . setThreadStateSpan j $
                ThreadStateSpan{threadState = Blocked status, ..}
              go (Just j)
          --
          -- If the current event is a `E.RunThread` event, and...
          | E.RunThread{} <- getEventInfo j
          , -- ...the previous event was a `E.RunThread` event, then...
            Just E.RunThread{} <- getEventInfo <$> mi -> do
              -- ...keep the oldest event.
              go (Just $ maybe j (minBy getEventTime j) mi)
          --
          -- If the current event is a `E.RunThread` event, and...
          | E.RunThread{} <- getEventInfo j
          , -- ...there is no previous event, then...
            isNothing mi ->
              -- ...keep the current event.
              --
              -- The reason for the additional `isNothing` test is because,
              -- otherwise, this case might silently swallow any `E.StopThread`
              -- events for which `timeUnixNano` gives `Nothing`.
              -- By excluding these, they are forwarded to the catch-all case.
              go (Just j)
          --
          -- If the current event is a `E.StopThread` event, and...
          | E.StopThread{} <- getEventInfo j
          , -- ...the previous event was a `E.StopThread` event, then...
            Just E.StopThread{status} <- getEventInfo <$> mi
          , -- ...gather the end time of the previous event, and...
            Just startTimeUnixNano <- timeUnixNano =<< mi
          , -- ...gather the start time of the current event, and...
            Just endTimeUnixNano <- timeUnixNano j -> do
              -- ...yield a thread state span, and...
              yield . setThreadStateSpan j $
                ThreadStateSpan{threadState = Blocked status, ..}
              -- ...keep the current event.
              --
              -- This causes us to adopt every `E.StopThread` event, until
              -- we hit a `E.StopThread` event with the `ThreadFinished`, at
              -- which point the first clause will cause us to stick with it.
              go (Just j)
          --
          -- If the current event is a `E.StopThread` event, and...
          | E.StopThread{} <- getEventInfo j
          , -- ...the previous event was a `E.RunThread` event, then...
            Just E.RunThread{} <- getEventInfo <$> mi
          , -- ...gather the capability of the `E.RunThread` event, and...
            Just cap <- getEventCap =<< mi
          , -- ...gather the end time of the previous event, and...
            Just startTimeUnixNano <- timeUnixNano =<< mi
          , -- ...gather the start time of the current event, and...
            Just endTimeUnixNano <- timeUnixNano j -> do
              -- ...yield a thread state span, and...
              yield . setThreadStateSpan j $
                ThreadStateSpan{threadState = Running cap, ..}
              -- ...keep the current event.
              go (Just j)
          --
          -- If the current event is any other event, then...
          | otherwise -> do
              -- ...emit a warning, and...
              let msg =
                    T.pack $
                      printf
                        "Thread %d: Unexpected event %s\n\
                        \This happens once per running thread when connecting to a running process,\n\
                        \but should not happen multiple times per thread."
                        thread
                        (showEventInfo (getEventInfo j))
              lift $ writeLog logger WARN $ msg

              --
              -- This case may trigger for any event that isn't `E.RunThread`
              -- or `E.StopThread` and for any `E.StopThread` event that comes
              -- before the first `E.RunThread` event. It may also trigger for
              -- any event for which `timeUnixNano` returns `Nothing`.
              --
              -- ...ignore it.
              go mi

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper.
Check whether a t`ThreadStopStatus` is equal to `ThreadFinished`.
This is needed because t`ThreadStopStatus` does not define an `Eq` instance.
-}
isThreadFinished :: ThreadStopStatus -> Bool
isThreadFinished = \case
  ThreadFinished -> True
  _otherwise -> False

{- |
Internal helper.
Show `EventInfo` in a condensed format suitable for logging.
-}
showEventInfo :: EventInfo -> String
showEventInfo = \case
  E.RunThread{thread} -> printf "RunThread{%d}" thread
  E.StopThread{thread, status} -> printf "StopThread{%d,%s}" thread (E.showThreadStopStatus status)
  E.MigrateThread{thread} -> printf "MigrateThread{%d}" thread
  E.StartGC{} -> "StartGC"
  E.EndGC{} -> "EndGC"
  evSpec -> takeWhile (not . isSpace) . show $ evSpec

{- |
Internal helper. Return the minimal value by some projection.
-}
minBy :: (Ord b) => (a -> b) -> a -> a -> a
minBy f x y = if f x < f y then x else y
