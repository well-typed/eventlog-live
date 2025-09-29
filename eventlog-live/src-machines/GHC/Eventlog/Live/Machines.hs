{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : GHC.Eventlog.Live.Machines
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machines (
  -- * Eventlog source
  sourceHandleWait,
  sourceHandleBatch,
  defaultChunkSizeBytes,

  -- * Eventlog file sink
  fileSink,
  fileSinkBatch,

  -- * Event decoding
  decodeEvent,
  decodeEventBatch,

  -- * Event processing

  -- ** Start time
  WithStartTime (..),
  setWithStartTime'value,
  tryGetTimeUnixNano,
  withStartTime,
  withStartTime',
  dropStartTime,

  -- ** Main thread ID
  WithMainThreadId (..),
  withMainThreadId,
  withMainThreadId',
  dropMainThreadId,

  -- ** Capability Usage

  -- *** Capability Usage Metrics
  processCapabilityUsageMetrics,

  -- *** Capability Usage Spans
  CapabilityUsageSpan,
  CapabilityUser (..),
  capabilityUser,
  showCapabilityUserCategory,
  processCapabilityUsageSpans,
  processCapabilityUsageSpans',

  -- *** GC Spans
  GCSpan (..),
  processGCSpans,
  processGCSpans',

  -- *** Mutator Spans
  MutatorSpan (..),
  asMutatorSpans,
  asMutatorSpans',
  processMutatorSpans,
  processMutatorSpans',

  -- ** Thread labels
  ThreadLabel (..),
  processThreadLabels,

  -- ** Thread State Spans
  ThreadState (..),
  showThreadStateCategory,
  threadStateStatus,
  threadStateCap,
  ThreadStateSpan (..),
  processThreadStateSpans,
  processThreadStateSpans',

  -- ** Heap events
  processHeapAllocatedData,
  processHeapSizeData,
  processBlocksSizeData,
  processHeapLiveData,
  MemReturnData (..),
  processMemReturnData,
  processHeapProfSampleData,

  -- * Ticks
  Tick (..),
  batchByTick,
  batchToTick,
  batchListToTick,
  batchByTickList,
  liftTick,
  dropTick,
  onlyTick,
  liftBatch,

  -- * Event sorting
  sortByBatch,
  sortByBatchTick,

  -- * Delimiting
  between,
  delimit,

  -- * Heap profile breakdown
  heapProfBreakdownEitherReader,
  heapProfBreakdownShow,
) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.DList qualified as D
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Function (fix, on)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable (..))
import Data.List qualified as L
import Data.Machine (Is (..), MachineT (..), Moore (..), PlanT, Process, ProcessT, Step (..), asParts, await, construct, encased, mapping, repeatedly, starve, stopped, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Machine.Mealy (unfoldMealy)
import Data.Machine.Process (Automaton (..))
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Semigroup (Max (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Eventlog.Live.Data.Attribute (Attr, AttrValue, IsAttrValue (..), (~=))
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Data.Span (duration)
import GHC.Eventlog.Live.Internal.Logger (logWarning)
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.RTS.Events (Event (..), EventInfo, HeapProfBreakdown (..), ThreadId, ThreadStopStatus (..), Timestamp)
import GHC.RTS.Events qualified as E
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import GHC.Records (HasField (..))
import Numeric (showHex)
import System.IO (Handle, hWaitForInput)
import System.IO.Error (isEOFError)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Read.Lex (readHexP)

-------------------------------------------------------------------------------
-- Event processing
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Main thread ID

{- |
Data decorated with a main thread ID.
-}
data WithMainThreadId a = WithMainThreadId
  { value :: !a
  , maybeMainThreadId :: !(Maybe ThreadId)
  }
  deriving (Functor, Show)

{- |
Wrap every event in t`WithMainThreadId`. Every event after the first `E.RunThread`
event will have its main thread ID set to `Just` the main thread ID.
-}
withMainThreadId :: Process Event (WithMainThreadId Event)
withMainThreadId = withMainThreadId' E.evSpec WithMainThreadId

{- |
Generalised version of `withMainThreadId` that can be adapted to work on arbitrary
types using a getter and a setter.
-}
withMainThreadId' :: (a -> EventInfo) -> (a -> Maybe ThreadId -> b) -> Process a b
withMainThreadId' getEventInfo setMainThreadId = construct start
 where
  start =
    await >>= \case
      value
        | E.RunThread{thread} <- getEventInfo value ->
            yield (value `setMainThreadId` Just thread) >> continue thread
        | otherwise ->
            yield (value `setMainThreadId` Nothing) >> start
  continue thread =
    mappingPlan $ \value ->
      value `setMainThreadId` Just thread

{- |
Drop the t`WithMainThreadId` wrapper.
-}
dropMainThreadId :: Process (WithMainThreadId a) a
dropMainThreadId = mapping (.value)

-------------------------------------------------------------------------------
-- Capability Usage

-------------------------------------------------------------------------------
-- Capability Usage Metrics

{- |
This machine processes t`CapabilityUsageSpan` spans and produces metrics that
contain the duration and category of each such span and each idle period in
between.
-}
processCapabilityUsageMetrics ::
  forall m.
  (MonadIO m) =>
  ProcessT m (WithStartTime CapabilityUsageSpan) (Metric Timestamp)
processCapabilityUsageMetrics =
  liftRouter measure spawn
 where
  measure :: WithStartTime CapabilityUsageSpan -> Maybe Int
  measure = Just . (.value.cap)

  spawn :: Int -> ProcessT m (WithStartTime CapabilityUsageSpan) (Metric Timestamp)
  spawn cap = construct $ go Nothing
   where
    go ::
      Maybe CapabilityUsageSpan ->
      PlanT (Is (WithStartTime CapabilityUsageSpan)) (Metric Timestamp) m Void
    go mi =
      await >>= \j -> do
        -- If there is a previous span, and...
        for_ mi $ \i ->
          -- ...the end time of the previous span precedes the start time of the current span, then...
          when (i.endTimeUnixNano < j.value.startTimeUnixNano) $
            -- ...yield an idle duration metric.
            yield
              Metric
                { value = j.value.startTimeUnixNano - i.endTimeUnixNano
                , maybeTimeUnixNano = Just i.endTimeUnixNano
                , maybeStartTimeUnixNano = j.maybeStartTimeUnixNano
                , attr = ["cap" ~= cap, "category" ~= ("Idle" :: Text)]
                }
        -- Yield a duration metric for the current span.
        let user = capabilityUser j.value
        yield
          Metric
            { value = duration j.value
            , maybeTimeUnixNano = Just j.value.startTimeUnixNano
            , maybeStartTimeUnixNano = j.maybeStartTimeUnixNano
            , attr = ["cap" ~= cap, "category" ~= showCapabilityUserCategory user, "user" ~= user]
            }
        go (Just j.value)

{- |
The type of process using a capability,
which is either a mutator thread or garbage collection.
-}
data CapabilityUser
  = GC
  | Mutator {thread :: !ThreadId}

instance Show CapabilityUser where
  show :: CapabilityUser -> String
  show = \case
    GC -> "GC"
    Mutator{thread} -> show thread

instance IsAttrValue CapabilityUser where
  toAttrValue :: CapabilityUser -> AttrValue
  toAttrValue = toAttrValue . show
  {-# INLINE toAttrValue #-}

{- |
Get the t`CapabilityUser` associated with a t`CapabilityUsageSpan`.
-}
capabilityUser :: CapabilityUsageSpan -> CapabilityUser
capabilityUser = either (const GC) (Mutator . (.thread))

{- |
Show the category of a `CapabilityUser` as either @"GC"@ or @"Mutator"@.
-}
showCapabilityUserCategory :: CapabilityUser -> Text
showCapabilityUserCategory = \case
  GC{} -> "GC"
  Mutator{} -> "Mutator"

-------------------------------------------------------------------------------
-- Capability Usage Spans

{- |
A t`CapabilityUsageSpan` is either a t`GCSpan` or a t`MutatorSpan`.
-}
type CapabilityUsageSpan = Either GCSpan MutatorSpan

instance HasField "startTimeUnixNano" CapabilityUsageSpan Timestamp where
  getField :: CapabilityUsageSpan -> Timestamp
  getField = either (.startTimeUnixNano) (.startTimeUnixNano)

instance HasField "endTimeUnixNano" CapabilityUsageSpan Timestamp where
  getField :: CapabilityUsageSpan -> Timestamp
  getField = either (.endTimeUnixNano) (.endTimeUnixNano)

instance HasField "cap" CapabilityUsageSpan Int where
  getField :: CapabilityUsageSpan -> Int
  getField = either (.cap) (.cap)

{-# SPECIALIZE duration :: CapabilityUsageSpan -> Timestamp #-}

{- |
This machine runs `processGCSpans` and `processMutatorSpans` in parallel and
combines their output.

This is effectively a fanout of `processGCSpans` and `processMutatorSpans`, the
latter of which runs `processThreadStateSpans` internally. If you are running
`processThreadStateSpans` as well, then using `asMutatorSpans` and constructing
the fanout yourself is more efficient.
-}
processCapabilityUsageSpans ::
  forall m.
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (WithStartTime Event) (WithStartTime CapabilityUsageSpan)
processCapabilityUsageSpans verbosity =
  processCapabilityUsageSpans' tryGetTimeUnixNano (.value) setWithStartTime'value setWithStartTime'value verbosity
    ~> mapping (either (fmap Left) (fmap Right))

{- |
Generalised version of `processCapabilityUsageSpans` that can be adapted to
work on arbitrary types using a getter and a pair of lenses.
-}
processCapabilityUsageSpans' ::
  forall m s t1 t2.
  (MonadIO m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> GCSpan -> t1) ->
  (s -> MutatorSpan -> t2) ->
  Verbosity ->
  ProcessT m s (Either t1 t2)
processCapabilityUsageSpans' timeUnixNano getEvent setGCSpan setMutatorSpan verbosity =
  -- NOTE:
  -- Combining this fanout with an `Either` is risky, because it
  -- has the potential to lose information if both `processGCSpans`
  -- and `processMutatorSpans` yield a value for the same input.
  -- However, this shouldn't ever happen, since the two processors
  -- process disjoint sets of events.
  fanout
    [ processGCSpans' timeUnixNano getEvent setGCSpan verbosity
        ~> mapping Left
    , processMutatorSpans' timeUnixNano getEvent setMutatorSpan verbosity
        ~> mapping Right
    ]

-------------------------------------------------------------------------------
-- GC spans

{- |
A t`GCSpan` represents a segment of time during which the specified capability
ran GC.
-}
data GCSpan = GCSpan
  { cap :: !Int
  , startTimeUnixNano :: !Timestamp
  , endTimeUnixNano :: !Timestamp
  }
  deriving (Show)

{-# SPECIALIZE duration :: GCSpan -> Timestamp #-}

{- |
This machine processes `E.StartGC` and `E.EndGC` events to produce t`GCSpan`
values that represent the segments of time a capability spent in GC.

This processor uses the following finite-state automaton:

@
      ┌─(EndGC)───┐
      │           ↓
    ┌→[   Idle    ]─┐
    │               │
(EndGC)         (StartGC)
    │               │
    └─[    GC     ]←┘
      ↑           │
      └─(StartGC)─┘
@

The transition from @GC@ to @Idle@ yields a GC span.
-}
processGCSpans ::
  forall m.
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (WithStartTime Event) (WithStartTime GCSpan)
processGCSpans =
  processGCSpans' tryGetTimeUnixNano (.value) setWithStartTime'value

{- |
Generalised version of `processGCSpans` that can be adapted to work on
arbitrary types using a getter and a lens.
-}
processGCSpans' ::
  forall m s t.
  (MonadIO m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> GCSpan -> t) ->
  Verbosity ->
  ProcessT m s t
processGCSpans' timeUnixNano getEvent setGCSpan verbosity =
  liftRouter measure spawn
 where
  getEventTime = (.evTime) . getEvent
  getEventInfo = (.evSpec) . getEvent
  getEventCap = (.evCap) . getEvent

  measure :: s -> Maybe Int
  measure i
    | accept (getEventInfo i) = getEventCap i
    | otherwise = Nothing
   where
    accept E.StartGC{} = True
    accept E.EndGC{} = True
    accept _ = False

  -- TODO: Rewrite using `MealyT`
  spawn :: Int -> ProcessT m s t
  spawn cap = construct $ go Nothing
   where
    -- The "mi" variable tracks the previous event for this capability, which
    -- is either `Nothing` or `Just` a `StartGC` or a `EndGC` event.
    go :: Maybe s -> PlanT (Is s) t m Void
    go mi =
      -- We start by awaiting the next event "j"...
      await >>= \j -> case getEventInfo j of
        -- If the next event is a `RunThread` event, and...
        E.StartGC{} -> case mi of
          Just i
            -- If the previous event was a `StartGC` event, then...
            | E.StartGC{} <- getEventInfo i ->
                -- ...continue with the oldest event.
                go (Just $ minBy getEventTime i j)
            -- If the previous event was a `EndGC` event, then...
            | E.EndGC{} <- getEventInfo i ->
                -- ...continue with the current event.
                go (Just j)
            -- If the previous event was any other event, then...
            | otherwise -> do
                -- ...emit an error, and...
                logWarning verbosity "processGCSpans" . T.pack $
                  printf
                    "Capability %d: Unsupported trace %s --> %s"
                    cap
                    (showEventInfo (getEventInfo i))
                    (showEventInfo (getEventInfo j))
                -- ...continue with the previous event.
                go (Just i)
          -- If there was no previous event, then...
          Nothing ->
            -- ...continue with the current event.
            go (Just j)
        -- If the next event is a `StopThread` event...
        E.EndGC{} -> case mi of
          Just i
            -- If the previous event was a `StartGC` event, then...
            | E.StartGC{} <- getEventInfo i
            , Just startTimeUnixNano <- timeUnixNano i
            , Just endTimeUnixNano <- timeUnixNano j -> do
                -- ...yield a GC span, and...
                yield . setGCSpan j $ GCSpan{..}
                -- ...continue with the current event.
                go (Just j)
            -- If the previous event was a `EndGC` event, then...
            | E.EndGC{} <- getEventInfo i ->
                -- ...continue with the oldest event.
                go (Just $ minBy getEventTime i j)
          -- If there was no previous event or it was any other event, then...
          _otherwise -> do
            -- ...emit an error, and...
            logWarning verbosity "processGCSpans" . T.pack $
              printf
                "Capability %d: Unsupported trace %s --> %s"
                cap
                (maybe "?" (showEventInfo . getEventInfo) mi)
                (showEventInfo (getEventInfo j))
            -- ...continue with the previous event.
            go mi
        -- If the next event is any other event, ignore it.
        _otherwise -> go mi

-------------------------------------------------------------------------------
-- Mutator spans

{- |
A t`MutatorSpan` represents a segment of time during which the specified
capability ran the specified mutator thread.
-}
data MutatorSpan = MutatorSpan
  { cap :: !Int
  , thread :: !ThreadId
  , startTimeUnixNano :: !Timestamp
  , endTimeUnixNano :: !Timestamp
  }
  deriving (Show)

{-# SPECIALIZE duration :: MutatorSpan -> Timestamp #-}

{- |
This machine processes `E.RunThread` and `E.StopThread` events to produce
t`MutatorSpan` values that represent the segments of time a capability spent
executating a mutator.

This processor uses the following finite-state automaton:

@
      ┌─(StopThread[X])─┐
      │                 ↓
    ┌→[      Idle       ]─┐
    │                     │
(StopThread[X])       (RunThread[X])
    │                     │
    └─[   Mutator[X]    ]←┘
      ↑                 │
      └─(RunThread[X])──┘
@

The transition from @Mutator[X]@ to @Idle@ yields a t`MutatorSpan`.
While in the @Mutator[X]@ state, any @RunThread[Y]@ or @StopThread[Y]@ events result in an error.
Furthermore, when a @StopThread[X]@ event with the @ThreadFinished@ status is processed,
the thread @X@ is added to a set of finished threads,
and any further @RunThread[X]@ events for that thread are ignored.
This is done because the GHC RTS frequently emits a @RunThread[X]@ event
immediately after a @StopThread[X]@ event with the @ThreadFinished@ status.

This runs `processThreadStateSpans` internally. If you are also running
`processThreadStateSpans`, then post-composing it with `asMutatorSpans`
is more efficient.
-}
processMutatorSpans ::
  forall m.
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (WithStartTime Event) (WithStartTime MutatorSpan)
processMutatorSpans =
  processMutatorSpans' tryGetTimeUnixNano (.value) setWithStartTime'value

{- |
Generalised version of `processMutatorSpans` that can be adapted to work on
arbitrary types using a getter and a lens.
-}
processMutatorSpans' ::
  forall m s t.
  (MonadIO m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> MutatorSpan -> t) ->
  Verbosity ->
  ProcessT m s t
processMutatorSpans' timeUnixNano getEvent setMutatorSpan verbosity =
  processThreadStateSpans' timeUnixNano getEvent setThreadStateSpan verbosity ~> asParts
 where
  setThreadStateSpan :: s -> ThreadStateSpan -> Maybe t
  setThreadStateSpan s threadStateSpan =
    setMutatorSpan s <$> threadStateSpanToMutatorSpan threadStateSpan

{- |
This machine converts any `Running` t`ThreadStateSpan` to a t`MutatorSpan`.
-}
asMutatorSpans ::
  forall m.
  (MonadIO m) =>
  ProcessT m ThreadStateSpan MutatorSpan
asMutatorSpans = asMutatorSpans' id (const id)

{- |
Generalised version of `asMutatorSpans` that can be adapted to work on
arbitrary types using a getter and a lens.
-}
asMutatorSpans' ::
  forall m s t.
  (MonadIO m) =>
  (s -> ThreadStateSpan) ->
  (s -> MutatorSpan -> t) ->
  ProcessT m s t
asMutatorSpans' getThreadStateSpan setMutatorSpan = repeatedly go
 where
  go =
    await >>= \s -> do
      let threadStateSpan = getThreadStateSpan s
      let maybeMutatorSpan = threadStateSpanToMutatorSpan threadStateSpan
      for_ maybeMutatorSpan $ yield . setMutatorSpan s

{- |
Convert the `Running` t`ThreadStateSpan` to `Just` a t`MutatorSpan`.
-}
threadStateSpanToMutatorSpan :: ThreadStateSpan -> Maybe MutatorSpan
threadStateSpanToMutatorSpan ThreadStateSpan{..} =
  case threadState of
    Running{..} -> Just MutatorSpan{..}
    _otherwise -> Nothing

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
processThreadLabels :: Process (WithStartTime Event) ThreadLabel
processThreadLabels = repeatedly go
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
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (WithStartTime Event) ThreadStateSpan
processThreadStateSpans =
  processThreadStateSpans' tryGetTimeUnixNano (.value) (const id)

{- |
Generalised version of `processThreadStateSpans` that can be adapted to work
on arbitrary types using a getter and a lens.
-}
processThreadStateSpans' ::
  forall m s t.
  (MonadIO m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> ThreadStateSpan -> t) ->
  Verbosity ->
  ProcessT m s t
processThreadStateSpans' timeUnixNano getEvent setThreadStateSpan verbosity =
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
              -- ...emit an error, and...
              logWarning verbosity "processThreadStateSpans" . T.pack $
                printf
                  "Thread %d: Unexpected event %s"
                  thread
                  (showEventInfo (getEventInfo j))
              --
              -- This case may trigger for any event that isn't `E.RunThread`
              -- or `E.StopThread` and for any `E.StopThread` event that comes
              -- before the first `E.RunThread` event. It may also trigger for
              -- any event for which `timeUnixNano` returns `Nothing`.
              --
              -- ...ignore it.
              go mi

-------------------------------------------------------------------------------
-- Heap events
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HeapAllocated

{- |
This machine processes `E.HeapAllocated` events into metrics.
-}
processHeapAllocatedData :: Process (WithStartTime Event) (Metric Word64)
processHeapAllocatedData =
  repeatedly $
    await >>= \case
      i
        | E.HeapAllocated{..} <- i.value.evSpec ->
            yield $
              metric i allocBytes $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- HeapSize

{- |
This machine processes `E.HeapSize` events into metrics.
-}
processHeapSizeData :: Process (WithStartTime Event) (Metric Word64)
processHeapSizeData = repeatedly go
 where
  go =
    await >>= \case
      i
        | E.HeapSize{..} <- i.value.evSpec -> do
            yield $
              metric i sizeBytes $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- BlocksSize

{- |
This machine processes `E.BlocksSize` events into metrics.
-}
processBlocksSizeData :: Process (WithStartTime Event) (Metric Word64)
processBlocksSizeData =
  repeatedly $
    await >>= \case
      i
        | E.BlocksSize{..} <- i.value.evSpec -> do
            yield $
              metric i blocksSize $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- HeapLive

{- |
This machine processes `E.HeapLive` events into metrics.
-}
processHeapLiveData :: Process (WithStartTime Event) (Metric Word64)
processHeapLiveData =
  repeatedly $
    await >>= \case
      i
        | E.HeapLive{..} <- i.value.evSpec -> do
            yield $
              metric i liveBytes $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- MemReturn

{- |
The type of data associated with a `E.MemReturn` event.
-}
data MemReturnData = MemReturnData
  { current :: !Word32
  -- ^ The number of megablocks currently allocated.
  , needed :: !Word32
  -- ^ The number of megablocks currently needed.
  , returned :: !Word32
  -- ^ The number of megablocks currently being returned to the OS.
  }

{- |
This machine processes `E.MemReturn` events into metrics.
-}
processMemReturnData :: Process (WithStartTime Event) (Metric MemReturnData)
processMemReturnData =
  repeatedly $
    await >>= \case
      i
        | E.MemReturn{..} <- i.value.evSpec -> do
            yield $
              metric i MemReturnData{..} $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- HeapProfSample

{- |
Internal helper.
The type of info table pointers.
-}
newtype InfoTablePtr = InfoTablePtr Word64
  deriving newtype (Eq, Hashable, Ord)

instance Show InfoTablePtr where
  showsPrec :: Int -> InfoTablePtr -> ShowS
  showsPrec _ (InfoTablePtr ptr) =
    showString "0x" . showHex ptr

instance Read InfoTablePtr where
  readsPrec :: Int -> ReadS InfoTablePtr
  readsPrec _ = readP_to_S (InfoTablePtr <$> (P.string "0x" *> readHexP))

{- |
Internal helper.
The type of an info table entry, as produced by the `E.InfoTableProv` event.
-}
data InfoTable = InfoTable
  { infoTablePtr :: InfoTablePtr
  , infoTableName :: Text
  , infoTableClosureDesc :: Int
  , infoTableTyDesc :: Text
  , infoTableLabel :: Text
  , infoTableModule :: Text
  , infoTableSrcLoc :: Text
  }
  deriving (Show)

{- |
Internal helper.
The type of the state kept by `processHeapProfSampleData`.
-}
data HeapProfSampleState = HeapProfSampleState
  { eitherShouldWarnOrHeapProfBreakdown :: Either Bool HeapProfBreakdown
  , infoTableMap :: HashMap InfoTablePtr InfoTable
  , heapProfSampleEraStack :: [Word64]
  }
  deriving (Show)

{- |
Internal helper.
Decides whether or not `processHeapProfSampleData` should track info tables.
We track info tables until (1) we learn that the RTS is not run with @-hi@,
or (2) we see the first heap profiling sample and don't yet know for sure
that the RTS is run with @-hi@.
-}
shouldTrackInfoTableMap :: Either Bool HeapProfBreakdown -> Bool
shouldTrackInfoTableMap (Left _shouldWarn) = True
shouldTrackInfoTableMap (Right HeapProfBreakdownInfoTable) = True
shouldTrackInfoTableMap _ = False

{- |
Internal helper.
Checks whether a `HeapProfBreakdown` is `HeapProfBreakdownInfoTable`.
This is needed because the ghc-events package does not define an `Eq`
instance for the `HeapProfBreakdown` type.
-}
isHeapProfBreakdownInfoTable :: HeapProfBreakdown -> Bool
isHeapProfBreakdownInfoTable HeapProfBreakdownInfoTable = True
isHeapProfBreakdownInfoTable _ = False

{- |
This machine processes `E.HeapProfSampleString` events into metrics.
Furthermore, it processes the `E.HeapProfBegin` and `E.ProgramArgs` events
to determine the heap profile breakdown, processes `E.InfoTableProv` events to
build an info table map, if necessary, and processes `E.HeapProfSampleBegin`
and `E.HeapProfSampleEnd` events to maintain an era stack.
-}
processHeapProfSampleData ::
  (MonadIO m) =>
  Verbosity ->
  Maybe HeapProfBreakdown ->
  ProcessT m (WithStartTime Event) (Metric Word64)
processHeapProfSampleData verbosityThreshold maybeHeapProfBreakdown =
  construct $
    go
      HeapProfSampleState
        { eitherShouldWarnOrHeapProfBreakdown = maybe (Left True) Right maybeHeapProfBreakdown
        , infoTableMap = mempty
        , heapProfSampleEraStack = mempty
        }
 where
  -- go :: HeapProfSampleState -> PlanT (Is (WithStartTime Event)) (Metric Word64) m Void
  go st@HeapProfSampleState{..} = do
    await >>= \i -> case i.value.evSpec of
      -- Announces the heap profile breakdown, amongst other things.
      -- This event is only emitted for code compiled with GHC >=9.14.
      E.HeapProfBegin{..}
        | isLeft eitherShouldWarnOrHeapProfBreakdown ->
            go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
      -- Announces the arguments with which the program was called.
      -- This *may* include RTS options, which can be used to determine the
      -- heap profile breakdown for code compiled with GHC <9.14.
      E.ProgramArgs{..}
        | isLeft eitherShouldWarnOrHeapProfBreakdown
        , Just heapProfBreakdown <- findHeapProfBreakdown args ->
            go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
      -- Announces an info table entry.
      E.InfoTableProv{..}
        | shouldTrackInfoTableMap eitherShouldWarnOrHeapProfBreakdown -> do
            let infoTablePtr = InfoTablePtr itInfo
                infoTable =
                  InfoTable
                    { infoTablePtr = infoTablePtr
                    , infoTableName = itTableName
                    , infoTableClosureDesc = itClosureDesc
                    , infoTableTyDesc = itTyDesc
                    , infoTableLabel = itLabel
                    , infoTableModule = itModule
                    , infoTableSrcLoc = itSrcLoc
                    }
            go st{infoTableMap = M.insert infoTablePtr infoTable infoTableMap}
      -- Announces the beginning of a heap profile sample.
      E.HeapProfSampleBegin{..} ->
        go st{heapProfSampleEraStack = heapProfSampleEra : heapProfSampleEraStack}
      -- Announces the end of a heap profile sample.
      E.HeapProfSampleEnd{..} ->
        case L.uncons heapProfSampleEraStack of
          Nothing -> do
            logWarning verbosityThreshold "processHeapProfSampleData" . T.pack $
              printf
                "Eventlog closed era %d, but there is no current era."
                heapProfSampleEra
            go st
          Just (currentEra, heapProfSampleEraStack') -> do
            unless (currentEra == heapProfSampleEra) $
              logWarning verbosityThreshold "processHeapProfSampleData" . T.pack $
                printf
                  "Eventlog closed era %d, but the current era is era %d."
                  heapProfSampleEra
                  currentEra
            go st{heapProfSampleEraStack = heapProfSampleEraStack'}
      -- Announces a heap profile sample.
      E.HeapProfSampleString{..}
        -- If there is no heap profile breakdown, issue a warning, then disable warnings.
        | Left True <- eitherShouldWarnOrHeapProfBreakdown -> do
            logWarning verbosityThreshold "processHeapProfSampleData" $
              "Cannot infer heap profile breakdown.\n\
              \         If your binary was compiled with a GHC version prior to 9.14,\n\
              \         you must also pass the heap profile type to this executable.\n\
              \         See: https://gitlab.haskell.org/ghc/ghc/-/commit/76d392a"
            go st{eitherShouldWarnOrHeapProfBreakdown = Left False, infoTableMap = mempty}
        -- If the heap profile breakdown is biographical, issue a warning, then disable warnings.
        | Right HeapProfBreakdownBiography <- eitherShouldWarnOrHeapProfBreakdown -> do
            logWarning verbosityThreshold "processHeapProfSampleData" . T.pack $
              printf
                "Unsupported heap profile breakdown %s"
                (heapProfBreakdownShow HeapProfBreakdownBiography)
            go st{eitherShouldWarnOrHeapProfBreakdown = Left False, infoTableMap = mempty}
        -- If there is a heap profile breakdown, handle it appropriately.
        | Right heapProfBreakdown <- eitherShouldWarnOrHeapProfBreakdown -> do
            -- If the heap profile breakdown is by info table, add the info table.
            let maybeInfoTable
                  | isHeapProfBreakdownInfoTable heapProfBreakdown = do
                      !infoTablePtr <- readMaybe (T.unpack heapProfLabel)
                      M.lookup infoTablePtr infoTableMap
                  | otherwise = Nothing
            yield $
              metric i heapProfResidency $
                [ "evCap" ~= i.value.evCap
                , "heapProfBreakdown" ~= heapProfBreakdownShow heapProfBreakdown
                , "heapProfId" ~= heapProfId
                , "heapProfLabel" ~= heapProfLabel
                , "heapProfSampleEra" ~= (fst <$> L.uncons heapProfSampleEraStack)
                , "infoTableName" ~= fmap (.infoTableName) maybeInfoTable
                , "infoTableClosureDesc" ~= fmap (.infoTableClosureDesc) maybeInfoTable
                , "infoTableTyDesc" ~= fmap (.infoTableTyDesc) maybeInfoTable
                , "infoTableLabel" ~= fmap (.infoTableLabel) maybeInfoTable
                , "infoTableModule" ~= fmap (.infoTableModule) maybeInfoTable
                , "infoTableSrcLoc" ~= fmap (.infoTableSrcLoc) maybeInfoTable
                ]
            go $ if isHeapProfBreakdownInfoTable heapProfBreakdown then st else st{infoTableMap = mempty}
      _otherwise -> go st

{- |
Parses the `HeapProfBreakdown` command-line arguments:

> heapProfBreakdownEitherReader "T" == Left HeapProfBreakdownClosureType
> heapProfBreakdownEitherReader "c" == Left HeapProfBreakdownCostCentre
> heapProfBreakdownEitherReader "m" == Left HeapProfBreakdownModule
> heapProfBreakdownEitherReader "d" == Left HeapProfBreakdownClosureDescr
> heapProfBreakdownEitherReader "y" == Left HeapProfBreakdownTypeDescr
> heapProfBreakdownEitherReader "e" == Left HeapProfBreakdownEra
> heapProfBreakdownEitherReader "r" == Left HeapProfBreakdownRetainer
> heapProfBreakdownEitherReader "b" == Left HeapProfBreakdownBiography
> heapProfBreakdownEitherReader "i" == Left HeapProfBreakdownInfoTable
-}
heapProfBreakdownEitherReader :: String -> Either String HeapProfBreakdown
heapProfBreakdownEitherReader =
  \case
    "T" -> Right HeapProfBreakdownClosureType
    "c" -> Right HeapProfBreakdownCostCentre
    "m" -> Right HeapProfBreakdownModule
    "d" -> Right HeapProfBreakdownClosureDescr
    "y" -> Right HeapProfBreakdownTypeDescr
    "e" -> Right HeapProfBreakdownEra
    "r" -> Right HeapProfBreakdownRetainer
    "b" -> Right HeapProfBreakdownBiography
    "i" -> Right HeapProfBreakdownInfoTable
    str -> Left $ "Unsupported heap profile breakdown -h" <> str

{- |
Shows a `HeapProfBreakdown` as its corresponding command-line flag:

> heapProfBreakdownShow HeapProfBreakdownClosureType == "-hT"
> heapProfBreakdownShow HeapProfBreakdownCostCentre == "-hc"
> heapProfBreakdownShow HeapProfBreakdownModule == "-hm"
> heapProfBreakdownShow HeapProfBreakdownClosureDescr == "-hd"
> heapProfBreakdownShow HeapProfBreakdownTypeDescr == "-hy"
> heapProfBreakdownShow HeapProfBreakdownEra == "-he"
> heapProfBreakdownShow HeapProfBreakdownRetainer == "-hr"
> heapProfBreakdownShow HeapProfBreakdownBiography == "-hb"
> heapProfBreakdownShow HeapProfBreakdownInfoTable == "-hi"
-}
heapProfBreakdownShow :: HeapProfBreakdown -> String
heapProfBreakdownShow =
  ("-h" <>) . \case
    HeapProfBreakdownClosureType -> "T"
    HeapProfBreakdownCostCentre -> "c"
    HeapProfBreakdownModule -> "m"
    HeapProfBreakdownClosureDescr -> "d"
    HeapProfBreakdownTypeDescr -> "y"
    HeapProfBreakdownEra -> "e"
    HeapProfBreakdownRetainer -> "r"
    HeapProfBreakdownBiography -> "b"
    HeapProfBreakdownInfoTable -> "i"

{- |
Internal helper.
Determine the `HeapProfBreakdown` from the list of program arguments.

__Warning__: This scan is not fully correct. It merely scans for the presence
of arguments that, as a whole, parse with `heapProfBreakdownEitherReader`.
It does not handle @-with-rtsopts@ and does not restrict its search to those
arguments between @+RTS@ and @-RTS@ tags.
-}
findHeapProfBreakdown :: [Text] -> Maybe HeapProfBreakdown
findHeapProfBreakdown = listToMaybe . mapMaybe parseHeapProfBreakdown
 where
  parseHeapProfBreakdown :: Text -> Maybe HeapProfBreakdown
  parseHeapProfBreakdown arg
    | "-h" `T.isPrefixOf` arg =
        either (const Nothing) Just
          . heapProfBreakdownEitherReader
          . T.unpack
          . T.drop 2
          $ arg
    | otherwise = Nothing

-------------------------------------------------------------------------------
-- Reading from the socket
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Socket source

{- |
A source which reads chunks from a `Handle`.
When an input is available, it yields an v`Item`.
When the timeout is reached, it yields a v`Tick`.
-}
sourceHandleWait ::
  (MonadIO m) =>
  -- | The wait timeout in milliseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog socket handle.
  Handle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleWait timeoutMilli chunkSizeBytes handle =
  construct $ fix $ \loop -> do
    ready <- liftIO $ hWaitForInput' handle timeoutMilli
    case ready of
      Ready -> do
        bs <- liftIO $ BS.hGetSome handle chunkSizeBytes
        yield (Item bs)
        loop
      NotReady -> do
        yield Tick
        loop
      EOF ->
        pure ()

-------------------------------------------------------------------------------
-- Socket source with batches

{- |
A source which reads chunks from a `Handle`.
When input is available, it yields an v`Item`.
It yields a v`Tick` at each increment of the batch interval.
-}
sourceHandleBatch ::
  (MonadIO m) =>
  -- | The batch interval in milliseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog socket handle.
  Handle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleBatch batchIntervalMs chunkSizeBytes handle = construct start
 where
  start = do
    startTimeMs <- liftIO getMonotonicTimeMilli
    batch startTimeMs
  batch startTimeMs = waitForInput
   where
    getRemainingTimeMilli = do
      currentTimeMilli <- liftIO getMonotonicTimeMilli
      pure $ (startTimeMs + batchIntervalMs) - currentTimeMilli
    waitForInput = do
      remainingTimeMilli <- getRemainingTimeMilli
      if remainingTimeMilli <= 0
        then do
          yield Tick
          start
        else do
          ready <- liftIO (hWaitForInput' handle remainingTimeMilli)
          case ready of
            Ready -> do
              chunk <- liftIO $ BS.hGetSome handle chunkSizeBytes
              yield (Item chunk) >> waitForInput
            NotReady -> waitForInput
            EOF -> pure ()

{- |
Eventlog chunk size in bytes.
This should be equal to the page size.
-}
defaultChunkSizeBytes :: Int
defaultChunkSizeBytes = 4096

{- |
Internal helper.
Return monotonic time in milliseconds, since some unspecified starting point
-}
getMonotonicTimeMilli :: IO Int
getMonotonicTimeMilli = nanoToMilli <$> getMonotonicTimeNSec

{- |
Internal helper.
Convert nanoseconds to milliseconds.
The conversion from 'Word64' to 'Int' is safe.
It cannot overflow due to the division by 1_000_000.
-}
nanoToMilli :: Word64 -> Int
nanoToMilli = fromIntegral . (`div` 1_000_000)

{- |
Internal helper.
Type to represent the state of a handle.
-}
data Ready = Ready | NotReady | EOF

{- |
Internal helper.
Wait for input from a `Handle` for a given number of milliseconds.
-}
hWaitForInput' ::
  -- | The handle.
  Handle ->
  -- | The timeout in milliseconds.
  Int ->
  IO Ready
hWaitForInput' handle timeoutMilli =
  catch (boolToReady <$> hWaitForInput handle timeoutMilli) handleEOFError
 where
  boolToReady True = Ready
  boolToReady False = NotReady
  handleEOFError err
    | isEOFError err = pure EOF
    | otherwise = throwIO err

-------------------------------------------------------------------------------
-- Writing to a file
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Log file sink

{- |
File sink for optional eventlog log file.
-}
fileSink ::
  (MonadIO m) =>
  Handle ->
  ProcessT m BS.ByteString Void
fileSink handle = repeatedly $ await >>= liftIO . BS.hPut handle

-------------------------------------------------------------------------------
-- Log file sink with batches

{- |
File sink for optional eventlog log file.
-}
fileSinkBatch ::
  (MonadIO m) =>
  Handle ->
  ProcessT m (Tick BS.ByteString) Void
fileSinkBatch handle = dropTick ~> fileSink handle

-------------------------------------------------------------------------------
-- Ticks
-------------------------------------------------------------------------------

{- |
The type of data on a stream of items and ticks.

The t`Tick` type is isomorphic to `Maybe` modulo strictness,
but with the caveat that v`Tick` does not represent failure.
-}
data Tick a = Item !a | Tick
  deriving (Eq, Functor, Foldable, Traversable, Show)

{- |
This machine batches all items between two ticks into a list.
-}
batchByTickList :: Process (Tick a) [a]
batchByTickList =
  mapping (fmap D.singleton)
    ~> batchByTick
    ~> mapping D.toList

{- |
Generalised version of `batchByTickList`.
-}
batchByTick :: (Monoid a) => Process (Tick a) a
batchByTick = construct start
 where
  start = batch mempty
  batch acc =
    await >>= \case
      Item a -> batch (a <> acc)
      Tick -> yield acc >> start

{- |
This machine streams a list of items into a series of items
separated by ticks.
-}
batchListToTick :: Process [a] (Tick a)
batchListToTick = batchToTick

{- |
Generalised version of `batchListToTick`.
-}
batchToTick :: (Foldable f) => Process (f a) (Tick a)
batchToTick = repeatedly go
 where
  go = await >>= \xs -> for_ xs (yield . Item) >> yield Tick

{- |
This machine drops all ticks.
-}
dropTick :: Process (Tick a) a
dropTick =
  repeatedly $
    await >>= \case
      Item a -> yield a
      Tick -> pure ()

{- |
This machine drops all items.
-}
onlyTick :: Process (Tick a) ()
onlyTick =
  repeatedly $
    await >>= \case
      Tick -> yield ()
      Item{} -> pure ()

-------------------------------------------------------------------------------
-- Decoding
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Decoding events

{- |
Parse t'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws a t'DecodeError' on error.
-}
decodeEvent :: (MonadIO m) => ProcessT m BS.ByteString Event
decodeEvent = construct $ loop decodeEventLog
 where
  loop :: (MonadIO m) => Decoder a -> PlanT (Is BS.ByteString) a m ()
  loop Done{} = pure ()
  loop (Consume k) = await >>= \chunk -> loop (k chunk)
  loop (Produce a d') = yield a >> loop d'
  loop (Error _ err) = liftIO $ throwIO $ DecodeError err

{- |
Parse 'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws 'DecodeError' on error.
-}
decodeEventBatch :: (MonadIO m) => ProcessT m (Tick BS.ByteString) (Tick Event)
decodeEventBatch = liftTick decodeEvent

newtype DecodeError = DecodeError String deriving (Show)

instance Exception DecodeError

-------------------------------------------------------------------------------
-- Machine combinators
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Lift a machine to a machine that passes on ticks unchanged

{- |
Lift a machine to a machine that passes on ticks unchanged.

Constructs the following machine:

@
           ┌─(if Tick)────────────────────┐
  [ Tick a ]                              [ Tick b ]
           └─(if Item)─( ProcessT m a b )─┘
@
-}
liftTick ::
  (Monad m) =>
  ProcessT m a b ->
  ProcessT m (Tick a) (Tick b)
liftTick m =
  MachineT $
    runMachineT m <&> \case
      Stop ->
        Stop
      Yield o k ->
        Yield (Item o) (liftTick k)
      Await (onNext :: t -> ProcessT m a b) Refl onStop ->
        await'
       where
        await' = Await onNext' Refl onStop'
         where
          onNext' :: Tick a -> ProcessT m (Tick a) (Tick b)
          onNext' = \case
            Tick ->
              MachineT . pure . Yield Tick $
                MachineT . pure $
                  await'
            Item a -> liftTick (onNext a)
          onStop' :: ProcessT m (Tick a) (Tick b)
          onStop' = liftTick onStop

--------------------------------------------------------------------------------
-- Lift a machine to a machine that operates on batches

{- |
Lift a machine that processes @a@s into @b@s to a machine that processes
batches of @a@s into batches of @b@s.
-}
liftBatch ::
  forall m a b.
  (Monad m) =>
  ProcessT m a b ->
  ProcessT m [a] [b]
liftBatch = MachineT . running [] []
 where
  -- The parent machine is running the child machine with the current batch.
  running :: [a] -> [b] -> ProcessT m a b -> m (Step (Is [a]) [b] (ProcessT m [a] [b]))
  running as bs m =
    runMachineT m >>= \case
      Stop ->
        pure Stop
      Yield b k ->
        running as (b : bs) k
      Await (onNext :: t -> ProcessT m a b) Refl onStop ->
        pure $ Yield (reverse bs) $ MachineT $ awaiting as onNext onStop

  -- The parent machine is awaiting new input.
  awaiting :: [a] -> (a -> ProcessT m a b) -> ProcessT m a b -> m (Step (Is [a]) [b] (ProcessT m [a] [b]))
  awaiting (a : as) onNext _onStop = running as [] $ onNext a
  awaiting [] onNext onStop = pure $ Await onNext' Refl onStop'
   where
    onNext' :: [a] -> ProcessT m [a] [b]
    onNext' as = MachineT $ awaiting as onNext onStop
    onStop' :: ProcessT m [a] [b]
    onStop' = exhausting onStop

  -- The parent machine is exhausting the child machine to gather its output.
  exhausting :: ProcessT m a b -> ProcessT m x [b]
  exhausting = MachineT . go []
   where
    go :: [b] -> ProcessT m a b -> m (Step (Is x) [b] (ProcessT m x [b]))
    go bs m =
      runMachineT m >>= \case
        Stop ->
          pure Stop
        Yield b k ->
          go (b : bs) k
        Await _onNext _Refl onStop ->
          pure $ Yield (reverse bs) $ MachineT $ go [] onStop

--------------------------------------------------------------------------------
-- Construct a processor that spawns a separate child processor for each measure

{- |
Spawn a copy of a machine for each "measure".

Constructs the following machine:

@
    ┌─────(if measure == k0)─( ProcessT m a b )────┐
  [ a ] ──(if measure == ..)─( ProcessT m a b )─ [ b ]
    └─────(if measure == kN)─( ProcessT m a b )────┘
@

__Warning:__ The router does not currently garbage-collect terminated child processors.
-}
liftRouter ::
  forall m k a b.
  (MonadIO m, Hashable k) =>
  -- | Function to measure.
  (a -> Maybe k) ->
  -- | Function to spawn child processors.
  (k -> ProcessT m a b) ->
  ProcessT m a b
liftRouter measure spawn = awaiting M.empty
 where
  awaiting :: HashMap k (ProcessT m a b) -> ProcessT m a b
  awaiting st = MachineT . pure $ Await onNext Refl onStop
   where
    onNext :: a -> MachineT m (Is a) b
    onNext a = case measure a of
      Nothing -> awaiting st
      Just k -> provideThen a m $ \m' -> awaiting (M.insert k m' st)
       where
        m = fromMaybe (spawn k) (M.lookup k st)
    onStop :: MachineT m (Is a) b
    onStop = foldr starve stopped (M.elems st)

  provideThen :: a -> ProcessT m a b -> (ProcessT m a b -> ProcessT m a b) -> ProcessT m a b
  provideThen a m k =
    MachineT $
      runMachineT m >>= \case
        Stop -> runMachineT (k stopped)
        Yield o m' -> pure (Yield o (provideThen a m' k))
        Await onNext Refl _onStop -> runMachineT (exhaustThen (onNext a) k)

  exhaustThen :: ProcessT m a b -> (ProcessT m a b -> ProcessT m a b) -> ProcessT m a b
  exhaustThen m k =
    MachineT $
      runMachineT m >>= \case
        Yield o m' -> pure (Yield o (k m'))
        m' -> runMachineT (k (encased m'))

-------------------------------------------------------------------------------
-- Event stream sorting
-------------------------------------------------------------------------------

{- |
Reorder events respecting ticks.

This machine caches two batches worth of events, sorts them together,
and then yields only those events whose timestamp is less than or equal
to the maximum of the first batch.
-}
sortByBatch :: (a -> Timestamp) -> Process [a] [a]
sortByBatch timestamp = construct $ go mempty
 where
  go old =
    await >>= \case
      new
        | null old -> go (sortByTime new)
        | otherwise -> yield before >> go after
       where
        -- NOTE: use of partial @maximum@ is guarded by the check @null old@.
        cutoff = getMax (foldMap (Max . timestamp) old)
        sorted = joinByTime old (sortByTime new)
        (before, after) = L.partition ((<= cutoff) . timestamp) sorted

  -- compByTime :: a -> a -> Ordering
  compByTime = compare `on` timestamp

  -- sortByTime :: [a] -> [a]
  sortByTime = L.sortBy compByTime

  -- joinByTime :: [a] -> [a] -> [a]
  joinByTime = go
   where
    go [] ys = ys
    go xs [] = xs
    go (x : xs) (y : ys) = case compByTime x y of
      LT -> x : go xs (y : ys)
      _ -> y : go (x : xs) ys

{- |
Variant of `sortByBatch` that operates on streams of items and ticks.
-}
sortByBatchTick :: (a -> Timestamp) -> Process (Tick a) (Tick a)
sortByBatchTick timestamp =
  mapping (fmap (: [])) ~> batchByTick ~> sortByBatch timestamp ~> batchListToTick

-------------------------------------------------------------------------------
-- Filtering semaphores
-------------------------------------------------------------------------------

{- | A simple delimiting t'Moore' machine,
which is opened by one constant marker and closed by the other one.
-}
between :: Text -> Text -> Moore Text Bool
between x y = open
 where
  open = Moore False open' where open' x' = if x == x' then close else open
  close = Moore True close' where close' y' = if y == y' then end else close
  end = Moore False (const end)

-- | Delimit the event process.
delimit :: (Monad m) => Moore Text Bool -> ProcessT m Event Event
delimit = construct . go
 where
  go :: (Monad m) => Moore Text Bool -> PlanT (Is Event) Event m ()
  go mm@(Moore s next) = do
    e <- await
    case evSpec e of
      -- on marker step the moore machine.
      E.UserMarker m -> do
        let mm'@(Moore s' _) = next m
        -- if current or next state is open (== True), emit the marker.
        when (s || s') $ yield e
        go mm'

      -- for other events, emit if the state is open.
      _ -> do
        when s $ yield e
        go mm

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper. Construct a t`Metric` from an event with a start time
(t`WithStartTime` t`Event`), together with the measurement and any attributes.
This is a smart constructor that pulls the various timestamps out of the event.
-}
metric ::
  WithStartTime Event ->
  v ->
  [Attr] ->
  Metric v
metric i v attr =
  Metric
    { value = v
    , maybeTimeUnixNano = tryGetTimeUnixNano i
    , maybeStartTimeUnixNano = i.maybeStartTimeUnixNano
    , attr = attr
    }

{- |
Internal helper. Variant of `mapping` for plans.
-}
mappingPlan :: (a -> b) -> PlanT (Is a) b m a
mappingPlan f = forever (await >>= \a -> yield (f a))

{- |
Internal helper. Return the minimal value by some projection.
-}
minBy :: (Ord b) => (a -> b) -> a -> a -> a
minBy f x y = if f x < f y then x else y
