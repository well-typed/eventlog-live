{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : GHC.Eventlog.Live.Machine
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Analysis.Capability (
  -- * Capability Usage

  -- ** Capability Usage Metrics
  processCapabilityUsageMetrics,

  -- ** Capability Usage Spans
  CapabilityUsageSpan,
  CapabilityUser (..),
  capabilityUser,
  showCapabilityUserCategory,
  processCapabilityUsageSpans,
  processCapabilityUsageSpans',

  -- ** GC Spans
  GCSpan (..),
  processGCSpans,
  processGCSpans',

  -- ** Mutator Spans
  MutatorSpan (..),
  asMutatorSpans,
  asMutatorSpans',
  processMutatorSpans,
  processMutatorSpans',
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.Machine (Is (..), PlanT, ProcessT, asParts, await, construct, mapping, repeatedly, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Eventlog.Live.Data.Attribute (AttrValue, IsAttrValue (..), (~=))
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Data.Span (duration)
import GHC.Eventlog.Live.Internal.Logger (logWarning)
import GHC.Eventlog.Live.Machine.Analysis.Thread (ThreadState (..), ThreadStateSpan (..), processThreadStateSpans')
import GHC.Eventlog.Live.Machine.Core (liftRouter)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..), setWithStartTime'value, tryGetTimeUnixNano)
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.RTS.Events (Event (..), EventInfo, ThreadId, Timestamp)
import GHC.RTS.Events qualified as E
import GHC.Records (HasField (..))
import Text.Printf (printf)

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
                logWarning verbosity . T.pack $
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
            logWarning verbosity . T.pack $
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

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

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
