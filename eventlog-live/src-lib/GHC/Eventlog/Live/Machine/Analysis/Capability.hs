{-# LANGUAGE OverloadedLists #-}
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

  -- ** Productivity Metrics
  Productivity (..),
  processProductivity,

  -- ** Capability Usage Metrics
  processCapabilityUsageDurationData,
  CapabilityUsageDuration (..),
  processCapabilityUsageDuration'Delta,
  processCapabilityUsageDuration'DeltaToCumulative,

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

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Char (isSpace)
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.Machine (Is (..), PlanT, ProcessT, asParts, await, construct, mapping, repeatedly, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Semigroup (Max (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Eventlog.Live.Data.Attribute (AttrValue, Attrs, IsAttrValue (..), (~=))
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Data.Span (duration)
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Machine.Analysis.Thread (ThreadState (..), ThreadStateSpan (..), processThreadStateSpans')
import GHC.Eventlog.Live.Machine.Core (deltaToCumulative, liftRouter)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..), setWithStartTime'value, tryGetTimeUnixNano)
import GHC.Generics (Generic)
import GHC.RTS.Events (Event (..), EventInfo, ThreadId, Timestamp)
import GHC.RTS.Events qualified as E
import GHC.Records (HasField (..))
import Text.Printf (printf)

-------------------------------------------------------------------------------
-- Productivity Metrics

{- |
The productivity measure.

This holds the cumulative elapsed time for both GC and mutator threads.
-}
data Productivity = Productivity
  { gc :: !Timestamp
  , mutator :: !Timestamp
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  , cap :: Int
  }

instance HasField "value" Productivity Double where
  getField :: Productivity -> Double
  getField Productivity{..} =
    realToFrac mutator / realToFrac (mutator + gc)

instance HasField "attrs" Productivity Attrs where
  getField :: Productivity -> Attrs
  getField Productivity{..} =
    ["cap" ~= cap]

instance Semigroup Productivity where
  (<>) :: Productivity -> Productivity -> Productivity
  x <> y =
    Productivity
      { gc = max x.gc y.gc
      , mutator = max x.mutator y.mutator
      , maybeTimeUnixNano = getMax <$> (Max <$> x.maybeTimeUnixNano) <> (Max <$> y.maybeTimeUnixNano)
      , maybeStartTimeUnixNano = getMax <$> (Max <$> x.maybeStartTimeUnixNano) <> (Max <$> y.maybeStartTimeUnixNano)
      , cap = assert (x.cap == y.cap) x.cap
      }

{- |
Convert a t`CapabilityUsageDuration` to a partial t`Productivity` that only
represents the category corresponding to this t`CapabilityUsageDuration`'s
usage category (GC or mutator).
-}
toProductivity :: CapabilityUsageDuration Timestamp -> Maybe Productivity
toProductivity CapabilityUsageDuration{..} = do
  usage >>= \case
    GC -> pure Productivity{gc = value, mutator = 0, ..}
    Mutator{} -> pure Productivity{gc = 0, mutator = value, ..}

{- |
This machine processes t`CapabilityUsageDuration` with the cumulative elapsed
time for each category and produces metrics that contain productivity.
-}
processProductivity ::
  forall m.
  (Monad m) =>
  ProcessT m (CapabilityUsageDuration Timestamp) Productivity
processProductivity =
  liftRouter measure spawn
 where
  -- This measure splits the input by capability.
  measure :: CapabilityUsageDuration Timestamp -> Maybe Int
  measure cud = Just cud.cap

  spawn :: Int -> ProcessT m (CapabilityUsageDuration Timestamp) Productivity
  spawn _cap = construct $ go Nothing
   where
    go ::
      Maybe Productivity ->
      PlanT (Is (CapabilityUsageDuration Timestamp)) Productivity m Void
    go maybeProductivity =
      await >>= \case
        cud
          -- If this usage duration yields a productivity update,
          -- yield an updated productivity.
          | productivityUpdate@Just{} <- toProductivity cud -> do
              let maybeProductivity' = maybeProductivity <> productivityUpdate
              for_ maybeProductivity' yield
              go maybeProductivity'
          -- Otherwise, ignore it.
          | otherwise ->
              go maybeProductivity

-------------------------------------------------------------------------------
-- Capability Usage Duration - Cumulative

{- |
This machine processes t`CapabilityUsageSpan` data and produces metrics
that contain the cumulative elapsed time for each category (idle, GC, mutator).
-}
processCapabilityUsageDurationData ::
  forall m.
  (Monad m) =>
  ProcessT m (WithStartTime CapabilityUsageSpan) (CapabilityUsageDuration Timestamp)
processCapabilityUsageDurationData =
  processCapabilityUsageDuration'Delta
    ~> processCapabilityUsageDuration'DeltaToCumulative

-------------------------------------------------------------------------------
-- Capability Usage Duration

{- |
The delta capability usage duration.
-}
data CapabilityUsageDuration a
  = CapabilityUsageDuration
  { value :: !a
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  , cap :: !Int
  , usage :: !(Maybe CapabilityUser)
  {- ^
  If the capability is actively used, this value is `Just` a `CapabilityUser`.
  If the capability is idle, this value is `Nothing`.
  -}
  }
  deriving (Functor, Foldable, Traversable)

instance HasField "attrs" (CapabilityUsageDuration a) Attrs where
  getField :: CapabilityUsageDuration a -> Attrs
  getField CapabilityUsageDuration{..} =
    [ "cap" ~= cap
    , "category" ~= maybe "Idle" showCapabilityUserCategory usage
    , "user" ~= usage
    ]

{- |
This machine processes t`CapabilityUsageDuration` with the delta of elapsed
time for each category and produces metrics that contain the cumulative elapsed
time for each category (idle, GC, mutator).
-}
processCapabilityUsageDuration'DeltaToCumulative ::
  forall m.
  (Monad m) =>
  ProcessT m (CapabilityUsageDuration Timestamp) (CapabilityUsageDuration Timestamp)
processCapabilityUsageDuration'DeltaToCumulative =
  liftRouter measure (const deltaToCumulative)
 where
  -- This measure splits the input by capability _and_ usage category:
  --
  -- 1. The `Int` represents the capability.
  -- 2. The `Maybe CapabilityUser` represents the usage category.
  --
  measure :: CapabilityUsageDuration Timestamp -> Maybe (Int, Maybe CapabilityUser)
  measure cud = Just (cud.cap, cud.usage)

{- |
This machine processes t`CapabilityUsageSpan` spans and produces metrics that
contain the duration and category of each such span and each idle period in
between.
-}
processCapabilityUsageDuration'Delta ::
  forall m.
  (Monad m) =>
  ProcessT m (WithStartTime CapabilityUsageSpan) (CapabilityUsageDuration Timestamp)
processCapabilityUsageDuration'Delta =
  liftRouter measure spawn
 where
  measure :: WithStartTime CapabilityUsageSpan -> Maybe Int
  measure = Just . (.value.cap)

  spawn :: Int -> ProcessT m (WithStartTime CapabilityUsageSpan) (CapabilityUsageDuration Timestamp)
  spawn cap = construct $ go Nothing
   where
    go ::
      Maybe CapabilityUsageSpan ->
      PlanT (Is (WithStartTime CapabilityUsageSpan)) (CapabilityUsageDuration Timestamp) m Void
    go mi =
      await >>= \j -> do
        -- If there is a previous span, and...
        for_ mi $ \i ->
          -- ...the end time of the previous span precedes the start time of the current span, then...
          when (i.endTimeUnixNano < j.value.startTimeUnixNano) $
            -- ...yield an idle duration metric.
            yield
              CapabilityUsageDuration
                { value = j.value.startTimeUnixNano - i.endTimeUnixNano
                , maybeTimeUnixNano = Just i.endTimeUnixNano
                , maybeStartTimeUnixNano = j.maybeStartTimeUnixNano
                , cap = cap
                , usage = Nothing -- Idle
                }
        -- Yield a duration metric for the current span.
        yield
          CapabilityUsageDuration
            { value = duration j.value
            , maybeTimeUnixNano = Just j.value.startTimeUnixNano
            , maybeStartTimeUnixNano = j.maybeStartTimeUnixNano
            , cap = cap
            , usage = Just $! capabilityUser j.value
            }
        go (Just j.value)

{- |
The type of process using a capability,
which is either a mutator thread or garbage collection.
-}
data CapabilityUser
  = GC
  | Mutator {thread :: !ThreadId}
  deriving stock (Eq, Generic)

instance Hashable CapabilityUser

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
  (Monad m) =>
  Logger m ->
  ProcessT m (WithStartTime Event) (WithStartTime CapabilityUsageSpan)
processCapabilityUsageSpans logger =
  processCapabilityUsageSpans' tryGetTimeUnixNano (.value) setWithStartTime'value setWithStartTime'value logger
    ~> mapping (either (fmap Left) (fmap Right))

{- |
Generalised version of `processCapabilityUsageSpans` that can be adapted to
work on arbitrary types using a getter and a pair of lenses.
-}
processCapabilityUsageSpans' ::
  forall m s t1 t2.
  (Monad m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> GCSpan -> t1) ->
  (s -> MutatorSpan -> t2) ->
  Logger m ->
  ProcessT m s (Either t1 t2)
processCapabilityUsageSpans' timeUnixNano getEvent setGCSpan setMutatorSpan logger =
  -- NOTE:
  -- Combining this fanout with an `Either` is risky, because it
  -- has the potential to lose information if both `processGCSpans`
  -- and `processMutatorSpans` yield a value for the same input.
  -- However, this shouldn't ever happen, since the two processors
  -- process disjoint sets of events.
  fanout
    [ processGCSpans' timeUnixNano getEvent setGCSpan logger
        ~> mapping Left
    , processMutatorSpans' timeUnixNano getEvent setMutatorSpan logger
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
  (Monad m) =>
  Logger m ->
  ProcessT m (WithStartTime Event) (WithStartTime GCSpan)
processGCSpans =
  processGCSpans' tryGetTimeUnixNano (.value) setWithStartTime'value

{- |
Generalised version of `processGCSpans` that can be adapted to work on
arbitrary types using a getter and a lens.
-}
processGCSpans' ::
  forall m s t.
  (Monad m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> GCSpan -> t) ->
  Logger m ->
  ProcessT m s t
processGCSpans' timeUnixNano getEvent setGCSpan logger =
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
                -- ...emit a warning, and...
                let msg =
                      T.pack $
                        printf
                          "Capability %d: Unsupported trace %s --> %s"
                          cap
                          (showEventInfo (getEventInfo i))
                          (showEventInfo (getEventInfo j))
                lift $ writeLog logger WARN $ msg
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
            -- ...emit a warning, and...
            let msg =
                  T.pack $
                    printf
                      "Capability %d: Unsupported trace %s --> %s"
                      cap
                      (maybe "?" (showEventInfo . getEventInfo) mi)
                      (showEventInfo (getEventInfo j))
            lift $ writeLog logger WARN $ msg
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
  (Monad m) =>
  Logger m ->
  ProcessT m (WithStartTime Event) (WithStartTime MutatorSpan)
processMutatorSpans =
  processMutatorSpans' tryGetTimeUnixNano (.value) setWithStartTime'value

{- |
Generalised version of `processMutatorSpans` that can be adapted to work on
arbitrary types using a getter and a lens.
-}
processMutatorSpans' ::
  forall m s t.
  (Monad m) =>
  (s -> Maybe Timestamp) ->
  (s -> Event) ->
  (s -> MutatorSpan -> t) ->
  Logger m ->
  ProcessT m s t
processMutatorSpans' timeUnixNano getEvent setMutatorSpan logger =
  processThreadStateSpans' timeUnixNano getEvent setThreadStateSpan logger ~> asParts
 where
  setThreadStateSpan :: s -> ThreadStateSpan -> Maybe t
  setThreadStateSpan s threadStateSpan =
    setMutatorSpan s <$> threadStateSpanToMutatorSpan threadStateSpan

{- |
This machine converts any `Running` t`ThreadStateSpan` to a t`MutatorSpan`.
-}
asMutatorSpans ::
  forall m.
  (Monad m) =>
  ProcessT m ThreadStateSpan MutatorSpan
asMutatorSpans = asMutatorSpans' id (const id)

{- |
Generalised version of `asMutatorSpans` that can be adapted to work on
arbitrary types using a getter and a lens.
-}
asMutatorSpans' ::
  forall m s t.
  (Monad m) =>
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
