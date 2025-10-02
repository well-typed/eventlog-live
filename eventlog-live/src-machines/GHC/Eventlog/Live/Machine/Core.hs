{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Machine.Core
Description : Core machines for processing data in batches.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Core (
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

  -- * Routers
  liftRouter,

  -- * Event sorting
  sortByBatch,
  sortByBatchTick,

  -- * Delimiting
  between,
  delimit,

  -- * Validation
  validateInput,
  validateOrder,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.DList qualified as D
import Data.Foldable (for_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable (..))
import Data.List qualified as L
import Data.Machine (Is (..), MachineT (..), Moore (..), PlanT, Process, ProcessT, Step (..), await, construct, encased, mapping, repeatedly, starve, stopped, yield, (~>))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Eventlog.Live.Internal.Logger (logDebug, logError, logWarning)
import GHC.Eventlog.Live.Verbosity (Verbosity, verbosityError, verbosityWarning)
import GHC.RTS.Events (Event (..), Timestamp)
import GHC.RTS.Events qualified as E
import Text.Printf (printf)

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
batchByTick ::
  forall m a.
  (Monad m, Monoid a) =>
  ProcessT m (Tick a) a
batchByTick = batchByTickWith mempty
 where
  batchByTickWith :: a -> MachineT m (Is (Tick a)) a
  batchByTickWith acc = MachineT $ pure $ Await onNext Refl onStop
   where
    onNext :: Tick a -> MachineT m (Is (Tick a)) a
    onNext = \case
      Item a -> batchByTickWith (a <> acc)
      Tick -> MachineT $ pure $ Yield acc batchByTick
    onStop :: MachineT m (Is (Tick a)) a
    onStop = MachineT $ pure $ Yield acc stopped

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
sortByBatch ::
  forall m a.
  (Monad m) =>
  (a -> Timestamp) ->
  ProcessT m [a] [a]
sortByBatch timestamp = sortByBatchWith Nothing
 where
  sortByBatchWith :: Maybe [a] -> ProcessT m [a] [a]
  sortByBatchWith = \case
    Nothing -> MachineT $ pure $ Await onNext Refl onStop
     where
      onNext :: [a] -> ProcessT m [a] [a]
      onNext new = sortByBatchWith (Just sortedNew)
       where
        sortedNew = sortByTime new
      onStop :: ProcessT m [a] [a]
      onStop = stopped
    Just sortedOld -> MachineT $ pure $ Await onNext Refl onStop
     where
      onNext :: [a] -> ProcessT m [a] [a]
      onNext new
        | null sortedOld = sortByBatchWith $ Just sortedNew
        | otherwise = MachineT $ pure $ Yield sortedBeforeCutoff $ sortByBatchWith $ Just sortedAfterCutoff
       where
        -- NOTE: use of partial @maximum@ is guarded by the check @null old@.
        cutoff = getMax (foldMap (Max . timestamp) sortedOld)
        sortedNew = sortByTime new
        sorted = joinByTime sortedOld sortedNew
        (sortedBeforeCutoff, sortedAfterCutoff) = L.partition ((<= cutoff) . timestamp) sorted
      onStop :: ProcessT m [a] [a]
      onStop = MachineT $ pure $ Yield sortedOld $ stopped

  -- compByTime :: a -> a -> Ordering
  compByTime = compare `on` timestamp

  -- sortByTime :: [a] -> [a]
  sortByTime = L.sortBy compByTime

  -- joinByTime :: [a] -> [a] -> [a]
  joinByTime [] ys = ys
  joinByTime xs [] = xs
  joinByTime (x : xs) (y : ys) = case compByTime x y of
    LT -> x : joinByTime xs (y : ys)
    _ -> y : joinByTime (x : xs) ys

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
-- Validation
-------------------------------------------------------------------------------

{- |
This machine validates that there is some input.

If no input is encountered after the given number of ticks, the machine prints
a warning that directs the user to check that the @-l@ flag was set correctly.
-}
validateInput ::
  (MonadIO m) =>
  Verbosity ->
  Int ->
  ProcessT m (Tick a) x
validateInput verbosity ticks
  | verbosityWarning >= verbosity = construct $ start ticks
  | otherwise = stopped
 where
  start remaining
    | remaining <= 0 = liftIO $ do
        logWarning verbosity "validateInput" . T.pack $
          printf
            "No input after %d ticks. Did you pass -l to the GHC RTS?"
            ticks
    | otherwise = do
        logDebug verbosity "validateInput" $
          T.pack (show remaining) <> " ticks remaining."
        await >>= \case
          Item{} -> do
            logDebug verbosity "validateInput" "Received item."
          Tick -> do
            logDebug verbosity "validateInput" "Received tick."
            start (pred remaining)

{- |
This machine validates that the inputs are received in order.

If an out-of-order input is encountered, the machine prints an error message
that directs the user to check that the @--eventlog-flush-interval@ and the
@--batch-interval@ flags are set correctly.
-}
validateOrder ::
  (MonadIO m, Show a) =>
  Verbosity ->
  (a -> Timestamp) ->
  ProcessT m a x
validateOrder verbosity timestamp
  | verbosityError >= verbosity = construct $ start Nothing
  | otherwise = stopped
 where
  start maybeOld =
    await >>= \new ->
      case maybeOld of
        Just old
          | timestamp new < timestamp old -> do
              logError verbosity "validateOrder" . T.pack $
                printf
                  "Encountered two out-of-order inputs.\n\
                  \Did you pass --eventlog-flush-interval to the GHC RTS?\n\
                  \Did you set --batch-interval to be at least as big as the value of --eventlog-flush-interval?"
                  (show old)
                  (show new)
              pure ()
        _otherwise -> do
          start (Just new)
