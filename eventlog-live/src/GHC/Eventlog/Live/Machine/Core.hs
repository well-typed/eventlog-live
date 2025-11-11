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
  batchByTickList,
  batchByTicksList,
  batchByTick,
  batchByTicks,
  batchToTick,
  batchToTicks,
  dropTick,
  onlyTick,
  liftTick,

  -- * Routers
  liftRouter,

  -- * Event sorting
  sortByBatch,
  sortByTick,

  -- * Delimiting
  delimit,
  betweenEach,
  betweenFirst,

  -- * Validation
  validateInput,
  validateOrder,
) where

import Control.Monad (replicateM_, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.DList qualified as D
import Data.Foldable (Foldable (..), for_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable (..))
import Data.List qualified as L
import Data.Machine (Is (..), MachineT (..), Moore (..), PlanT, Process, ProcessT, Step (..), asParts, await, construct, encased, mapping, repeatedly, starve, stopped, yield, (~>))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max (..))
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Eventlog.Live.Logger (logDebug, logError, logWarning)
import GHC.Eventlog.Live.Verbosity (Verbosity, verbosityError, verbosityWarning)
import Text.Printf (printf)

{- $setup
>>> import Data.Machine ((<~), auto, echo, fold, taking, run, runT_, scan, source)
>>> import Data.Semigroup (Sum (..))
-}

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
Batches items to lists.

The process @`batchByTickList`@ consumes a stream of items and ticks.
It preserves ticks but batches items between ticks to lists.

__Warning:__ This process does not yield empty batches.

==== __Examples__

>>> run $ batchByTickList <~ source [Item 1,Item 2,Tick,Item 3,Tick,Item 4,Item 5,Tick,Item 6,Tick]
[[1,2],[3],[4,5],[6]]

>>> run $ batchByTickList <~ source [Item 1,Item 2,Tick,Tick]
[[1,2]]
-}
batchByTickList :: Process (Tick a) [a]
batchByTickList =
  mapping (fmap D.singleton)
    ~> batchByTick
    ~> dropTick
    ~> mapping D.toList

{- |
Batches items for a given number of ticks to lists.

The process @`batchByTicksList` n@ consumes a stream of items and ticks.
It preserves ticks but batches items for @n@ ticks to lists and yields the batch before the @n@'th tick.

__Warning:__ This process does not yield empty batches.

==== __Examples__

>>> run $ batchByTicksList 2 <~ source [Item 1,Item 2,Tick,Item 3,Tick,Item 4,Item 5,Tick,Item 6,Tick]
[[1,2,3],[4,5,6]]

>>> run $ batchByTicksList 2 <~ source [Item 1,Item 2,Tick,Tick]
[[1,2]]
-}
batchByTicksList ::
  -- | The number of ticks per batch.
  Int ->
  Process (Tick a) [a]
batchByTicksList ticks =
  mapping (fmap D.singleton)
    ~> batchByTicks ticks
    ~> dropTick
    ~> mapping D.toList

{- |
Batches items via their `Semigroup` instance.

The process @`batchByTick`@ consumes a stream of items and ticks.
It preserves ticks but batches items between ticks using `sconcat`.

==== __Examples__

>>> run $ batchByTick <~ source [Item [1],Item [2],Tick,Item [3],Tick,Item [4],Item [5],Tick,Item [6],Tick]
[Item [1,2],Tick,Item [3],Tick,Item [4,5],Tick,Item [6],Tick]

>>> run $ batchByTick <~ source [Item (Sum 1),Item (Sum 2),Tick,Item (Sum 3),Tick,Item (Sum 4),Item (Sum 5),Tick,Item (Sum 6),Tick]
[Item (Sum {getSum = 3}),Tick,Item (Sum {getSum = 3}),Tick,Item (Sum {getSum = 9}),Tick,Item (Sum {getSum = 6}),Tick]
-}
batchByTick ::
  forall a.
  (Monoid a) => Process (Tick a) (Tick a)
batchByTick = batchByTicks 1

{- |
Batches items for a given number of ticks via their `Semigroup` instance.

The process @`batchByTicks` n@ consumes a stream of items and ticks.
It preserves ticks but batches items for @n@ ticks using `sconcat` and yields the batch before the @n@'th tick.

==== __Examples__

>>> run $ batchByTicks 2 <~ source [Item [1],Item [2],Tick,Item [3],Tick,Item [4],Item [5],Tick,Item [6],Tick]
[Tick,Item [1,2,3],Tick,Tick,Item [4,5,6],Tick]

>>> run $ batchByTicks 2 <~ source [Item (Sum 1),Item (Sum 2),Tick,Item (Sum 3),Tick,Item (Sum 4),Item (Sum 5),Tick,Item (Sum 6),Tick]
[Tick,Item (Sum {getSum = 6}),Tick,Tick,Item (Sum {getSum = 15}),Tick]
-}
batchByTicks ::
  forall a.
  (Semigroup a) =>
  -- | The number of ticks per batch.
  Int ->
  Process (Tick a) (Tick a)
batchByTicks ticks = batchByTicksWith ticks mempty
 where
  batchByTicksWith ::
    forall m.
    (Monad m) =>
    Int ->
    Maybe a ->
    MachineT m (Is (Tick a)) (Tick a)
  batchByTicksWith ticksRemaining maybeAcc =
    MachineT $ pure $ Await onNext Refl onStop
   where
    yieldItem :: a -> ProcessT m (Tick a) (Tick a) -> ProcessT m (Tick a) (Tick a)
    yieldItem a = MachineT . pure . Yield (Item a)

    yieldTick :: ProcessT m (Tick a) (Tick a) -> ProcessT m (Tick a) (Tick a)
    yieldTick = MachineT . pure . Yield Tick

    onNext :: Tick a -> MachineT m (Is (Tick a)) (Tick a)
    onNext = \case
      Item a -> batchByTicksWith ticksRemaining (maybeAcc <> Just a)
      Tick
        | ticksRemaining <= 1 ->
            -- Yield an `Item` if any items were accumulated.
            maybe id yieldItem maybeAcc $
              -- Yield the `Tick`.
              yieldTick $
                -- Continue with the initial state.
                batchByTicksWith ticks Nothing
        | otherwise ->
            -- Yield the `Tick`.
            yieldTick $
              -- Continue with one fewer tick remaining.
              batchByTicksWith (ticksRemaining - 1) maybeAcc

    onStop :: MachineT m (Is (Tick a)) (Tick a)
    onStop =
      -- Yield an `Item` if any items were accumulated.
      maybe id yieldItem maybeAcc $
        -- Stop.
        stopped

{- |
Streams batches of items as items and ticks.

The process @`batchToTick`@ consumes a stream of batches of items
and produces a stream of items and with a tick separating each batch.

__Warning:__ The process @`batchByTickList` `~>` `batchToTick`@ is not the identity!

==== __Examples__

>>> run $ batchToTick <~ source [[1,2],[3,4]]
[Item 1,Item 2,Tick,Item 3,Item 4,Tick]

>>> run $ batchToTick <~ batchByTickList <~ source [Item 1,Item 2,Tick,Tick]
[Item 1,Item 2,Tick]
-}
batchToTick ::
  (Foldable f) =>
  Process (f a) (Tick a)
batchToTick = batchToTicks 1

{- |
Streams batches of items as items and ticks.

The process @`batchToTicks` n@ consumes a stream of lists of items
and produces a stream of items and with @n@ ticks separating each list.

To mirror the batches created by @`batchByTicks` n@, each list is
opened with @n - 1@ ticks and closed by the @n@'th tick.

__Warning:__ The process @`batchByTicks` n `~>` `batchToTicks` n@ is not the identity!

==== __Examples__

>>> run $ batchToTicks 2 <~ source [[1,2],[3,4]]
[Tick,Item 1,Item 2,Tick,Tick,Item 3,Item 4,Tick]

>>> run $ batchToTicks 2 <~ batchByTicksList 2 <~ source [Item 1,Item 2,Tick,Tick]
[Tick,Item 1,Item 2,Tick]
-}
batchToTicks ::
  (Foldable f) =>
  -- | The number of ticks per batch.
  Int ->
  Process (f a) (Tick a)
batchToTicks ticks = repeatedly go
 where
  go = await >>= \xs -> replicateM_ (ticks - 1) (yield Tick) >> for_ xs (yield . Item) >> yield Tick

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

==== __Examples__

>>> run $ liftTick (mapping (+1)) <~ source [Item 1,Tick,Item 2,Item 3,Tick,Tick]
[Item 2,Tick,Item 3,Item 4,Tick,Tick]

>>> run $ liftTick (scan (+) 0) <~ source [Item 1,Tick,Item 2,Item 3,Tick,Tick]
[Item 0,Item 1,Tick,Item 3,Item 6,Tick,Tick]
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
-- Construct a processor that spawns a separate child processor for each measure

{- |
Spawns a process for each measure.

Constructs the following machine:

@
    ┌─────(if measure == k0)─( spawn k0 :: ProcessT m a b )────┐
  [ a ] ──(if measure == ..)─( spawn .. :: ProcessT m a b )─ [ b ]
    └─────(if measure == kN)─( spawn kN :: ProcessT m a b )────┘
@

If the spawned process for some measure stops,
then all future inputs for that measure are ignored.

__Warning:__
The router process holds on to a reference to each measure @i@ for each child
process, even after that child process has stopped.

==== __Examples__

>>> run $ liftRouter (Just . even) (\case {True -> mapping (+1); False -> echo}) <~ source [1,2,3,4,5]
[1,3,3,5,5]

>>> run $ liftRouter (Just . even) (\case {True -> echo; False -> stopped}) <~ source [1,2,3,4,5]
[2,4]

>>> run $ liftRouter (Just . even) (\case {True -> echo; False -> taking 1}) <~ source [1,2,3,4,5]
[1,2,4]
-}
liftRouter ::
  forall m i a b.
  (Monad m, Hashable i) =>
  -- | Function to measure.
  (a -> Maybe i) ->
  -- | Function to spawn child processors.
  (i -> ProcessT m a b) ->
  ProcessT m a b
liftRouter measure spawn = awaiting M.empty
 where
  awaiting :: HashMap i (Child (ProcessT m a b)) -> ProcessT m a b
  awaiting st = MachineT . pure $ Await onNext Refl onStop
   where
    onNext :: a -> MachineT m (Is a) b
    onNext a = case measure a of
      Nothing -> awaiting st
      Just i ->
        case fromMaybe (ChildRunning $ spawn i) (M.lookup i st) of
          ChildRunning p ->
            provideThen a p $ \p' ->
              let !st' = M.insert i p' st
               in awaiting st'
          ChildStopped ->
            awaiting st

    onStop :: MachineT m (Is a) b
    onStop = foldr starve stopped (concatMap toList . M.elems $ st)

  provideThen :: a -> ProcessT m a b -> (Child (ProcessT m a b) -> ProcessT m a b) -> ProcessT m a b
  provideThen a p k =
    MachineT $
      runMachineT p >>= \case
        Stop -> runMachineT (k ChildStopped)
        Yield o p' -> pure (Yield o (provideThen a p' k))
        Await onNext Refl _onStop -> runMachineT (exhaustThen (onNext a) k)

  exhaustThen :: ProcessT m a b -> (Child (ProcessT m a b) -> ProcessT m a b) -> ProcessT m a b
  exhaustThen p k =
    MachineT $
      runMachineT p >>= \case
        Yield o p' -> pure (Yield o (k $ ChildRunning p'))
        p' -> runMachineT (k (ChildRunning $ encased p'))

{- |
Internal helper.
A wrapper for child processes spawned by `liftRouter`.
-}
data Child a
  = ChildRunning !a
  | ChildStopped
  deriving (Functor, Foldable)

-------------------------------------------------------------------------------
-- Event stream sorting
-------------------------------------------------------------------------------

{- |
Sort items in successive batches.

If the maximum key in batch @i@ is guaranteed to be smaller than the minimum
key in batch @i + 2@, this process produces a totally ordered stream of items.

The process @`sortByBatch` key@ caches two batches of items, sorts them
together, and yields only those items whose key is less than or equal
to the maximum key in the first batch.

==== __Examples__

>>> run $ sortByBatch @Int id <~ source [[1,4],[7,2,3,5],[6,8]]
[[1,2,3,4],[5,6,7],[8]]

>>> run $ sortByBatch @Int id <~ source [[1,7],[4,2,3,5],[6,8]]
[[1,2,3,4,5,7],[6,8]]
-}
sortByBatch ::
  forall a k.
  (Bounded k, Ord k) =>
  (a -> k) ->
  Process [a] [a]
sortByBatch key = sortByBatchWith Nothing
 where
  sortByBatchWith ::
    forall m.
    (Monad m) =>
    Maybe [a] ->
    ProcessT m [a] [a]
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
        cutoff = getMax (foldMap (Max . key) sortedOld)
        sortedNew = sortByTime new
        sorted = joinByTime sortedOld sortedNew
        (sortedBeforeCutoff, sortedAfterCutoff) = L.partition ((<= cutoff) . key) sorted
      onStop :: ProcessT m [a] [a]
      onStop = MachineT $ pure $ Yield sortedOld $ stopped

  compByTime :: a -> a -> Ordering
  compByTime = compare `on` key

  sortByTime :: [a] -> [a]
  sortByTime = L.sortBy compByTime

  joinByTime {- Sorted -} :: [a {- Sorted -}] -> [a {- Sorted -}] -> [a]
  joinByTime [] ys = ys
  joinByTime xs [] = xs
  joinByTime (x : xs) (y : ys)
    | compByTime x y == LT = x : joinByTime xs (y : ys)
    | otherwise = y : joinByTime (x : xs) ys

{- |
Sort items between successive ticks.

If the maximum key in batch @i@ is guaranteed to be smaller than the minimum
key in batch @i + 2@, this process produces a totally ordered stream of items.

==== __Examples__

>>> run $ sortByTick @Int id <~ source [Item 1,Item 4,Tick,Item 7,Item 2,Item 3,Item 5,Tick,Item 6,Item 8]
[Tick,Item 1,Item 2,Item 3,Item 4,Tick,Item 5,Item 6,Item 7,Item 8]

>>> run $ sortByTick @Int id <~ source [Item 1,Item 7,Tick,Item 4,Item 2,Item 3,Item 5,Tick,Item 6,Item 8]
[Tick,Item 1,Item 2,Item 3,Item 4,Item 5,Item 7,Tick,Item 6,Item 8]
-}
sortByTick ::
  forall a k.
  (Bounded k, Ord k) =>
  (a -> k) ->
  Process (Tick a) (Tick a)
sortByTick key =
  mapping (fmap D.singleton)
    ~> batchByTick
    ~> mapping (fmap D.toList)
    ~> liftTick (sortByBatch key ~> asParts)

-------------------------------------------------------------------------------
-- Filtering semaphores
-------------------------------------------------------------------------------

{- |
A delimiting t`Moore` machine based on constant open/close markers.

The machine @`between` o c@ consumes consumes a stream of items, and produces
a stream of `Bool` that is `False` up to and including the first occurrence of
@o@, then is `True` up to and including the first occurrence of @c@, and then
is `False` forever.

==== __Examples__

>>> run $ auto (betweenEach (2, 4)) <~ source [1, 2, 3, 4, 5]
[False,False,True,True,False,False]

>>> run $ auto (betweenEach (2, 4)) <~ source [2, 3, 4, 2, 3, 4]
[False,True,True,False,True,True,False]
-}
betweenEach :: (Eq a) => (a, a) -> Moore a Bool
betweenEach (open, close) = beforeOpen
 where
  beforeOpen = Moore False $ \a ->
    if a == open then betweenOpenAndClose else beforeOpen
  betweenOpenAndClose = Moore True $ \a ->
    if a == close then beforeOpen else betweenOpenAndClose

{- |
A delimiting t`Moore` machine based on constant open/close markers.

The machine @`between` o c@ consumes consumes a stream of items, and produces
a stream of `Bool` that is `False` up to and including the first occurrence of
@o@, then is `True` up to and including the first occurrence of @c@, and then
is `False` forever.

==== __Examples__

>>> run $ auto (betweenFirst (2, 4)) <~ source [1, 2, 3, 4, 5]
[False,False,True,True,False,False]

>>> run $ auto (betweenFirst (2, 4)) <~ source [2, 3, 4, 2, 3, 4]
[False,True,True,False,False,False,False]
-}
betweenFirst :: (Eq a) => (a, a) -> Moore a Bool
betweenFirst (open, close) = beforeFirstOpen
 where
  beforeFirstOpen = Moore False $ \a ->
    if a == open then betweenFirstOpenAndClose else beforeFirstOpen
  betweenFirstOpenAndClose = Moore True $ \a ->
    if a == close then afterFirstClose else betweenFirstOpenAndClose
  afterFirstClose = Moore False (const afterFirstClose)

{- |
Filter the items in a stream based on a t`Moore` machine.

==== __Examples__

>>> run $ delimit (betweenEach (2, 4)) <~ source [1, 2, 3, 4, 5]
[2,3,4]

>>> run $ delimit (betweenEach (2, 4)) <~ source [2, 3, 4, 2, 3, 4]
[2,3,4,2,3,4]

>>> run $ delimit (betweenFirst (2, 4)) <~ source [1, 2, 3, 4, 5]
[2,3,4]

>>> run $ delimit (betweenFirst (2, 4)) <~ source [2, 3, 4, 2, 3, 4]
[2,3,4]
-}
delimit ::
  forall m a.
  (Monad m) =>
  Moore a Bool ->
  ProcessT m a a
delimit = construct . go
 where
  go ::
    Moore a Bool ->
    PlanT (Is a) a m Void
  go _st@(Moore wasOpen onNext) =
    await >>= \a -> do
      -- Feed the item to the delimiting Moore machine.
      let st'@(Moore willBeOpen _) = onNext a
      -- If the state has changed, i.e., @wasOpen /= willBeOpen@, then
      -- the current item is a marker. All markers should be yielded.
      let isMarker = wasOpen /= willBeOpen
      when (wasOpen || isMarker) $ yield a
      go st'

-------------------------------------------------------------------------------
-- Validation
--
-- TODO: These machines, or at least the error messages that they print, are
--       specific to eventlog processing. Hence, they should be moved.
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
        logWarning verbosity . T.pack $
          printf
            "No input after %d ticks. Did you pass -l to the GHC RTS?"
            ticks
    | otherwise = do
        logDebug verbosity $
          T.pack (show remaining) <> " ticks remaining."
        await >>= \case
          Item{} -> do
            logDebug verbosity "Received item."
          Tick -> do
            logDebug verbosity "Received tick."
            start (pred remaining)

{- |
This machine validates that the inputs are received in order.

If an out-of-order input is encountered, the machine prints an error message
that directs the user to check that the @--eventlog-flush-interval@ and the
@--batch-interval@ flags are set correctly.
-}
validateOrder ::
  (MonadIO m, Ord k, Show a) =>
  Verbosity ->
  (a -> k) ->
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
              logError verbosity . T.pack $
                "Encountered two out-of-order inputs.\n\
                \Did you pass --eventlog-flush-interval to the GHC RTS?\n\
                \Did you set --batch-interval to be at least as big as the value of --eventlog-flush-interval?"
              logDebug verbosity . T.pack $
                printf
                  "Out-of-order inputs:\n\
                  \- %s\n\
                  \- %s"
                  (show old)
                  (show new)
              pure ()
        _otherwise -> do
          start (Just new)
