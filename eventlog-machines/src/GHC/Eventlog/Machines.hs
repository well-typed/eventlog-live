{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Eventlog.Machines (
  -- * Machines

  -- ** Event Source
  sourceHandleWait,
  sourceHandleInterval,
  decodeEventsTick,

  -- ** Ticks
  Tick (..),
  batchByTick,
  dropTick,

  -- ** Event Analysis
  analysis,
  analyse,
  AnalysisState,
  threadAnalysis,
  analyseThread,
  ThreadAnalysisState,

  -- ** Event Order
  reorderEvents,
  checkOrder,

  -- ** Delimiting
  delimit,
  between,

  -- * Exceptions
  DecodeError (..),
) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception, catch, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.Foldable (for_, traverse_)
import Data.Function (fix)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (partition, sortBy)
import Data.Machine (Is, MachineT, PlanT, Process, ProcessT, await, construct, repeatedly, yield)
import Data.Machine.Moore (Moore (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.RTS.Events (Event (..), EventInfo (..), Timestamp, evTime)
import qualified GHC.RTS.Events.Analysis as EA
import GHC.RTS.Events.Analysis.Thread (ThreadState (..), threadMachine)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import qualified System.Clock as Clock
import System.IO (Handle, hWaitForInput)
import System.IO.Error (isEOFError)
import GHC.Stack (HasCallStack)
import GHC.RTS.Events.Analysis (Machine(..))

-------------------------------------------------------------------------------
-- Source from handle
-------------------------------------------------------------------------------

{- |
A source which waits for input using 'hWaitForInput',
produces 'Tick' events on timeout.
-}
sourceHandleWait ::
  (MonadIO m) =>
  -- | The wait timeout in milliseconds (argument of 'hWaitForInput'').
  Int ->
  -- | The number of bytes to read (argument of 'BS.hGetSome').
  Int ->
  Handle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleWait timeoutMs chunkSizeBytes handle = construct $ fix $ \loop -> do
  ready <- liftIO $ hWaitForInput' handle timeoutMs
  case ready of
    Ready -> do
      bs <- liftIO $ BS.hGetSome handle chunkSizeBytes
      yield (Item bs)
      loop
    NotReady -> do
      yield Tick
      loop
    EOF ->
      return ()

data Ready
  = Ready
  | EOF
  | NotReady

hWaitForInput' :: Handle -> Int -> IO Ready
hWaitForInput' handle timeoutMs =
  catch (boolToReady <$> hWaitForInput handle timeoutMs) handleEOFError
 where
  boolToReady True = Ready
  boolToReady False = NotReady
  handleEOFError exc
    | isEOFError exc = return EOF
    | otherwise = throwIO exc

-------------------------------------------------------------------------------
-- Source from handle at interval
-------------------------------------------------------------------------------

sourceHandleInterval ::
  (MonadIO m) =>
  -- | The interval in milliseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog socket handle.
  Handle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleInterval intervalMs chunkSizeBytes handle = construct start
 where
  start = do
    startTimeMs <- liftIO getMonotonicTimeMs
    batch startTimeMs
  batch startTimeMs = waitForInput
   where
    getRemainingTimeMs = do
      currentTimeMs <- liftIO getMonotonicTimeMs
      pure $ (startTimeMs + intervalMs) - currentTimeMs
    waitForInput = do
      remainingTimeMs <- getRemainingTimeMs
      if remainingTimeMs <= 0
        then do
          yield Tick
          start
        else do
          ready <- liftIO (hWaitForInput' handle remainingTimeMs)
          case ready of
            Ready -> do
              chunk <- liftIO $ BS.hGetSome handle chunkSizeBytes
              yield (Item chunk) >> waitForInput
            NotReady -> waitForInput
            EOF -> pure ()

{- |
Internal helper.
Return monotonic time in milliseconds, since some unspecified starting point
-}
getMonotonicTimeMs :: IO Int
getMonotonicTimeMs = nanoToMilli <$> getMonotonicTimeNSec

{- |
Internal helper.
Convert nanoseconds to milliseconds.
The conversion from 'Word64' to 'Int' is safe.
It cannot overflow due to the division by 1_000_000.
-}
nanoToMilli :: Word64 -> Int
nanoToMilli = fromIntegral . (`div` 1_000_000)

-------------------------------------------------------------------------------
-- Ticks
-------------------------------------------------------------------------------

data Tick a = Item !a | Tick
  deriving (Eq, Functor, Foldable, Traversable, Show)

batchByTick :: Process (Tick a) [a]
batchByTick = construct start
 where
  start = batch []
  batch acc =
    await >>= \case
      Item a -> batch (a : acc)
      Tick -> yield (reverse acc) >> start

dropTick :: Process (Tick a) a
dropTick = repeatedly go
 where
  go =
    await >>= \case
      Item a -> yield a
      Tick -> pure ()

-------------------------------------------------------------------------------
-- Helper functions
--
--     [ l ] ———————— [ l' ]
--       |              |
-- [Either l r]   [Either l' r']
--       |              |
--     [ r ] ———————— [ r' ]
--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Decoding events
-------------------------------------------------------------------------------

{- |
Parse 'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws 'DecodeError' on error.
-}
decodeEventsTick :: (MonadIO m) => ProcessT m (Tick BS.ByteString) (Tick Event)
decodeEventsTick = construct $ loop decodeEventLog
 where
  loop :: (MonadIO m) => Decoder a -> PlanT (Is (Tick BS.ByteString)) (Tick a) m ()
  loop Done{} = pure ()
  loop (Consume k) =
    await >>= \case
      Item chunk -> loop (k chunk)
      Tick -> yield Tick >> loop (Consume k)
  loop (Produce a d') = yield (Item a) >> loop d'
  loop (Error _ err) = liftIO $ throwIO $ DecodeError err

newtype DecodeError = DecodeError String deriving (Show)

instance Exception DecodeError

-------------------------------------------------------------------------------
-- Running ghc-events Analysis Machines
-------------------------------------------------------------------------------

newtype AnalysisState s = AnalysisState {unAnalysisState :: IntMap s}
  deriving newtype (Semigroup, Monoid)

notMember :: Int -> AnalysisState s -> Bool
notMember k = (k `IM.notMember`) . unAnalysisState

analysis ::
  (MonadIO m) =>
  (i -> Int) ->
  EA.Machine s i ->
  ProcessT m i (s, i)
analysis key m = construct (go mempty)
 where
  go st = do
    i <- await
    let (ms', st') = analyse key m st i
    for_ ms' $ \s' -> yield (s', i)
    go st'

analyse ::
  (i -> Int) ->
  EA.Machine s i ->
  AnalysisState s ->
  i ->
  (Maybe s, AnalysisState s)
analyse key m st i
  | not (EA.alpha m i) = (Nothing, st)
  | otherwise = coerce (IM.alterF handle (key i) (coerce st))
 where
  handle ms = do
    -- The input state (initial if missing).
    let s = fromMaybe (EA.initial m) ms
    -- The output state (Nothing if step fails).
    let ms' = rightToMaybe (EA.step m s i)
    (ms', ms' <|> Just s)

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

-------------------------------------------------------------------------------
-- Thread Analysis

threadAnalysis ::
  (MonadIO m) =>
  ProcessT m EventInfo (ThreadState, EventInfo)
threadAnalysis = construct (go mempty)
 where
  go st = do
    i <- await
    let (ms', st') = analyseThread st i
    for_ ms' $ \s' -> yield (s', i)
    go st'

data ThreadAnalysisState = ThreadAnalysisState
  { analysisState :: AnalysisState ThreadState
  , mainThreadId :: Maybe Int
  }

instance Semigroup ThreadAnalysisState where
  (<>) :: ThreadAnalysisState -> ThreadAnalysisState -> ThreadAnalysisState
  tas1 <> tas2 =
    ThreadAnalysisState
      { analysisState = analysisState tas1 <> analysisState tas2
      , mainThreadId = mainThreadId tas1 <|> mainThreadId tas2
      }

instance Monoid ThreadAnalysisState where
  mempty :: ThreadAnalysisState
  mempty = ThreadAnalysisState mempty empty

{- |
The `threadMachine` exported by "GHC.RTS.Events.Analysis.Thread" has several flaws:

* GHC does not issue a `CreateThread` event for the main thread.
* GHC may restart a thread and issue a `WakeupThread` after stopping a thread with `ThreadFinished`.

This machine fixes both of those issues by accepting two new transitions:

* @(ThreadInitial, RunThread) -> ThreadRunning@
* @(ThreadFinal, WakeupThread) -> ThreadRunning@

Noted the first of the two transitions is more general than _just_ accepting this transition once for the main thread.
However, doing so greatly simplifies the code and improves its robustness.

TODO: Upstream to @ghc-events@.
-}
threadMachine' :: EA.Machine ThreadState EventInfo
threadMachine' = threadMachine{delta = delta'}
 where
  delta' ThreadInitial RunThread {} = Just ThreadRunning
  delta' ThreadInitial WakeupThread {} = Just ThreadRunning
  delta' s i = EA.delta threadMachine s i

analyseThread ::
  ThreadAnalysisState ->
  EventInfo ->
  (Maybe ThreadState, ThreadAnalysisState)
--analyseThread st@ThreadAnalysisState{..} evSpec@RunThread {}
--  -- NOTE: There is no `CreateThread` event for the main thread. However, the
--  --       `threadMachine` does not (and cannot) take this into account.
--  --       To work around the fact that there is no event announcing the main
--  --       thread, we take the first `RunThread` with an unknown `ThreadId`
--  --       to be the main thread.
--  | isNothing mainThreadId && threadKey evSpec `notMember` analysisState = do
--    let mainThreadId' = threadKey evSpec
--    let s' = ThreadRunning
--    let analysisState' = AnalysisState (IM.insert mainThreadId' s' (coerce analysisState))
--    (Just s', st{mainThreadId = Just mainThreadId', analysisState = analysisState'})
analyseThread st evSpec = do
    let (ms', analysisState') = analyse threadKey threadMachine' (analysisState st) evSpec
    (ms', st{analysisState = analysisState'})

{- |
Internal helper. Get the thread ID as an `Int`.

__Warning__:
This function is partial and fails if the event does not have the field
named `thread`. The assumtion is that if @`EA.alpha` `threadMachine` evSpec@
holds, then @`thread` evSpec@ is defined.
-}
threadKey :: HasCallStack => EventInfo -> Int
threadKey = fromIntegral . thread

-------------------------------------------------------------------------------
-- Reordering buffer.
-------------------------------------------------------------------------------

{- | Buffer and reorder 'Event's to hopefully achieve
monotonic, causal stream of 'Event's.
-}
reorderEvents :: (MonadIO m) => Word64 -> ProcessT m (Tick Event) Event
reorderEvents interval = construct start
 where
  -- interval to wait for cutoff, 1.5 of flushout interval
  interval' :: Int64
  interval' = fromIntegral $ interval + interval `div` 2

  start :: (MonadIO m) => PlanT (Is (Tick Event)) Event m ()
  start = do
    mev <- await
    case mev of
      Tick -> start
      Item ev -> do
        our <- liftIO (timeSpec <$> Clock.getTime Clock.Monotonic)
        loop (our - timeStamp (evTime ev)) [ev]

  -- the Int64 argument is the minimum difference we have seen between our
  -- clock and incoming events.
  loop :: (MonadIO m) => Int64 -> [Event] -> PlanT (Is (Tick Event)) Event m ()
  loop diff evs = do
    mev <- await
    case mev of
      Tick -> do
        our <- liftIO (timeSpec <$> Clock.getTime Clock.Monotonic)
        yieldEvents our diff evs
      Item ev -> do
        our <- liftIO (timeSpec <$> Clock.getTime Clock.Monotonic)

        -- Adjust the difference
        let this :: Int64
            this = our - timeStamp (evTime ev)

        let diff'
              | abs diff < abs this = diff
              | otherwise = this

        yieldEvents our diff' (ev : evs)

  yieldEvents :: (MonadIO m) => Int64 -> Int64 -> [Event] -> PlanT (Is (Tick Event)) Event m ()
  yieldEvents our diff evs = do
    -- approximation of events time in our clock
    let approx e = timeStamp (evTime e) + diff
    let cutoff = our - interval'
    let (old, new) = partition (\e -> approx e < cutoff) evs
    traverse_ yield (sortBy (comparing evTime) old)
    loop diff new

  timeStamp :: Timestamp -> Int64
  timeStamp = fromIntegral

  timeSpec :: Clock.TimeSpec -> Int64
  timeSpec ts
    | ns >= 0 = fromIntegral ns
    | otherwise = 0
   where
    ns = Clock.sec ts * 1_000_000_000 + Clock.nsec ts

{- | Machine which checks that consecutive events are properly ordered.
Runs an effect on non-causal events.
-}
checkOrder ::
  (Monad m) =>
  (Event -> Event -> m ()) ->
  ProcessT m Event Event
checkOrder f = construct start
 where
  start = do
    e <- await
    yield e
    loop e

  loop e = do
    e' <- await
    when (evTime e' < evTime e) $ lift $ f e e'
    yield e'
    loop e'

-------------------------------------------------------------------------------
-- Filtering semaphores
-------------------------------------------------------------------------------

{- | A simple delimiting 'Moore' machine,
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
      UserMarker m -> do
        let mm'@(Moore s' _) = next m
        -- if current or next state is open (== True), emit the marker.
        when (s || s') $ yield e
        go mm'

      -- for other events, emit if the state is open.
      _ -> do
        when s $ yield e
        go mm
