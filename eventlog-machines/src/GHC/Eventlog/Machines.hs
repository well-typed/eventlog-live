{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
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
  analyse,

  -- ** Event Order
  reorderEvents,
  checkOrder,

  -- ** Delimiting
  delimit,
  between,

  -- * Exceptions
  DecodeError (..),
) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_, for_)
import Data.Function (fix)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import Data.List (partition, sortBy)
import Data.Machine (Is, MachineT, PlanT, Process, ProcessT, await, construct, yield, repeatedly)
import Data.Machine.Moore (Moore (..))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.RTS.Events (
  Event (evSpec),
  EventInfo (UserMarker),
  Timestamp,
  evTime,
 )
import qualified GHC.RTS.Events.Analysis as EA
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import qualified System.Clock as Clock
import System.IO (Handle, hWaitForInput)
import System.IO.Error (isEOFError)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative((<|>)))

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
    go = await >>= \case
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

analyse ::
  (MonadIO m) =>
  (i -> Int) ->
  EA.Machine s i ->
  ProcessT m i (s, i)
analyse key m = construct (go IM.empty)
 where
  -- go :: (MonadIO m) => IntMap s -> PlanT (Is i) (s, i) m ()
  go st = await >>= \case
      i | EA.alpha m i -> do
          let handle ms = do
                -- The input state (initial if missing).
                let s   = fromMaybe (EA.initial m) ms
                -- The output state (Nothing if step fails).
                let ms' = rightToMaybe (EA.step m s i)
                -- The altered state in the map (deleted if final).
                let ms'_in_st'
                      | any (EA.final m) ms' = Nothing
                      | otherwise = ms' <|> Just s
                (ms', ms'_in_st')
          let (ms', st') = IM.alterF handle (key i) st
          for_ ms' $ \s' -> yield (s', i)
          go st'
        | otherwise -> go st

rightToMaybe :: Either e a -> Maybe a
rightToMaybe = either (const Nothing) Just

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
