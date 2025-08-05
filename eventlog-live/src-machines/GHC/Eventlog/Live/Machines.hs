module GHC.Eventlog.Live.Machines (
  -- * Event Processing
  sourceHandleWait,
  fileSink,
  decodeEvent,
  defaultChunkSizeBytes,

  -- * Batch Event Processing
  Tick (..),
  batchByTick,
  liftTick,
  dropTick,
  onlyTick,
  sourceHandleBatch,
  fileSinkBatch,
  decodeEventBatch,

  -- * Timestamps
  setStartTime,
  tryMakeAbsolute,

  -- * Event Order
  sortEventsUpTo,
  handleOutOfOrderEvents,

  -- * Delimiting
  between,
  delimit,
) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.List qualified as L
import Data.Machine (Is (..), MachineT (..), Moore (..), PlanT, Process, ProcessT, Step (..), await, construct, repeatedly, yield, (~>))
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.RTS.Events (Event (..), EventInfo (..), Timestamp)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import System.Clock qualified as Clock
import System.IO (Handle, hWaitForInput)
import System.IO.Error (isEOFError)

-------------------------------------------------------------------------------
-- Socket source

{- |
A source which waits for input using 'hWaitForInput',
produces 'Tick' events on timeout.
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
sourceHandleWait timeoutMs chunkSizeBytes handle =
  construct $ fix $ \loop -> do
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
        pure ()

-------------------------------------------------------------------------------
-- Socket source with batches

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
    startTimeMs <- liftIO getMonotonicTimeMs
    batch startTimeMs
  batch startTimeMs = waitForInput
   where
    getRemainingTimeMs = do
      currentTimeMs <- liftIO getMonotonicTimeMs
      pure $ (startTimeMs + batchIntervalMs) - currentTimeMs
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
Eventlog chunk size in bytes.
This should be equal to the page size.
-}
defaultChunkSizeBytes :: Int
defaultChunkSizeBytes = 4096

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

data Ready = Ready | NotReady | EOF

hWaitForInput' :: Handle -> Int -> IO Ready
hWaitForInput' handle timeoutMs =
  catch (boolToReady <$> hWaitForInput handle timeoutMs) handleEOFError
 where
  boolToReady True = Ready
  boolToReady False = NotReady
  handleEOFError err
    | isEOFError err = pure EOF
    | otherwise = throwIO err

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
dropTick =
  repeatedly $
    await >>= \case
      Item a -> yield a
      Tick -> pure ()

onlyTick :: Process (Tick a) ()
onlyTick =
  repeatedly $
    await >>= \case
      Tick -> yield ()
      Item{} -> pure ()

--------------------------------------------------------------------------------
-- Lift a machine to a machine that passes on ticks unchanged

{- |

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

-------------------------------------------------------------------------------
-- Decoding events

{- |
Parse 'Event's from a stream of 'BS.ByteString' chunks with ticks.

Throws 'DecodeError' on error.
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

{- | Uses the provided getter and setter to set an absolute timestamp for every
  event issued /after/ the `WallClockTime` event. The provided setter should
  /get/ the old relative timestamp and should /set/ the new absolute timestamp.

  This machine swallows the first and only `WallClockTime` event.
-}
setStartTime ::
  forall m i o.
  (Monad m) =>
  -- | Getter that gets event info from the input.
  (i -> EventInfo) ->
  -- | Setter that adds the start time timestamp.
  (i -> Maybe Timestamp -> o) ->
  ProcessT m i o
setStartTime getEvSpec setStartTimeNs = construct start
 where
  start =
    await >>= \case
      ev
        -- The `WallClockTime` event announces the wall-clock time at which the
        -- process was started.
        | WallClockTime{..} <- getEvSpec ev -> do
            -- This will start overflowing on Sunday, 21 July 2554 23:34:33, UTC.
            let !startTimeNs = sec * 1_000_000_000 + fromIntegral nsec
            -- We do not re-emit the `WallClockTime` event.
            continue startTimeNs
        | otherwise ->
            yield (setStartTimeNs ev Nothing) >> start
  continue startTimeNs = mappingPlan (\ev -> setStartTimeNs ev (Just startTimeNs))

tryMakeAbsolute :: Event -> Maybe Timestamp -> Event
tryMakeAbsolute ev@Event{..} startTimeNs =
  ev {evTime = maybe evTime (evTime +) startTimeNs}

-------------------------------------------------------------------------------
-- Sorting the eventlog event stream

{- | Buffer and reorder 'Event's to hopefully achieve
monotonic, causal stream of 'Event's.
-}
sortEventsUpTo ::
  (MonadIO m) =>
  -- | Interval in nanoseconds.
  Word64 ->
  ProcessT m (Tick Event) Event
sortEventsUpTo flushoutIntervalNs = construct start
 where
  -- Interval to wait for cutoff, 1.5 times the flushout interval
  cutoffIntervalNs :: Int64
  cutoffIntervalNs = fromIntegral $ flushoutIntervalNs + flushoutIntervalNs `div` 2

  start :: (MonadIO m) => PlanT (Is (Tick Event)) Event m ()
  start = do
    mev <- await
    case mev of
      Tick -> start
      Item ev -> do
        our <- liftIO (timeSpec <$> Clock.getTime Clock.Monotonic)
        loop (our - timeStampNs (evTime ev)) [ev]

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
            this = our - timeStampNs (evTime ev)

        let diff'
              | abs diff < abs this = diff
              | otherwise = this

        yieldEvents our diff' (ev : evs)

  yieldEvents :: (MonadIO m) => Int64 -> Int64 -> [Event] -> PlanT (Is (Tick Event)) Event m ()
  yieldEvents our diff evs = do
    -- approximation of events time in our clock
    let approx e = timeStampNs (evTime e) + diff
    let cutoff = our - cutoffIntervalNs
    let (old, new) = L.partition (\e -> approx e < cutoff) evs
    traverse_ yield (L.sortBy (comparing evTime) old)
    loop diff new

  timeStampNs :: Timestamp -> Int64
  timeStampNs = fromIntegral

  timeSpec :: Clock.TimeSpec -> Int64
  timeSpec ts
    | ns >= 0 = ns
    | otherwise = 0
   where
    ns = Clock.sec ts * 1_000_000_000 + Clock.nsec ts

{- |
Machine which checks that consecutive events are properly ordered.
Runs an effect on non-causal events.
-}
handleOutOfOrderEvents ::
  (Monad m) =>
  (Event -> Event -> m ()) ->
  ProcessT m Event Event
handleOutOfOrderEvents cbOutOfOrderEvents = construct start
 where
  start = do
    e <- await
    yield e
    loop e

  loop e = do
    e' <- await
    when (evTime e' < evTime e) . lift $
      cbOutOfOrderEvents e e'
    yield e'
    loop e'

-------------------------------------------------------------------------------
-- Filtering semaphores

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

-------------------------------------------------------------------------------
-- Internal Helpers

mappingPlan :: (a -> b) -> PlanT (Is a) b m a
mappingPlan f = forever (await >>= \a -> yield (f a))
