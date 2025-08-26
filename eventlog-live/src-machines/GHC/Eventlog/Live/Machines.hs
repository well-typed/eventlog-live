{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
  tryGetTimeUnixNano,
  withStartTime,
  dropStartTime,

  -- ** Capability Usage
  CapabilityUser (..),
  prettyCapabilityUser,
  prettyCapabilityUserAsCategory,
  CapabilityUsageSpan (..),
  processCapabilityUsage,
  metricFromCapabilityUsageSpan,
  processCapabilityUsageSpans,

  -- ** Thread labels
  ThreadLabel (..),
  processThreadLabels,

  -- ** Thread State Spans
  ThreadState (..),
  prettyThreadState,
  ThreadStateSpan (..),
  metricFromThreadStateSpan,
  processThreadStateSpans,

  -- ** Metrics
  Metric (..),
  AttrKey,
  AttrValue (..),
  Attr,

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
  batchListToTick,
  batchByTickList,
  liftTick,
  dropTick,
  onlyTick,

  -- * Event sorting
  sortByBatch,
  sortByBatchTick,
  sortEventsUpTo,
  handleOutOfOrderEvents,

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
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.DList qualified as D
import Data.Either (isLeft)
import Data.Foldable (for_, traverse_)
import Data.Function (fix, on)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable (..), hashUsing)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List qualified as L
import Data.Machine (Is (..), MachineT (..), Moore (..), PlanT, Process, ProcessT, Step (..), await, construct, mapping, repeatedly, yield, (~>))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Semigroup (Max (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.RTS.Events (Event (..), EventInfo, HeapProfBreakdown (..), ThreadId, ThreadStopStatus (..), Timestamp)
import GHC.RTS.Events qualified as E
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import Numeric (showHex)
import System.Clock qualified as Clock
import System.IO (Handle, hWaitForInput)
import System.IO qualified as IO
import System.IO.Error (isEOFError)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
import Text.Printf (printf)
import Text.Printf qualified as IO
import Text.Read (readMaybe)
import Text.Read.Lex (readHexP)

-------------------------------------------------------------------------------
-- Reading from the socket
-------------------------------------------------------------------------------

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

data Ready = Ready | NotReady | EOF

hWaitForInput' :: Handle -> Int -> IO Ready
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

data Tick a = Item !a | Tick
  deriving (Eq, Functor, Foldable, Traversable, Show)

batchByTick :: (Monoid a) => Process (Tick a) a
batchByTick = construct start
 where
  start = batch mempty
  batch acc =
    await >>= \case
      Item a -> batch (a <> acc)
      Tick -> yield acc >> start

batchListToTick :: Process [a] (Tick a)
batchListToTick = repeatedly go
 where
  go = await >>= \xs -> for_ xs (yield . Item) >> yield Tick

batchByTickList :: Process (Tick a) [a]
batchByTickList =
  mapping (fmap D.singleton)
    ~> batchByTick
    ~> mapping D.toList

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

-------------------------------------------------------------------------------
-- Decoding
-------------------------------------------------------------------------------

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
If the event has a start time, return `Just` the time of the event in number of
nanoseconds since the Unix epoch. Otherwise, return `Nothing`.
-}
tryGetTimeUnixNano :: WithStartTime Event -> Maybe Timestamp
tryGetTimeUnixNano i = (i.value.evTime +) <$> i.maybeStartTimeUnixNano

{- |
Uses the provided getter and setter to set an absolute timestamp for every
event issued /after/ the `WallClockTime` event. The provided setter should
/get/ the old relative timestamp and should /set/ the new absolute timestamp.

This machine swallows the first and only `WallClockTime` event.
-}
withStartTime :: Process Event (WithStartTime Event)
withStartTime = construct start
 where
  start =
    await >>= \case
      ev
        -- The `WallClockTime` event announces the wall-clock time at which the
        -- process was started.
        | E.WallClockTime{..} <- ev.evSpec -> do
            -- This will start overflowing on Sunday, 21 July 2554 23:34:33, UTC.
            let !startTimeNs = sec * 1_000_000_000 + fromIntegral nsec
            -- We do not re-emit the `WallClockTime` event.
            continue startTimeNs
        | otherwise ->
            yield (WithStartTime ev Nothing) >> start
  continue startTimeNs =
    mappingPlan (\ev -> WithStartTime ev (Just startTimeNs))

dropStartTime :: Process (WithStartTime a) a
dropStartTime = mapping (.value)

-------------------------------------------------------------------------------
-- Metrics

data Metric a = Metric
  { value :: !a
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  , attr :: [Attr]
  }
  deriving (Functor, Show)

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

-------------------------------------------------------------------------------
-- Attributes

type AttrKey =
  Text

data AttrValue
  = AttrInt !Int
  | AttrInt8 !Int8
  | AttrInt16 !Int16
  | AttrInt32 !Int32
  | AttrInt64 !Int64
  | AttrWord !Word
  | AttrWord8 !Word8
  | AttrWord16 !Word16
  | AttrWord32 !Word32
  | AttrWord64 !Word64
  | AttrDouble !Double
  | AttrText !Text
  | AttrNull
  deriving (Show)

class IsAttrValue v where
  toAttrValue :: v -> AttrValue

instance IsAttrValue AttrValue where
  toAttrValue :: AttrValue -> AttrValue
  toAttrValue = id
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int where
  toAttrValue :: Int -> AttrValue
  toAttrValue = AttrInt
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int8 where
  toAttrValue :: Int8 -> AttrValue
  toAttrValue = AttrInt8
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int16 where
  toAttrValue :: Int16 -> AttrValue
  toAttrValue = AttrInt16
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int32 where
  toAttrValue :: Int32 -> AttrValue
  toAttrValue = AttrInt32
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int64 where
  toAttrValue :: Int64 -> AttrValue
  toAttrValue = AttrInt64
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word where
  toAttrValue :: Word -> AttrValue
  toAttrValue = AttrWord
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word8 where
  toAttrValue :: Word8 -> AttrValue
  toAttrValue = AttrWord8
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word16 where
  toAttrValue :: Word16 -> AttrValue
  toAttrValue = AttrWord16
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word32 where
  toAttrValue :: Word32 -> AttrValue
  toAttrValue = AttrWord32
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word64 where
  toAttrValue :: Word64 -> AttrValue
  toAttrValue = AttrWord64
  {-# INLINE toAttrValue #-}

instance IsAttrValue Double where
  toAttrValue :: Double -> AttrValue
  toAttrValue = AttrDouble
  {-# INLINE toAttrValue #-}

instance IsAttrValue String where
  toAttrValue :: String -> AttrValue
  toAttrValue = AttrText . T.pack
  {-# INLINE toAttrValue #-}

instance IsAttrValue Text where
  toAttrValue :: Text -> AttrValue
  toAttrValue = AttrText
  {-# INLINE toAttrValue #-}

instance (IsAttrValue v) => IsAttrValue (Maybe v) where
  toAttrValue :: Maybe v -> AttrValue
  toAttrValue = maybe AttrNull toAttrValue
  {-# INLINE toAttrValue #-}

type Attr = (AttrKey, AttrValue)

(~=) :: (IsAttrValue v) => AttrKey -> v -> Attr
k ~= v = (ak, av)
 where
  !ak = k
  !av = toAttrValue v
{-# INLINE (~=) #-}

-------------------------------------------------------------------------------
-- Thread events
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Capability Usage

newtype CapabilityUsageSpans = CapabilityUsageSpans
  { capUsageSpanMap :: HashMap Int CapabilityUsageSpan
  }

processCapabilityUsage ::
  Process (WithStartTime CapabilityUsageSpan) (Metric Timestamp)
processCapabilityUsage = construct $ go CapabilityUsageSpans{capUsageSpanMap = mempty}
 where
  go st =
    await >>= \i -> do
      let updateCapabilityUsage' = updateCapabilityUsage i
      capUsageSpanMap' <- M.alterF updateCapabilityUsage' i.value.cap st.capUsageSpanMap
      go st{capUsageSpanMap = capUsageSpanMap'}

{- |
Internal helper. Update the capability usage counts.
-}
updateCapabilityUsage ::
  forall k m.
  WithStartTime CapabilityUsageSpan ->
  Maybe CapabilityUsageSpan ->
  PlanT k (Metric Timestamp) m (Maybe CapabilityUsageSpan)
updateCapabilityUsage i maybeCapabilityUsage = do
  -- Calculate idle duration delta since previous span.
  let maybeIdleDurationDelta
        -- If there is a previous span...
        | Just capabilityUsageSpan <- maybeCapabilityUsage
        , capabilityUsageSpan.endTimeUnixNano < i.value.startTimeUnixNano =
            -- ...return the time between the end of that span and the start of the new span.
            Just (i.value.startTimeUnixNano - capabilityUsageSpan.endTimeUnixNano)
        -- If there is no previous span...
        | Nothing <- maybeCapabilityUsage
        , Just startTimeUnixNano <- i.maybeStartTimeUnixNano
        , startTimeUnixNano < i.value.startTimeUnixNano =
            -- ...return the time between the start of the process and the start of the new span.
            Just (i.value.startTimeUnixNano - startTimeUnixNano)
        -- Otherwise, return nothing.
        | otherwise = Nothing

  -- Publish the calculated idle duration delta.
  for_ maybeIdleDurationDelta $ \idleDurationDelta ->
    yield
      Metric
        { value = idleDurationDelta
        , maybeTimeUnixNano = Just i.value.startTimeUnixNano
        , maybeStartTimeUnixNano = i.maybeStartTimeUnixNano
        , attr =
            [ "capability" ~= i.value.cap
            , "category" ~= prettyCapabilityUserAsCategory Nothing
            ]
        }

  -- Calculate active duraction delta for new span.
  let activeDuractionDelta =
        i.value.endTimeUnixNano - i.value.startTimeUnixNano

  -- Publish the calculated active duration delta.
  yield
    Metric
      { value = activeDuractionDelta
      , maybeTimeUnixNano = Just i.value.startTimeUnixNano
      , maybeStartTimeUnixNano = i.maybeStartTimeUnixNano
      , attr =
          [ "capability" ~= i.value.cap
          , "category" ~= prettyCapabilityUserAsCategory (Just i.value.capUser)
          , "user" ~= prettyCapabilityUser i.value.capUser
          ]
      }

  -- Return the updated state.
  pure $ Just i.value

prettyCapabilityUserAsCategory :: Maybe CapabilityUser -> Text
prettyCapabilityUserAsCategory = \case
  Nothing -> "IDLE"
  Just GCThread -> "GC"
  Just (MutatorThread _thread) -> "MUTATOR"

{- |
A span representing the usage of a capability.
-}
data CapabilityUsageSpan = CapabilityUsageSpan
  { spanId :: !Word64
  , cap :: !Int
  , capUser :: !CapabilityUser
  , startTimeUnixNano :: !Timestamp
  , endTimeUnixNano :: !Timestamp
  }

{- |
The user of a capability. Either a mutator thread or a garbage collection thread.
-}
data CapabilityUser
  = MutatorThread !ThreadId
  | GCThread
  deriving stock (Eq, Show)

toMutatorThreadId :: CapabilityUser -> Maybe ThreadId
toMutatorThreadId = \case
  MutatorThread thread -> Just thread
  GCThread -> Nothing

instance Hashable CapabilityUser where
  hashWithSalt :: Int -> CapabilityUser -> Int
  hashWithSalt = hashUsing toMutatorThreadId

{- |
Pretty-print a `CapabilityUser` as either the thread ID or "GC".
-}
prettyCapabilityUser :: CapabilityUser -> Text
prettyCapabilityUser = \case
  MutatorThread thread -> T.pack . show $ thread
  GCThread -> "GC"

{- |
Convert a `CapabilityUsageSpan` to a `Metric`.
-}
metricFromCapabilityUsageSpan :: Process (WithStartTime CapabilityUsageSpan) (Metric Double)
metricFromCapabilityUsageSpan = repeatedly go
 where
  go =
    await >>= \i -> do
      let CapabilityUsageSpan{..} = i.value
      -- Start event
      yield
        Metric
          { value = 1.0
          , maybeTimeUnixNano = Just startTimeUnixNano
          , maybeStartTimeUnixNano = i.maybeStartTimeUnixNano
          , attr = ["evCap" ~= cap, "thread" ~= prettyCapabilityUser capUser]
          }
      -- End event
      yield
        Metric
          { value = 0.0
          , maybeTimeUnixNano = Just endTimeUnixNano
          , maybeStartTimeUnixNano = i.maybeStartTimeUnixNano
          , attr = ["evCap" ~= cap, "thread" ~= prettyCapabilityUser capUser]
          }

{-
Internal helper. Partial `CapabilityUsageSpan` used in `CapabilityUsageSpanStarts`.
-}
data CapabilityUsageStart = CapabilityUsageStart
  { startTimeUnixNano :: !Timestamp
  , capUser :: !CapabilityUser
  }

{-
Internal helper. State used by `processCapabilityUsageSpans`.
-}
data CapabilityUsageSpanStarts = CapabilityUsageSpanStarts
  { capUsageSpanStartsMap :: HashMap Int CapabilityUsageStart
  , nextSpanId :: Word64
  }

{- |
Process thread events and yield capability usage spans.
-}
processCapabilityUsageSpans ::
  forall m.
  (MonadIO m) =>
  ProcessT m (WithStartTime Event) (WithStartTime CapabilityUsageSpan)
processCapabilityUsageSpans = construct $ go CapabilityUsageSpanStarts{capUsageSpanStartsMap = mempty, nextSpanId = 1}
 where
  go st@CapabilityUsageSpanStarts{..} =
    await >>= \case
      i
        | Just cap <- i.value.evCap -> case i.value.evSpec of
            -- Register the start of a `MutatorThread` running on the capability
            E.RunThread{thread} -> do
              let startCapabilityUsage' = startCapabilityUsage i cap (MutatorThread thread)
              capUsageSpanStartsMap' <- M.alterF startCapabilityUsage' cap capUsageSpanStartsMap
              go st{capUsageSpanStartsMap = capUsageSpanStartsMap'}
            -- Register the end of a `MutatorThread` running on the capability
            E.StopThread{thread} -> do
              let stopCapabilityUsage' = stopCapabilityUsage i cap nextSpanId (MutatorThread thread)
              (nextSpanId', capUsageSpanStartsMap') <- (.unPlanTWriter) $ M.alterF stopCapabilityUsage' cap capUsageSpanStartsMap
              go st{capUsageSpanStartsMap = capUsageSpanStartsMap', nextSpanId = nextSpanId'}
            -- Register the start of a `GCThread` running on the capability.
            E.StartGC{} -> do
              let startCapabilityUsage' = startCapabilityUsage i cap GCThread
              capUsageSpanStartsMap' <- M.alterF startCapabilityUsage' cap capUsageSpanStartsMap
              go st{capUsageSpanStartsMap = capUsageSpanStartsMap'}
            -- Register the end of a `MutatorThread` running on the capability.
            E.EndGC{} -> do
              let stopCapabilityUsage' = stopCapabilityUsage i cap nextSpanId GCThread
              (nextSpanId', capUsageSpanStartsMap') <- (.unPlanTWriter) $ M.alterF stopCapabilityUsage' cap capUsageSpanStartsMap
              go st{capUsageSpanStartsMap = capUsageSpanStartsMap', nextSpanId = nextSpanId'}
            _otherwise -> go st
        | otherwise -> go st

{- |
Internal helper. Process a capability usage start event.
-}
startCapabilityUsage ::
  (MonadIO m) =>
  WithStartTime Event ->
  Int ->
  CapabilityUser ->
  Maybe CapabilityUsageStart ->
  PlanT k i m (Maybe CapabilityUsageStart)
startCapabilityUsage i cap capUser maybeCapabilityUsageStart = do
  -- If a usage was already open, issue a warning...
  sequence_ $ do
    capabilityUsageStart <- maybeCapabilityUsageStart
    pure . liftIO $
      warnf
        "Warning: Received %s event for capability %d, but previous event for that capability was %s.\n"
        (showCapabilityUserAsStartEvent capUser)
        cap
        (showCapabilityUserAsStartEvent capabilityUsageStart.capUser)
  -- Either way, return the new usage...
  pure $ do
    startTimeUnixNano <- tryGetTimeUnixNano i
    pure CapabilityUsageStart{..}

{- |
Internal helper. Process a capability usage stop event.
-}
stopCapabilityUsage ::
  (MonadIO m) =>
  WithStartTime Event ->
  Int ->
  Word64 ->
  CapabilityUser ->
  Maybe CapabilityUsageStart ->
  PlanTWriter Word64 k (WithStartTime CapabilityUsageSpan) m (Maybe x)
stopCapabilityUsage i cap nextSpanId endCapUser = \case
  Just CapabilityUsageStart{..}
    -- If the currently open span matches, yield a usage span...
    | capUser == endCapUser
    , Just endTimeUnixNano <- tryGetTimeUnixNano i -> PlanTWriter $ do
        yield . (<$ i) $
          CapabilityUsageSpan
            { spanId = nextSpanId
            , ..
            }
        pure (succ nextSpanId, Nothing)
    -- If the currently open span does not match, emit a warning...
    | otherwise -> PlanTWriter $ do
        liftIO $
          warnf
            "Warning: Received %s event for capability %d, but previous event for that capability was %s.\n"
            (showCapabilityUserAsStartEvent endCapUser)
            cap
            (showCapabilityUserAsStartEvent capUser)
        pure (nextSpanId, Nothing)
  -- If there is no currently open span, emit a warning...
  Nothing -> PlanTWriter $ do
    liftIO $
      warnf
        "Warning: Received %s event for capability %d, but there was no previous event for that capability.\n"
        (showCapabilityUserAsStartEvent endCapUser)
        cap
    pure (nextSpanId, Nothing)

{- |
Internal helper. Show the original `EventInfo` constructor name from which a `CapabilityUser` was derived.
-}
showCapabilityUserAsStartEvent :: CapabilityUser -> String
showCapabilityUserAsStartEvent = \case
  MutatorThread thread -> printf "RunThread{%d}" thread
  GCThread -> "StartGC"

-------------------------------------------------------------------------------
-- Thread Labels

data ThreadLabel
  = ThreadLabel
  { thread :: !ThreadId
  , threadlabel :: !Text
  , startTimeUnixNano :: !Timestamp
  }

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
  = Running
  | Blocked
  | Finished
  deriving (Eq, Show)

{- |
Pretty-print a thread state as "Running", "Blocked", or "Finished".
-}
prettyThreadState :: ThreadState -> Text
prettyThreadState = T.pack . show

{- |
A span representing the state of a mutator thread.
-}
data ThreadStateSpan
  = ThreadStateSpan
  { spanId :: !Word64
  , thread :: !ThreadId
  , threadState :: !ThreadState
  , startTimeUnixNano :: !Timestamp
  , endTimeUnixNano :: !Timestamp
  , cap :: !Int
  , event :: !Text
  , status :: !(Maybe Text)
  }
  deriving (Show)

{-
Internal helper. Partial `ThreadStateSpan` used in `ThreadStates`.
-}
data ThreadStateSpanStart
  = ThreadStateSpanStart
  { spanId :: Word64
  , startTimeUnixNano :: !Timestamp
  , threadState :: !ThreadState
  , prevEvSpec :: !EventInfo
  , cap :: !Int
  , event :: !Text
  , status :: !(Maybe Text)
  }
  deriving (Show)

{-
Internal helper. State used by `processThreadStateSpans`.
-}
data ThreadStates = ThreadStates
  { threadStates :: HashMap ThreadId ThreadStateSpanStart
  , nextSpanId :: Word64
  }
  deriving (Show)

{- |
Convert a `ThreadStateSpan` to a `Metric`.
-}
metricFromThreadStateSpan ::
  (MonadIO m) =>
  ProcessT m (WithStartTime ThreadStateSpan) (Metric Double)
metricFromThreadStateSpan = repeatedly go
 where
  go =
    await >>= \i -> do
      let ThreadStateSpan{..} = i.value
      case threadState of
        Running ->
          yield
            Metric
              { value = 1.0
              , maybeTimeUnixNano = Just startTimeUnixNano
              , maybeStartTimeUnixNano = Just startTimeUnixNano
              , attr = ["endTimeUnixNano" ~= endTimeUnixNano, "evCap" ~= cap, "event" ~= event, "status" ~= status, "thread" ~= thread]
              }
        Blocked ->
          yield
            Metric
              { value = 0.0
              , maybeTimeUnixNano = Just endTimeUnixNano
              , maybeStartTimeUnixNano = Just startTimeUnixNano
              , attr = ["endTimeUnixNano" ~= endTimeUnixNano, "evCap" ~= cap, "event" ~= event, "status" ~= status, "thread" ~= thread]
              }
        Finished ->
          yield
            Metric
              { value = -1.0
              , maybeTimeUnixNano = Just endTimeUnixNano
              , maybeStartTimeUnixNano = Just startTimeUnixNano
              , attr = ["endTimeUnixNano" ~= endTimeUnixNano, "evCap" ~= cap, "event" ~= event, "status" ~= status, "thread" ~= thread]
              }

{- |
Process thread events and yield thread state spans.
-}
processThreadStateSpans ::
  (MonadIO m) =>
  ProcessT m (WithStartTime Event) (WithStartTime ThreadStateSpan)
processThreadStateSpans = construct $ go ThreadStates{threadStates = mempty, nextSpanId = 1}
 where
  go st =
    await >>= \i -> case i.value.evSpec of
      -- Marks the creation of a Haskell thread.
      E.CreateThread{} ->
        go =<< transitionTo st i Blocked
      -- The indicated thread has started running.
      E.RunThread{} ->
        go =<< transitionTo st i Running
      -- The indicated thread has stopped running,
      -- for the reason given by the status field.
      E.StopThread{status} -> case status of
        ThreadFinished ->
          go =<< transitionTo st i Finished
        _otherwise ->
          go =<< transitionTo st i Blocked
      -- The indicated thread has been migrated to a new capability.
      E.MigrateThread{} ->
        go =<< transitionTo st i Blocked
      _otherwise -> go st

{- |
Transition the `ThreadStates` to the target thread state, and
end any current thread span and `yield` the resulting `ThreadStateSpan`.

__Warning__: This function is partial and only defined for events with a `thread` field.
-}
transitionTo ::
  (MonadIO m) =>
  ThreadStates ->
  WithStartTime Event ->
  ThreadState ->
  PlanT k (WithStartTime ThreadStateSpan) m ThreadStates
transitionTo st i targetThreadState
  | thread <- i.value.evSpec.thread
  , Just cap <- i.value.evCap
  , Just timeUnixNano <- tryGetTimeUnixNano i = do
      let
        transitionTo' maybeThreadStateSpanStart = PlanTWriter $ do
          -- End the current thread span, if any:
          for_ maybeThreadStateSpanStart $ \ThreadStateSpanStart{..} ->
            yield . (<$ i) $
              ThreadStateSpan
                { endTimeUnixNano = timeUnixNano
                , ..
                }
          -- Validate the thread state transition:
          let threadStateTransition =
                ThreadStateTransition
                  { maybePrevEvSpec = (.prevEvSpec) <$> maybeThreadStateSpanStart
                  , maybeStartTimeUnixNano = (.startTimeUnixNano) <$> maybeThreadStateSpanStart
                  , maybeStartState = (.threadState) <$> maybeThreadStateSpanStart
                  , evSpec = i.value.evSpec
                  , endTimeUnixNano = timeUnixNano
                  , endState = targetThreadState
                  }
          unless (isValidThreadStateTransition threadStateTransition) . liftIO $
            warnf "Warning: Invalid thread state transition:\n\n  %s\n" $
              show threadStateTransition

          -- If the target thread state is "Finished"...
          if targetThreadState == Finished
            then do
              -- ...immediately yield a zero-width span...
              yield . (<$ i) $
                ThreadStateSpan
                  { spanId = st.nextSpanId
                  , thread = thread
                  , threadState = targetThreadState
                  , startTimeUnixNano = timeUnixNano
                  , endTimeUnixNano = timeUnixNano
                  , event = eventFromEventInfo i.value.evSpec
                  , status = statusFromEventInfo i.value.evSpec
                  , ..
                  }
              -- ...and delete the thread state from the thread state map.
              pure (succ st.nextSpanId, Nothing)
            else do
              --- Otherwise, start a new thread span...
              let threadSpanStart =
                    ThreadStateSpanStart
                      { startTimeUnixNano = timeUnixNano
                      , spanId = st.nextSpanId
                      , prevEvSpec = i.value.evSpec
                      , event = eventFromEventInfo i.value.evSpec
                      , status = statusFromEventInfo i.value.evSpec
                      , threadState = targetThreadState
                      , ..
                      }
              -- ...and update the thread state from the thread state map.
              pure (succ st.nextSpanId, Just threadSpanStart)
      (nextSpanId', threadStates') <- (.unPlanTWriter) $ M.alterF transitionTo' thread st.threadStates
      pure st{threadStates = threadStates', nextSpanId = nextSpanId'}
  | otherwise = pure st

{- |
Internal helper. Create an attribute that holds the threading event name.

__Warning:__
This works by truncating the `String` produced by `show`.
Hence, it will work for _any_ `EventInfo`, not just for threading events.
-}
eventFromEventInfo :: EventInfo -> Text
eventFromEventInfo = T.pack . takeWhile (not . isSpace) . show

{- |
Internal helper. Create an attribute that holds the `ThreadStopStatus` or null.
-}
statusFromEventInfo :: EventInfo -> Maybe Text
statusFromEventInfo = \case
  E.StopThread{status} -> Just . T.pack . show $ status
  _otherwise -> Nothing

-------------------------------------------------------------------------------
-- Thread State Transition Validation

{- |
Type that represents transitions between thread states.
For validation purposes only.
-}
data ThreadStateTransition
  = ThreadStateTransition
  { maybePrevEvSpec :: !(Maybe EventInfo)
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  , maybeStartState :: !(Maybe ThreadState)
  , evSpec :: !EventInfo
  , endTimeUnixNano :: !Timestamp
  , endState :: !ThreadState
  }

instance Show ThreadStateTransition where
  show :: ThreadStateTransition -> String
  show ThreadStateTransition{..} =
    printf
      "[ %s ]-- %s --[ %s ]--> %s\n"
      (maybe "Nothing" (("Just " <>) . showLabel) maybePrevEvSpec)
      (show (maybeStartTimeUnixNano, maybeStartState))
      (showLabel evSpec)
      (show (endTimeUnixNano, endState))
   where
    showLabel :: EventInfo -> String
    showLabel E.StopThread{..} = printf "StopThread{%s}" (show status)
    showLabel evSpec = takeWhile (not . isSpace) . show $ evSpec

{- |
Validate a thread state transition. Validation check that:

* The first event precedes the second event.
* The transition follows the model.
-}
isValidThreadStateTransition :: ThreadStateTransition -> Bool
isValidThreadStateTransition ThreadStateTransition{..} =
  isValidState maybeStartState evSpec endState && isValidTime maybeStartTimeUnixNano endTimeUnixNano
 where
  isValidState :: Maybe ThreadState -> EventInfo -> ThreadState -> Bool
  isValidState Nothing E.CreateThread{} Blocked = True
  isValidState (Just Blocked) E.RunThread{} Running = True
  isValidState (Just Running) E.StopThread{status} Finished = isThreadFinished status
  isValidState (Just Running) E.StopThread{status} Blocked = not (isThreadFinished status)
  isValidState (Just Blocked) E.MigrateThread{} Blocked = True
  -- These ones are weird, but observed in practice:
  isValidState (Just Finished) E.RunThread{} Running = True
  isValidState (Just Running) E.RunThread{} Running = True
  isValidState (Just Blocked) E.CreateThread{} Blocked = True
  isValidState (Just Blocked) E.StopThread{status} Finished = isThreadFinished status
  isValidState (Just Blocked) E.StopThread{status} Blocked = not (isThreadFinished status)
  -- These ones occur whenever an initial segment of the eventlog is dropped:
  isValidState Nothing _ _ = True
  isValidState _ _ _ = False

  isValidTime :: Maybe Timestamp -> Timestamp -> Bool
  isValidTime maybeStartTime endTime
    | Just startTime <- maybeStartTime = startTime <= endTime
    | otherwise = True

  isThreadFinished :: ThreadStopStatus -> Bool
  isThreadFinished ThreadFinished = True
  isThreadFinished _ = False

-------------------------------------------------------------------------------
-- Heap events
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HeapAllocated

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
-- HeapLive

data MemReturnData = MemReturnData
  { current :: !Word32
  , needed :: !Word32
  , returned :: !Word32
  }

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

newtype InfoTablePtr = InfoTablePtr Word64
  deriving newtype (Eq, Hashable, Ord)

instance Show InfoTablePtr where
  showsPrec :: Int -> InfoTablePtr -> ShowS
  showsPrec _ (InfoTablePtr ptr) =
    showString "0x" . showHex ptr

instance Read InfoTablePtr where
  readsPrec :: Int -> ReadS InfoTablePtr
  readsPrec _ = readP_to_S (InfoTablePtr <$> (P.string "0x" *> readHexP))

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

data HeapProfSampleState = HeapProfSampleState
  { eitherShouldWarnOrHeapProfBreakdown :: Either Bool HeapProfBreakdown
  , infoTableMap :: HashMap InfoTablePtr InfoTable
  , heapProfSampleEraStack :: [Word64]
  }
  deriving (Show)

shouldTrackInfoTableMap :: Either Bool HeapProfBreakdown -> Bool
shouldTrackInfoTableMap (Left _shouldWarn) = True
shouldTrackInfoTableMap (Right HeapProfBreakdownInfoTable) = True
shouldTrackInfoTableMap _ = False

isHeapProfBreakdownInfoTable :: HeapProfBreakdown -> Bool
isHeapProfBreakdownInfoTable HeapProfBreakdownInfoTable = True
isHeapProfBreakdownInfoTable _ = False

processHeapProfSampleData ::
  (MonadIO m) =>
  Maybe HeapProfBreakdown ->
  ProcessT m (WithStartTime Event) (Metric Word64)
processHeapProfSampleData maybeHeapProfBreakdown =
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
            liftIO $
              warnEmptyHeapProfSampleEraStack heapProfSampleEra
            go st
          Just (currentEra, heapProfSampleEraStack') -> do
            unless (currentEra == heapProfSampleEra) . liftIO $
              warnMismatchedHeapProfSampleEraStack heapProfSampleEra currentEra
            go st{heapProfSampleEraStack = heapProfSampleEraStack'}
      -- Announces a heap profile sample.
      E.HeapProfSampleString{..}
        -- If there is no heap profile breakdown, issue a warning, then disable warnings.
        | Left True <- eitherShouldWarnOrHeapProfBreakdown -> do
            liftIO warnMissingHeapProfBreakdown
            go st{eitherShouldWarnOrHeapProfBreakdown = Left False, infoTableMap = mempty}
        -- If the heap profile breakdown is biographical, issue a warning, then disable warnings.
        | Right HeapProfBreakdownBiography <- eitherShouldWarnOrHeapProfBreakdown -> do
            liftIO $ warnUnsupportedHeapProfBreakdown HeapProfBreakdownBiography
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

-- NOTE: This scan is currently flawed, as it does not handle @-with-rtsopts@,
--       nor does it restrict its search to between @+RTS@ and @-RTS@ tags.
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

warnEmptyHeapProfSampleEraStack :: Word64 -> IO ()
warnEmptyHeapProfSampleEraStack =
  warnf
    "Warning: Eventlog closed era %d, but there is no current era."

warnMismatchedHeapProfSampleEraStack :: Word64 -> Word64 -> IO ()
warnMismatchedHeapProfSampleEraStack =
  warnf
    "Warning: Eventlog closed era %d, but the current era is era %d."

warnUnsupportedHeapProfBreakdown :: HeapProfBreakdown -> IO ()
warnUnsupportedHeapProfBreakdown heapProfBreakdown =
  warnf
    "Warning: Unsupported heap profile breakdown %s"
    (heapProfBreakdownShow heapProfBreakdown)

warnMissingHeapProfBreakdown :: IO ()
warnMissingHeapProfBreakdown =
  warnf
    "Warning: Cannot infer heap profile breakdown.\n\
    \         If your binary was compiled with a GHC version prior to 9.14,\n\
    \         you must also pass the heap profile type to this executable.\n\
    \         See: https://gitlab.haskell.org/ghc/ghc/-/commit/76d392a"

-------------------------------------------------------------------------------
-- Event stream sorting
-------------------------------------------------------------------------------

{-
Reorder events respecting ticks.

This function caches two batches worth of events, sorts them together,
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

sortByBatchTick :: (a -> Timestamp) -> Process (Tick a) (Tick a)
sortByBatchTick timestamp =
  mapping (fmap (: [])) ~> batchByTick ~> sortByBatch timestamp ~> batchListToTick

-------------------------------------------------------------------------------
-- Sorting the eventlog event stream

{- |
Buffer and reorder 'Event's to hopefully achieve
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
Internal helper. Variant of `mapping` for plans.
-}
mappingPlan :: (a -> b) -> PlanT (Is a) b m a
mappingPlan f = forever (await >>= \a -> yield (f a))

{- |
Internal helper. Variant of `IO.printf` to print warnings. Combines `IO.hPrintf` with `IO.stderr`.
-}
warnf :: (IO.HPrintfType r) => String -> r
warnf = IO.hPrintf IO.stderr

{- |
Internal helper. Combines the planning `Functor` @`PlanT` k o m@ with the writer `Functor` @(w,)@.
-}
newtype PlanTWriter w k o m a = PlanTWriter {unPlanTWriter :: PlanT k o m (w, a)}

instance Functor (PlanTWriter w k o m) where
  fmap :: (a -> b) -> PlanTWriter w k o m a -> PlanTWriter w k o m b
  fmap f (PlanTWriter m) = PlanTWriter (fmap (fmap f) m)
  {-# INLINE fmap #-}
