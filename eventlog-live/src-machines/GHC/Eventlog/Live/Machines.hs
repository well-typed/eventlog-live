{-# LANGUAGE OverloadedStrings #-}

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
  withStartTime,

  -- ** Events with meta information
  WithStartTime (..),
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
  batchByTickList,
  liftTick,
  dropTick,
  onlyTick,

  -- * Event sorting
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
import Data.DList qualified as D
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List qualified as L
import Data.Machine (Is (..), MachineT (..), Moore (..), PlanT, Process, ProcessT, Step (..), await, construct, mapping, repeatedly, yield, (~>))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.RTS.Events (Event (..), EventInfo (..), HeapProfBreakdown (..), Timestamp)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import Numeric (showHex)
import System.Clock qualified as Clock
import System.IO (Handle, hWaitForInput)
import System.IO qualified as IO
import System.IO.Error (isEOFError)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
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
-- Values with start time

data WithStartTime a = WithStartTime
  { value :: !a
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  }
  deriving (Functor, Show)

-------------------------------------------------------------------------------
-- Spans

-------------------------------------------------------------------------------
-- Metrics

data Metric a = Metric
  { value :: !a
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  , maybeStartTimeUnixNano :: !(Maybe Timestamp)
  , attr :: [Attr]
  }
  deriving (Functor, Show)

mkMetric ::
  WithStartTime Event ->
  v ->
  [Attr] ->
  Metric v
mkMetric i v attr =
  Metric
    { value = v
    , maybeTimeUnixNano = (i.value.evTime +) <$> i.maybeStartTimeUnixNano
    , maybeStartTimeUnixNano = i.maybeStartTimeUnixNano
    , attr = attr
    }

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
-- Start time

{- | Uses the provided getter and setter to set an absolute timestamp for every
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
        | WallClockTime{..} <- ev.evSpec -> do
            -- This will start overflowing on Sunday, 21 July 2554 23:34:33, UTC.
            let !startTimeNs = sec * 1_000_000_000 + fromIntegral nsec
            -- We do not re-emit the `WallClockTime` event.
            continue startTimeNs
        | otherwise ->
            yield (WithStartTime ev Nothing) >> start
  continue startTimeNs =
    mappingPlan (\ev -> WithStartTime ev (Just startTimeNs))

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
        | HeapAllocated{..} <- i.value.evSpec ->
            yield $
              mkMetric i allocBytes $
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
        | HeapSize{..} <- i.value.evSpec -> do
            yield $
              mkMetric i sizeBytes $
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
        | BlocksSize{..} <- i.value.evSpec -> do
            yield $
              mkMetric i blocksSize $
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
        | HeapLive{..} <- i.value.evSpec -> do
            yield $
              mkMetric i liveBytes $
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
        | MemReturn{..} <- i.value.evSpec -> do
            yield $
              mkMetric i MemReturnData{..} $
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
      HeapProfBegin{..}
        | isLeft eitherShouldWarnOrHeapProfBreakdown ->
            go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
      -- Announces the arguments with which the program was called.
      -- This *may* include RTS options, which can be used to determine the
      -- heap profile breakdown for code compiled with GHC <9.14.
      ProgramArgs{..}
        | isLeft eitherShouldWarnOrHeapProfBreakdown
        , Just heapProfBreakdown <- findHeapProfBreakdown args ->
            go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
      -- Announces an info table entry.
      InfoTableProv{..}
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
      HeapProfSampleBegin{..} ->
        go st{heapProfSampleEraStack = heapProfSampleEra : heapProfSampleEraStack}
      -- Announces the end of a heap profile sample.
      HeapProfSampleEnd{..} ->
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
      HeapProfSampleString{..}
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
              mkMetric i heapProfResidency $
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

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

mappingPlan :: (a -> b) -> PlanT (Is a) b m a
mappingPlan f = forever (await >>= \a -> yield (f a))

warnf :: (IO.HPrintfType r) => String -> r
warnf = IO.hPrintf IO.stderr
