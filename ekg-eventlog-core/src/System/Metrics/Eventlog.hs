{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}

module System.Metrics.Eventlog(
    Field(..)
  , FieldValue(..)
  , Line(..)
  , Store(..)
  , eventlogMetrics
  ) where

import qualified System.Metrics       as EKG
import qualified System.Metrics.Gauge (set)
import qualified System.Metrics.Gauge as EKG
import qualified System.Metrics.Label (set)
import qualified System.Metrics.Label as EKG
import GHC.RTS.Events.Incremental
import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO as IO
import System.IO (Handle)
import GHC.RTS.Events
import Control.Monad
-- import qualified GHC.Eventlog.Socket as EventLogSocket
import System.IO.Temp
import Control.Concurrent
import Control.Concurrent.Async
import Debug.Trace

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Word
import Data.IORef
import Data.Int
import Data.Text (Text)
import Data.Time
import Data.Fixed


data Field = Field Text FieldValue
  deriving Show

data FieldValue
    = FieldInt Int64
    | FieldFloat Double
    | FieldString Text
    | FieldBool Bool
  deriving Show

data Line = Line
    { measurementName :: !Text
    , timestamp :: !NominalDiffTime
    , fields :: ![Field]
    }
  deriving Show

data Store = Store
    { ekgStore :: EKG.Store
    , lineCont :: !(Line -> IO ())
    }

newStore :: (Line -> IO ()) -> IO Store
newStore k = Store <$> EKG.newStore <*> pure k

-------------------

data Gauge =
    Gauge
      EKG.Gauge
      Text
      (IORef Int64)
      (Line -> IO ())

data Label =
    Label
      EKG.Label
      Text
      (IORef Text)
      (Line -> IO ())

createGauge :: Text -> Store ->  IO Gauge
createGauge t (Store s k) = Gauge <$> EKG.createGauge t s <*> pure t <*> newIORef 0 <*> pure k

createLabel :: Text -> Store ->  IO Label
createLabel t (Store s k) = Label <$> EKG.createLabel t s <*> pure t <*> newIORef "" <*> pure k

modifyGauge :: (EKG.Gauge -> IO ()) -> (Int64 -> Int64) -> Timestamp -> Gauge -> IO ()
modifyGauge ekg f t (Gauge g n r k) = do
  ekg g
  new_val <- atomicModifyIORef' r (\n -> (f n, f n))
  k
    ( Line measurementName (nanoToDiffTime t)
        [ Field "value" (FieldInt new_val)
        , Field "measurement_name" (FieldString measurementName)
        ]
    )
  where
    nanoToDiffTime :: Timestamp -> NominalDiffTime
    nanoToDiffTime t = secondsToNominalDiffTime (MkFixed (fromIntegral t * 1_000))

    measurementName :: Text
    measurementName = "gauge." <> n

modifyLabel :: (EKG.Label -> IO ()) -> (Text -> Text) -> Timestamp -> Label -> IO ()
modifyLabel ekg f t (Label g n r k) = do
  ekg g
  new_val <- atomicModifyIORef' r (\n -> (f n, f n))
  k
    ( Line measurementName (nanoToDiffTime t)
        [ Field "value" (FieldString new_val)
        , Field "measurement_name" (FieldString measurementName)
        ]
    )
  where
    nanoToDiffTime :: Timestamp -> NominalDiffTime
    nanoToDiffTime t = secondsToNominalDiffTime (MkFixed (fromIntegral t * 1_000))

    measurementName :: Text
    measurementName = "label." <> n

incGauge :: Timestamp -> Gauge -> IO ()
incGauge = modifyGauge EKG.inc (+ 1)

setGauge :: Word64 -> Gauge -> Int64 -> IO ()
setGauge t g n = modifyGauge (\g -> System.Metrics.Gauge.set g n) (const n) t g

setLabel :: Word64 -> Label -> Text -> IO ()
setLabel t l v = modifyLabel (\l -> System.Metrics.Label.set l v) (const v) t l

-------------------

connectToEventlogSocket :: FilePath -> IO Handle
connectToEventlogSocket socketName = do
  s <- socket AF_UNIX Stream defaultProtocol
  connect s (SockAddrUnix socketName)
  h <- socketToHandle s IO.ReadMode
  IO.hSetBuffering h IO.NoBuffering
  return h


_testReadFromFile :: FilePath -> IO ()
_testReadFromFile fp = do
  h <- IO.openFile fp IO.ReadMode
  readEventlog h print

readLoop :: Handle -> Decoder Event -> IO (Event, Decoder Event)
readLoop h d =
  case d of
    Consume k -> do
      B.hGet h 10000 >>= readLoop h . k
    Produce a d -> return (a, d)
    Done {} -> error "Unexpectedly done"
    Error _ err -> error err


-- | Connect to the event log socket (if it exists) and start a thread that
-- processes incoming events.
readEventlog ::
     Handle
  -> (Event -> IO r)
  -- ^ Listener function that processes events.
  -> IO a
  -- ^ True if successfully connected to the event log socket.
readEventlog hdl handler = eLoop decodeEventLog
  where
    eLoop d = do
      (e, d') <- readLoop hdl d
      handler e
      eLoop d'

readEventlogSocket :: FilePath -> (Event -> IO r) -> IO ()
readEventlogSocket sockName k = do
  h <- connectToEventlogSocket sockName
  readEventlog h k

-- | Starts the eventlog socket and then listens for events, this will
-- block so you should spawn this into it's own thread.
eventlogMetrics :: FilePath -> (Line -> IO ()) -> IO (Async r, EKG.Store)
eventlogMetrics f k = do
    store <- newStore k
    st <- initEventLogState store
    putStrLn "starting eventlog listener"
    h' <- connectToEventlogSocket f
    a <- async $ readEventlog h' (processEvents store st)
    return (a, ekgStore store)

processEvents :: Store -> EventLogState -> Event -> IO ()
processEvents store el@EventLogState{..} ev@(Event t e _) =
  case e of
    WallClockTime c w n -> do
      let o = (w * 1_000_000_000 + fromIntegral n) - t
      print o
      writeIORef timeOffset (Just o)
    _ -> do
      mo <- readIORef timeOffset
      forM_ mo $ \o -> processEventsWithOffset o store el ev

-- | Events which we *need* the offset for
processEventsWithOffset :: Word64 -> Store -> EventLogState -> Event -> IO ()
processEventsWithOffset o store EventLogState{..} ev@(Event raw_t e _cap) =
  case e of
    -- These two events are a bit hard to understand because you might
    -- connect to the process when there are k threads already running so
    -- you will see more StopThread events than CreateThread events
    CreateThread {} -> incGauge t createdThreads
    StopThread _ ThreadFinished   -> incGauge t finishedThreads

    HeapLive _ nlb -> setGauge t liveBytes(fromIntegral nlb)
    HeapSize _ ns -> setGauge t heapSize (fromIntegral ns)
    BlocksSize _ ns -> setGauge t blocksSize (fromIntegral ns)
    MemReturn _ current needed returned -> do
      setGauge t currentMblocks (fromIntegral current)
      setGauge t neededMblocks (fromIntegral needed)
      setGauge t returnedMblocks (fromIntegral returned)
    StartGC {} -> gcPause (Left t)
    EndGC {} -> gcPause (Right t)

    -- Heap profiles

    -- Residency stats for a heap item (closure, info table, etc.)
    HeapProfSampleString {..} -> do
      -- Read the current stats
      heapStats <- readIORef heapAllocationStats

      -- Check if this item already has a gauge and create one if necessary
      g <- case M.lookup heapProfLabel heapStats of
        Just gauge -> return gauge
        Nothing    -> createGauge ("eventlog.heap_prof_sample." <> heapProfLabel) store

      -- Set the value of the gauge to the current residency and update the heap
      -- stats in the state
      setGauge t g (fromIntegral heapProfResidency)
      writeIORef heapAllocationStats $ M.insert heapProfLabel g heapStats

    -- User messages (e.g. from `traceEvent` and `traceMarker`)
    UserMessage {..} -> setLabel t userMessages msg
    UserMarker {..}  -> setLabel t userMarkers markername

    -- Ignore all other events
    e -> return ()
  where
    t = o + raw_t

initEventLogState st = do
  createdThreads <- createGauge "eventlog.created_threads" st
  finishedThreads <- createGauge "eventlog.finished_threads" st
  liveBytes <- createGauge "eventlog.live_bytes" st
  heapSize <- createGauge "eventlog.heap_size" st
  blocksSize <- createGauge "eventlog.blocks_size" st
  currentMblocks <- createGauge "eventlog.current_mblocks" st
  neededMblocks <- createGauge "eventlog.needed_mblocks" st
  returnedMblocks <- createGauge "eventlog.returned_mblocks" st
  gcPause <- initGCPause st
  heapAllocationStats <- newIORef M.empty
  userMessages <- createLabel "eventlog.user_messages" st
  userMarkers <- createLabel "eventlog.user_markers" st
  timeOffset <- newIORef Nothing
  return EventLogState{..}

initGCPause :: Store -> IO (Either Timestamp Timestamp -> IO ())
initGCPause st = do
  g <- createGauge "eventlog.gc_pause" st
  lastRead <- newIORef Nothing
  let
      go (Left tstart) (Right tend) = (Nothing, Just (tend - tstart))
      go _ x = (Just x, Nothing)

      func ett = do
        mt <- atomicModifyIORef lastRead (\lr ->
          case lr of
            Nothing -> (Just ett, Nothing)
            Just v  -> go v ett)
        let end_t = either id id ett
        -- Time is already correct by this point
        forM_ mt $ \t -> setGauge end_t g (fromIntegral t)
  return func

--data GcStatsState = GcStatsState

data EventLogState = EventLogState
    { createdThreads :: Gauge
    , finishedThreads :: Gauge
    , liveBytes :: Gauge
    , heapSize :: Gauge
    , blocksSize :: Gauge
    , currentMblocks :: Gauge
    , neededMblocks :: Gauge
    , returnedMblocks :: Gauge
    , gcPause :: Either Timestamp Timestamp -> IO ()

    , heapAllocationStats :: IORef (M.Map Text Gauge)

    -- User messages and markers
    , userMessages :: Label
    , userMarkers  :: Label

    -- Add this number to timestamps, we need to see the WallClockSync
    -- event in order to set this value.
    , timeOffset :: IORef (Maybe Word64)
    }


data Gc = Gc { requestor     :: Cap
             , parallel      :: Bool
             , generation    :: Int
             , requestTime   :: Timestamp
             , startTimes    :: M.Map Cap Timestamp -- ^ when each capability starts GC
             , gcWorkIntervals :: M.Map Cap (S.Set Interval) -- ^ when each capability was working
             , bytesCopied   :: Word64
             , gcEndTimes    :: M.Map Cap Timestamp
             }
        deriving (Show)

data GCState = GCState (Maybe Gc) (M.Map Cap Timestamp)

initGCState = GCState Nothing mempty

toGcs :: GCState -> Event -> (Maybe Gc, GCState)
toGcs (GCState mc starts) e = go mc starts e
  where
    go :: Maybe Gc -> M.Map Cap Timestamp -> Event -> (Maybe Gc, GCState)
    go gc _workStart  (Event t ev (Just cap))
      --- | not $ M.null workStart = error $ "non-empty workStart: "++show workStart
      | RequestParGC <- ev =
          (gc,  GCState (Just $ gc0 { parallel = True}) mempty)
      | RequestSeqGC <- ev =
          (gc,  GCState (Just $ gc0 { parallel = False}) mempty)
      where gc0 = Gc { requestor     = Cap cap
                     , parallel      = undefined
                     , generation    = undefined
                     , requestTime   = t
                     , startTimes    = mempty
                     , gcWorkIntervals = mempty
                     , bytesCopied   = undefined
                     , gcEndTimes    = mempty
                     }

    go (Just gc) workStart (Event t ev (Just cap'))
      | GCIdle <- ev
      , Just start <- M.lookup cap workStart =
          let workStart' = M.delete cap workStart
              int = Interval start t
          in (Nothing, GCState (Just $ gc { gcWorkIntervals = M.insertWith (<>) cap (S.singleton int) (gcWorkIntervals gc) }) workStart')
      | GCWork <- ev
      , cap `M.member` workStart = error "worker never started"
      | GCWork <- ev =
          (Nothing, GCState (Just gc) (M.insert cap t workStart))
      where cap = Cap cap'

    go (Just gc) workStart (Event t ev (Just cap'))
      | StartGC <- ev =
          (Nothing, GCState (Just $ gc { startTimes = M.insert cap t (startTimes gc) }) workStart)
      | GCStatsGHC {..} <- ev =
          (Nothing, GCState (Just $ gc { generation = gen, bytesCopied = parTotCopied }) workStart)
      | EndGC <- ev
      , Just start <- M.lookup cap workStart =
          let workStart' = M.delete cap workStart
              int = Interval start t
          in (Nothing, GCState (Just $ gc { gcEndTimes = M.insert cap t (gcEndTimes gc)
                           , gcWorkIntervals = M.insertWith (<>) cap (S.singleton int) (gcWorkIntervals gc)
                           }) workStart')
      | EndGC <- ev =
          let workStart' = M.delete cap workStart
          in (Nothing, GCState (Just $ gc { gcEndTimes = M.insert cap t (gcEndTimes gc) }) workStart')
      where cap = Cap cap'
    --go gc (Event t ev Nothing : rest)
    go gc workStart _ = (Nothing, GCState gc workStart)

maxStartLatency :: Gc -> Timestamp
maxStartLatency gc =
    maximum [ t - requestTime gc
            | t <- M.elems $ startTimes gc
            ]

gcEndTime :: Gc -> Timestamp
gcEndTime = maximum . gcEndTimes

endIdleTimes :: Gc -> [Timestamp]
endIdleTimes gc =
    [ endTime - maximum (map intEnd $ S.toList workIntervals)
    | workIntervals <- M.elems $ gcWorkIntervals gc
    ]
  where endTime = gcEndTime gc


newtype Cap = Cap Int
            deriving (Show, Ord, Eq)

data Interval = Interval { intStart, intEnd :: Timestamp }
              deriving (Show, Ord, Eq)

intDuration :: Interval -> Timestamp
intDuration i = intEnd i - intStart i

toSeconds :: Timestamp -> Double
toSeconds = (/1e9) . realToFrac

