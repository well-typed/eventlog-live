module GHC.Eventlog.Counters where

import Control.Concurrent.STM (
    STM,
    TVar,
    modifyTVar',
    newTVar,
    readTVar,
    writeTVar,
 )
import Data.Foldable (for_)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Word (Word32, Word64)

import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map

import GHC.RTS.Events

-- | Various counters we update with 'count'.
data Counters = Counters
    { cntEvents :: TVar Word64
    , cntTime :: TVar Timestamp
    , cntWallClock :: TVar (Maybe WallClockInfo)
    , cntCaps :: TVar IntSet
    , cntHeapSize :: TVar Word64
    , cntHeapLive :: TVar Word64
    , cntBlocksSize :: TVar Word64
    , cntHeapAlloc :: TVar Word64
    , cntBlocksCurrent :: TVar Word32
    , cntBlocksNeeded :: TVar Word32
    , cntBlocksReturned :: TVar Word32
    , cntGCStart :: TVar (Maybe Timestamp)
    , cntMaxGCTime :: TVar Timestamp
    , cntGcCount0 :: TVar Word64
    , cntGcCountS :: TVar Word64
    , cntThreads :: TVar (Map ThreadId ThreadState)
    }

{- | Wall-clock information.

RTS issues an event so we can connect event timestamps with the wall clock.
-}
data WallClockInfo = WallClockInfo
    { wciTime :: !Timestamp
    -- ^ event timestamp
    , wciSec :: !Word64
    -- ^ wall clock seconds
    , wciNSec :: !Word32
    -- ^ wall clock nanoseconds
    }

-- | Thread states, used in 'cntThreads'.
data ThreadState
    = -- | we know thread exist, but not its state
      ThreadUnknown
    | -- | currently running
      ThreadRunning
    | -- | ready to be run
      ThreadQueued
    | -- | stopped, e.g. blocked
      ThreadStopped
    deriving (Eq, Show)

instance Semigroup ThreadState where
    ThreadUnknown <> y = y
    x <> ThreadUnknown = x
    x <> _ = x

newCounters :: STM Counters
newCounters = do
    cntEvents <- newTVar 0
    cntTime <- newTVar 0
    cntWallClock <- newTVar Nothing
    cntCaps <- newTVar IS.empty

    cntHeapSize <- newTVar 0
    cntHeapLive <- newTVar 0
    cntHeapAlloc <- newTVar 0
    cntBlocksSize <- newTVar 0

    cntBlocksCurrent <- newTVar 0
    cntBlocksNeeded <- newTVar 0
    cntBlocksReturned <- newTVar 0

    cntGCStart <- newTVar Nothing
    cntMaxGCTime <- newTVar 0
    cntGcCount0 <- newTVar 0
    cntGcCountS <- newTVar 0

    cntThreads <- newTVar Map.empty

    return Counters{..}

count :: Counters -> Event -> STM ()
count cnt (Event t s c) = do
    -- Count events
    modifyTVar' (cntEvents cnt) (+ 1)

    -- Update time
    t' <- readTVar (cntTime cnt)
    writeTVar (cntTime cnt) (max t t')

    -- update seen capabilities
    for_ c $ \c' -> modifyTVar' (cntCaps cnt) (IS.insert c')

    case s of
        -- real time
        WallClockTime{sec, nsec} ->
            writeTVar (cntWallClock cnt) $
                Just
                    WallClockInfo
                        { wciTime = t
                        , wciSec = sec
                        , wciNSec = nsec
                        }
        -- Thread events
        WakeupThread{..} -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadQueued)
        CreateThread{..} -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadQueued)
        RunThread{..} -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadRunning)
        MigrateThread{..} -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadUnknown)
        ThreadLabel{..} -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadUnknown)
        StopThread{..} -> case status of
            ThreadFinished -> modifyTVar' (cntThreads cnt) (Map.delete thread)
            ForeignCall -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadQueued)
            HeapOverflow -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadQueued)
            StackOverflow -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadQueued)
            _ -> modifyTVar' (cntThreads cnt) (Map.insertWith (<>) thread ThreadStopped)
        -- GC events
        StartGC -> writeTVar (cntGCStart cnt) (Just t)
        EndGC -> do
            mt <- readTVar (cntGCStart cnt)
            case mt of
                Nothing -> return () -- shouldn't happen, but may.
                Just start -> modifyTVar' (cntMaxGCTime cnt) (max (t - start))

        -- stats
        GCStatsGHC{..} -> do
            if gen == 0 then modifyTVar' (cntGcCount0 cnt) (+ 1) else modifyTVar' (cntGcCountS cnt) (+ 1)
        HeapAllocated{allocBytes = x} -> writeTVar (cntHeapAlloc cnt) x
        HeapSize{sizeBytes = x} -> writeTVar (cntHeapSize cnt) x
        BlocksSize{blocksSize = x} -> writeTVar (cntBlocksSize cnt) x
        MemReturn{..} -> do
            writeTVar (cntBlocksCurrent cnt) current
            writeTVar (cntBlocksNeeded cnt) needed
            writeTVar (cntBlocksReturned cnt) returned
        HeapLive{liveBytes = x} -> writeTVar (cntHeapLive cnt) x
        _otherwise -> pure ()
