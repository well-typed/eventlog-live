{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Counters.EKG (
    registerCounters,
) where

import Control.Concurrent.STM (STM, atomically, readTVar)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Eventlog.Counters

import qualified System.Metrics as EKG

{- | Register some metrics from 'Counters'.

=== Registered counters

[@eventlog.time@] TBW
[@eventlog.heapSize@] TBW
[@eventlog.heapLive@] TBW
[@eventlog.heapAlloc@] TBW

=== Registered gauges

[@eventlog.totalThreads@] TBW
-}
registerCounters :: Counters -> EKG.Store -> IO ()
registerCounters Counters{..} ekg = do
    registerGauge "eventlog.time" ekg $ do
        fromIntegral <$> readTVar cntTime

    registerGauge "eventlog.heapsize" ekg $ do
        fromIntegral <$> readTVar cntHeapSize

    registerGauge "eventlog.heaplive" ekg $ do
        fromIntegral <$> readTVar cntHeapLive

    registerCounter "eventlog.heapalloc" ekg $ do
        fromIntegral <$> readTVar cntHeapAlloc

    registerGauge "eventlog.totalThreads" ekg $ do
        fromIntegral . length <$> readTVar cntThreads

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

registerCounter :: Text -> EKG.Store -> STM Int64 -> IO ()
registerCounter name ekg action =
    EKG.registerCounter name (atomically action) ekg

registerGauge :: Text -> EKG.Store -> STM Int64 -> IO ()
registerGauge name ekg action =
    EKG.registerGauge name (atomically action) ekg
