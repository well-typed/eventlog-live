{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Word (Word8)
import Debug.Trace (traceMarkerIO)
import GHC.Eventlog.Socket (startFromEnv, startWait)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.Random (newStdGen, randomRIO, randomRs)
import Text.Printf (printf)

#ifdef JUMPY_JUMP_USE_GHC_STACK_PROFILER
import GHC.Stack.Profiler (ProfilerSamplingInterval (..), withRootStackProfiler, withStackProfiler)
#endif

withGhcStackProfiler :: IO () -> IO ()
#ifdef JUMPY_JUMP_USE_GHC_STACK_PROFILER
withGhcStackProfiler action =
  withRootStackProfiler True $ \manager ->
    withStackProfiler manager (SampleIntervalMs 1) $
      action
#else
withGhcStackProfiler action = action
#endif

main :: IO ()
main =
  withGhcStackProfiler $ do
    startFromEnv
    forever $ do
      jumpyJump0
      beABitOfAnOddball

{-# SCC beABitOfAnOddball #-}
{-# OPAQUE beABitOfAnOddball #-}
beABitOfAnOddball = do
  n <- randomRIO (100, 1000)
  traceMarkerIO $ "Summing " ++ show n ++ " numbers"
  putStrLn $ "Generating " ++ show n ++ " random numbers"
  nRandomIntegers <- randomRs @Integer (-1000, 1000) <$> newStdGen
  let sumOfNRandomIntegers = foldr (+) 00 $ take n nRandomIntegers
  putStrLn $ "Sum: " ++ show sumOfNRandomIntegers

randomJumpTarget :: IO Word8
randomJumpTarget = do
  n <- randomRIO @Word8 (0, 9)
  -- Decrease the chance of returning 9,
  -- which makes the chains of jumps longer.
  if n /= 9 then pure n else randomRIO @Word8 (0, 9)

{-# SCC jumpyJump0 #-}
{-# OPAQUE jumpyJump0 #-}
jumpyJump0 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump1 #-}
{-# OPAQUE jumpyJump1 #-}
jumpyJump1 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump2 #-}
{-# OPAQUE jumpyJump2 #-}
jumpyJump2 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump3 #-}
{-# OPAQUE jumpyJump3 #-}
jumpyJump3 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump4 #-}
{-# OPAQUE jumpyJump4 #-}
jumpyJump4 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump5 #-}
{-# OPAQUE jumpyJump5 #-}
jumpyJump5 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump6 #-}
{-# OPAQUE jumpyJump6 #-}
jumpyJump6 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump7 #-}
{-# OPAQUE jumpyJump7 #-}
jumpyJump7 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n

{-# SCC jumpyJump8 #-}
{-# OPAQUE jumpyJump8 #-}
jumpyJump8 = do
  n <- randomJumpTarget
  threadDelay 5
  printf "enter %d\n" n
  case n of
    0 -> jumpyJump0
    1 -> jumpyJump1
    2 -> jumpyJump2
    3 -> jumpyJump3
    4 -> jumpyJump4
    5 -> jumpyJump5
    6 -> jumpyJump6
    7 -> jumpyJump7
    8 -> jumpyJump8
    9 -> pure ()
  printf "leave %d\n" n
