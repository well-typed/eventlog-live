{-# LANGUAGE CPP #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Word (Word8)
import GHC.Eventlog.Socket (startWait)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Text.Printf (printf)

#ifdef JUMPY_JUMP_USE_GHC_STACK_PROFILER
import GHC.Stack.Profiler (ProfilerSamplingInterval (..), withRootStackProfiler, withStackProfiler)
#endif

withGhcStackProfiler :: IO () -> IO ()
#ifdef JUMPY_JUMP_USE_GHC_STACK_PROFILER
withGhcStackProfiler action =
  withRootStackProfiler True $ \manager ->
    withStackProfiler manager (SampleIntervalMs 30) $
      action
#else
withGhcStackProfiler action = action
#endif

main :: IO ()
main =
  withGhcStackProfiler $ do
    traverse_ startWait =<< lookupEnv "GHC_EVENTLOG_SOCKET"
    forever $ do
      jumpyJump0
      threadDelay 100

{-# SCC jumpyJump0 #-}
{-# OPAQUE jumpyJump0 #-}
jumpyJump0 = do
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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
  n <- randomRIO @Word8 (0, 9)
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

{-# SCC jumpyJump9 #-}
jumpyJump9 = do
  n <- randomRIO @Word8 (0, 9)
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
