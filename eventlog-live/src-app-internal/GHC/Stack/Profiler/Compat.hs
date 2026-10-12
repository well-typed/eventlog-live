{-# LANGUAGE CPP #-}

{- |
Module      : GHC.Stack.Profiler.Compat
Description : The implementation of @eventlog-live-otlp@.
Stability   : experimental
Portability : portable
-}
module GHC.Stack.Profiler.Compat (
  withGhcStackProfiler,
) where

#ifdef EVENTLOG_LIVE_USE_GHC_STACK_PROFILER
import GHC.Stack.Profiler (withProfilerFromEnv)
#endif

--------------------------------------------------------------------------------
-- My Eventlog Socket
--------------------------------------------------------------------------------

{- |
Set @ghc-stack-profiler@ as the eventlog writer.
-}
withGhcStackProfiler :: IO () -> IO ()
#ifdef EVENTLOG_LIVE_USE_GHC_STACK_PROFILER
withGhcStackProfiler action = withProfilerFromEnv $ const action
#else
withGhcStackProfiler action = action
#endif
