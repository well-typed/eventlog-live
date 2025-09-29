{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Internal.Logger
Description : /Internal module/. Logging functions.
Stability   : experimental
Portability : portable

This module is __internal__. The [PVP](https://pvp.haskell.org) __does not apply__.
-}
module GHC.Eventlog.Live.Internal.Logger (
  LogSource,
  logMessage,
  logError,
  logWarning,
  logInfo,
  logDebug,
) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.Eventlog.Live.Verbosity (Verbosity, showVerbosity, verbosityDebug, verbosityError, verbosityInfo, verbosityWarning)
import System.IO qualified as IO

{- |
Internal helper. Denotes the source of a log message.
-}
type LogSource = Text

{- |
Internal helper. Log messages to `IO.stderr`.
Only prints a message if its verbosity level is above the verbosity threshold.
-}
logMessage :: Verbosity -> Verbosity -> LogSource -> Text -> IO ()
logMessage verbosityLevel verbosityThreshold logSource msg
  | verbosityLevel >= verbosityThreshold =
      TIO.hPutStrLn IO.stderr . mconcat $ [logSource, ": ", showVerbosity verbosityLevel, ": ", msg]
  | otherwise = pure ()

{- |
Internal helper. Log errors to `IO.stderr`.
-}
logError :: Verbosity -> LogSource -> Text -> IO ()
logError = logMessage verbosityError

{- |
Internal helper. Log warnings to `IO.stderr`.
-}
logWarning :: Verbosity -> LogSource -> Text -> IO ()
logWarning = logMessage verbosityWarning

{- |
Internal helper. Log info messages to `IO.stderr`.
-}
logInfo :: Verbosity -> LogSource -> Text -> IO ()
logInfo = logMessage verbosityInfo

{- |
Internal helper. Log debug messages to `IO.stderr`.
-}
logDebug :: Verbosity -> LogSource -> Text -> IO ()
logDebug = logMessage verbosityDebug
