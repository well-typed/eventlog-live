{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Internal.Logger (
  LogSource,
  logMessage,
  logError,
  logWarning,
) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.Eventlog.Live.Verbosity (Verbosity, showVerbosity, verbosityError, verbosityWarning)
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
