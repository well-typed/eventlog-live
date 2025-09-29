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

import Control.Monad.IO.Class (MonadIO (..))
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
logMessage :: (MonadIO m) => Verbosity -> Verbosity -> LogSource -> Text -> m ()
logMessage verbosityLevel verbosityThreshold logSource msg
  | verbosityLevel >= verbosityThreshold =
      liftIO . TIO.hPutStrLn IO.stderr . mconcat $ [logSource, ": ", showVerbosity verbosityLevel, ": ", msg]
  | otherwise = pure ()

{- |
Internal helper. Log errors to `IO.stderr`.
-}
logError :: (MonadIO m) => Verbosity -> LogSource -> Text -> m ()
logError = logMessage verbosityError

{- |
Internal helper. Log warnings to `IO.stderr`.
-}
logWarning :: (MonadIO m) => Verbosity -> LogSource -> Text -> m ()
logWarning = logMessage verbosityWarning

{- |
Internal helper. Log info messages to `IO.stderr`.
-}
logInfo :: (MonadIO m) => Verbosity -> LogSource -> Text -> m ()
logInfo = logMessage verbosityInfo

{- |
Internal helper. Log debug messages to `IO.stderr`.
-}
logDebug :: (MonadIO m) => Verbosity -> LogSource -> Text -> m ()
logDebug = logMessage verbosityDebug
