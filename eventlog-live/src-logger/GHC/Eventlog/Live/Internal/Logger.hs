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

import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Eventlog.Live.Verbosity (Verbosity, showVerbosity, verbosityDebug, verbosityError, verbosityInfo, verbosityWarning)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, hSetSGR)
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
      liftIO
        . withVerbosityColor verbosityLevel IO.stderr
        . flip TIO.hPutStrLn
        . formatMessage verbosityLevel verbosityThreshold logSource
        $ msg
  | otherwise = pure ()

{- |
Internal helper. Format the message appropriately for the given verbosity level and threshold.
-}
formatMessage :: Verbosity -> Verbosity -> LogSource -> Text -> Text
formatMessage verbosityLevel verbosityThreshold logSource msg
  | verbosityLevel == verbosityInfo && verbosityThreshold /= verbosityDebug = msg
  | otherwise = mconcat [showVerbosity verbosityLevel, T.pack "[", logSource, T.pack "]: ", msg]

{- |
Internal helper. Use a handle with the color set appropriately for the given verbosity level.
-}
withVerbosityColor :: Verbosity -> IO.Handle -> (IO.Handle -> IO a) -> IO a
withVerbosityColor verbosity handle action = do
  supportsANSI <- hNowSupportsANSI handle
  if not supportsANSI
    then
      action handle
    else case verbosityColor verbosity of
      Nothing ->
        action handle
      Just color -> do
        let setVerbosityColor = hSetSGR handle [SetColor Foreground Vivid color]
        let setDefaultColor = hSetSGR handle [SetDefaultColor Foreground]
        bracket_ setVerbosityColor setDefaultColor $ action handle

{- |
Internal helper. Determine the ANSI color associated with a particular verbosity level.
-}
verbosityColor :: Verbosity -> Maybe Color
verbosityColor verbosity
  | verbosity == verbosityError = Just Red
  | verbosity == verbosityWarning = Just Yellow
  | verbosity == verbosityDebug = Just Blue
  | otherwise = Nothing

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
