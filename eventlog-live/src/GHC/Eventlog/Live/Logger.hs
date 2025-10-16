{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live..Logger
Description : Logging functions.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Logger (
  logError,
  logWarning,
  logInfo,
  logDebug,
) where

import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Eventlog.Live.Verbosity (Verbosity, showVerbosity, verbosityDebug, verbosityError, verbosityInfo, verbosityWarning)
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, hSetSGR)
import System.IO qualified as IO

{- |
Log messages to given handle.
Only prints a message if its verbosity level is above the verbosity threshold.
-}
logMessage :: (MonadIO m) => IO.Handle -> CallStack -> Verbosity -> Verbosity -> Text -> m ()
logMessage handle theCallStack verbosityLevel verbosityThreshold msg
  | verbosityLevel >= verbosityThreshold = liftIO $ do
      withVerbosityColor verbosityLevel handle
        . flip TIO.hPutStrLn
        . formatMessage verbosityLevel verbosityThreshold theCallStack
        $ msg
      IO.hFlush handle
  | otherwise = pure ()

{- |
Internal helper.
Format the `CallStack`.
-}
formatCallStack :: CallStack -> Text
formatCallStack theCallStack =
  maybe T.empty (formatSrcLoc . snd . fst) (L.uncons (getCallStack theCallStack))
 where
  formatSrcLoc :: SrcLoc -> Text
  formatSrcLoc srcLoc =
    mconcat [T.pack srcLoc.srcLocFile, ":", T.pack (show srcLoc.srcLocStartLine), ":", T.pack (show srcLoc.srcLocStartCol)]

{- |
Internal helper.
Format the message appropriately for the given verbosity level and threshold.
-}
formatMessage :: Verbosity -> Verbosity -> CallStack -> Text -> Text
formatMessage verbosityLevel verbosityThreshold theCallStack msg
  | verbosityLevel == verbosityInfo && verbosityThreshold /= verbosityDebug = msg
  | otherwise = mconcat [showVerbosity verbosityLevel, " (", formatCallStack theCallStack, "): ", msg]

{- |
Internal helper.
Use a handle with the color set appropriately for the given verbosity level.
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
Internal helper.
Determine the ANSI color associated with a particular verbosity level.
-}
verbosityColor :: Verbosity -> Maybe Color
verbosityColor verbosity
  | verbosity == verbosityError = Just Red
  | verbosity == verbosityWarning = Just Yellow
  | verbosity == verbosityDebug = Just Blue
  | otherwise = Nothing

{- |
Log errors to `IO.stderr`.
-}
logError :: (HasCallStack, MonadIO m) => Verbosity -> Text -> m ()
logError = logMessage IO.stderr callStack verbosityError

{- |
Log warnings to `IO.stderr`.
-}
logWarning :: (HasCallStack, MonadIO m) => Verbosity -> Text -> m ()
logWarning = logMessage IO.stderr callStack verbosityWarning

{- |
Log info messages to `IO.stderr`.
-}
logInfo :: (HasCallStack, MonadIO m) => Verbosity -> Text -> m ()
logInfo = logMessage IO.stdout callStack verbosityInfo

{- |
Log debug messages to `IO.stderr`.
-}
logDebug :: (HasCallStack, MonadIO m) => Verbosity -> Text -> m ()
logDebug = logMessage IO.stderr callStack verbosityDebug
