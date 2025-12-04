{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live..Logger
Description : Logging functions.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Logger (
  Logger,
  writeLog,
  stderrLogger,
  handleLogger,
  filterBySeverity,
) where

import Colog.Core.Action (cfilter, (<&))
import Colog.Core.Action qualified as CCA (LogAction (..))
import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Ix (Ix (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Eventlog.Live.Data.Attribute (AttrValue (..), (~=))
import GHC.Eventlog.Live.Data.Attribute qualified as A
import GHC.Eventlog.Live.Data.LogRecord (LogRecord (..))
import GHC.Eventlog.Live.Data.Severity (Severity (..), toSeverityString)
import GHC.RTS.Events (Timestamp)
import GHC.Stack (callStack, prettyCallStack, withFrozenCallStack)
import GHC.Stack.Types (HasCallStack)
import System.Clock (Clock (..), TimeSpec (..), getTime)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, hSetSGR)
import System.IO qualified as IO

type Logger m = CCA.LogAction m LogRecord

{- |
Use a `Logger` to log a message with a severity.
-}
writeLog :: (HasCallStack) => Logger m -> Severity -> Text -> m ()
writeLog logger severity body =
  withFrozenCallStack $
    logger
      <& LogRecord
        { body
        , maybeSeverity = Just severity
        , maybeTimeUnixNano = Nothing
        , attrs = ["call-stack" ~= prettyCallStack callStack]
        }

{- |
A `LogAction` that writes each `Message` to a `IO.stderr`.
-}
stderrLogger :: Logger IO
stderrLogger = handleLogger IO.stderr

{- |
A `LogAction` that writes each `Message` to a `IO.Handle`.
-}
handleLogger ::
  IO.Handle ->
  Logger IO
handleLogger handle = CCA.LogAction $ \logRecord -> liftIO $ do
  withSeverityColor logRecord.maybeSeverity handle $ \handleWithColor ->
    TIO.hPutStrLn handleWithColor $ formatLogRecord logRecord
  IO.hFlush handle

{- |
Filter a @`Logger` m@ by a `Severity`.
-}
filterBySeverity ::
  (Applicative m) =>
  Severity ->
  Logger m ->
  Logger m
filterBySeverity severityThreshold =
  cfilter (maybe False (>= severityThreshold) . (.maybeSeverity))

{- |
Internal helper.
Format the message appropriately for the given verbosity level and threshold.
-}
formatLogRecord :: LogRecord -> Text
formatLogRecord logRecord =
  TL.toStrict . TLB.toLazyText . mconcat $
    [ -- format the severity
      maybe "" (\severity -> "[" <> TLB.fromString (toSeverityString severity) <> "]") logRecord.maybeSeverity
    , -- format the body
      TLB.fromText logRecord.body
    , -- format the call-stack, if any
      case A.lookup "call-stack" logRecord.attrs of
        Just (AttrText theCallStack)
          | maybe False (>= ERROR) logRecord.maybeSeverity ->
              "\n" <> TLB.fromText theCallStack
        _otherwise -> ""
    ]

{- |
Internal helper.
Determine the ANSI color and intensity associated with a particular `Severity`.
-}
severityColor :: Severity -> Maybe (Color, ColorIntensity)
severityColor severity
  | inRange (TRACE, TRACE4) severity = Just (Blue, Dull)
  | inRange (DEBUG, DEBUG4) severity = Just (Blue, Vivid)
  | inRange (WARN, WARN4) severity = Just (Yellow, Vivid)
  | inRange (ERROR, ERROR4) severity = Just (Red, Dull)
  | inRange (FATAL, FATAL4) severity = Just (Red, Vivid)
  | otherwise = Nothing

{- |
Internal helper.
Use a handle with the color set appropriately for the given `Severity`.
-}
withSeverityColor :: Maybe Severity -> IO.Handle -> (IO.Handle -> IO a) -> IO a
withSeverityColor maybeSeverity handle action = do
  supportsANSI <- hNowSupportsANSI handle
  if not supportsANSI
    then
      action handle
    else case severityColor =<< maybeSeverity of
      Nothing ->
        action handle
      Just (color, intensity) -> do
        let setVerbosityColor = hSetSGR handle [SetColor Foreground intensity color]
        let setDefaultColor = hSetSGR handle [SetDefaultColor Foreground]
        bracket_ setVerbosityColor setDefaultColor $ action handle

--------------------------------------------------------------------------------
-- Internal helpers.
--------------------------------------------------------------------------------

{- |
Get the current Unix time in nanoseconds.

__Warning:__ This will start overflowing in the year 2554.
-}
_getTimeUnixNano :: IO Timestamp
_getTimeUnixNano = toNanos <$> getTime Realtime
 where
  -- NOTE: This will overflow if @t.sec > (2^64 - 1) `div` 10^9@,
  --       which means you're running this code in the year 2554.
  --       What's that like?
  toNanos :: TimeSpec -> Timestamp
  toNanos t = 1_000_000_000 * fromIntegral t.sec + fromIntegral t.nsec
