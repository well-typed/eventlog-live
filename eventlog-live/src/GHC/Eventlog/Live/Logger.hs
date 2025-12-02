{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live..Logger
Description : Logging functions.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Logger (
  LogAction,
  Message,
  (<&),
  (&>),
  handleLogAction,
  stderrLogAction,
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
import GHC.Eventlog.Live.Data.Attribute ((~=))
import GHC.Eventlog.Live.Data.LogRecord (LogRecord (..))
import GHC.Eventlog.Live.Data.Severity (Severity (..), toSeverityString)
import GHC.RTS.Events (Timestamp)
import GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack, withFrozenCallStack)
import System.Clock (Clock (..), TimeSpec (..), getTime)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, hSetSGR)
import System.IO qualified as IO

type LogAction m = CCA.LogAction m Message

data Message = Message
  { severity :: !Severity
  , callStack :: !CallStack
  , body :: Text
  }

infix 6 &>

{- |
Construct a `Message` from a `Severity` and a body.
-}
(&>) :: (HasCallStack) => Severity -> Text -> Message
(&>) severity body = withFrozenCallStack Message{callStack, ..}

{- |
A `LogAction` that writes each `Message` to a `IO.stderr`.
-}
stderrLogAction :: LogAction IO
stderrLogAction = handleLogAction IO.stderr

{- |
A `LogAction` that writes each `Message` to a `IO.Handle`.
-}
handleLogAction ::
  IO.Handle ->
  LogAction IO
handleLogAction handle = CCA.LogAction $ \msg -> liftIO $ do
  withSeverityColor msg.severity handle $ \handleWithColor ->
    TIO.hPutStrLn handleWithColor $ formatMessage msg
  IO.hFlush handle

{- |
Filter a @`LogAction` m `Message`@ by a `Severity`.
-}
filterBySeverity ::
  (Applicative m) =>
  Severity ->
  LogAction m ->
  LogAction m
filterBySeverity severityThreshold =
  cfilter ((>= severityThreshold) . (.severity))

{- |
Internal helper.
Format the message appropriately for the given verbosity level and threshold.
-}
formatMessage :: Message -> Text
formatMessage msg =
  TL.toStrict . TLB.toLazyText . mconcat $
    [ "["
    , TLB.fromString (toSeverityString msg.severity)
    , "]"
    , " "
    , TLB.fromText msg.body
    , if msg.severity >= ERROR
        then "\n" <> TLB.fromString (prettyCallStack msg.callStack)
        else ""
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
withSeverityColor :: Severity -> IO.Handle -> (IO.Handle -> IO a) -> IO a
withSeverityColor severity handle action = do
  supportsANSI <- hNowSupportsANSI handle
  if not supportsANSI
    then
      action handle
    else case severityColor severity of
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
Create a `LogRecord` from a `Message`.
-}
_logRecord :: Message -> IO LogRecord
_logRecord msg = do
  !timeUnixNano <- getTimeUnixNano
  pure
    LogRecord
      { body = msg.body
      , maybeTimeUnixNano = Just timeUnixNano
      , maybeSeverity = Just msg.severity
      , attrs = ["call-stack" ~= show msg.callStack]
      }

{- |
Get the current Unix time in nanoseconds.

__Warning:__ This will start overflowing in the year 2554.
-}
getTimeUnixNano :: IO Timestamp
getTimeUnixNano = toNanos <$> getTime Realtime
 where
  -- NOTE: This will overflow if @t.sec > (2^64 - 1) `div` 10^9@,
  --       which means you're running this code in the year 2554.
  --       What's that like?
  toNanos :: TimeSpec -> Timestamp
  toNanos t = 1_000_000_000 * fromIntegral t.sec + fromIntegral t.nsec
