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
  MyTelemetryData (..),
  writeLog,
  writeMetric,
  filterBySeverity,
  stderrLogger,
  handleLogger,
  chanLogger,
  chanSource,
) where

import Colog.Core.Action (cfilter, (<&))
import Colog.Core.Action qualified as CCA (LogAction (..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Exception (bracket_)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Ix (Ix (..))
import Data.Machine (SourceT, repeatedly, yield)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Eventlog.Live.Data.Attribute (AttrValue (..), (~=))
import GHC.Eventlog.Live.Data.Attribute qualified as A
import GHC.Eventlog.Live.Data.LogRecord (LogRecord (..))
import GHC.Eventlog.Live.Data.Metric (KnownMetricType, Metric (..), SomeMetric (..))
import GHC.Eventlog.Live.Data.Severity (Severity (..), toSeverityString)
import GHC.RTS.Events (Timestamp)
import GHC.Stack (callStack, prettyCallStack, withFrozenCallStack)
import GHC.Stack.Types (HasCallStack)
import System.Clock (Clock (..), TimeSpec (..), getTime)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, hSetSGR)
import System.IO qualified as IO
import Prelude hiding (log)

type Logger m = CCA.LogAction m MyTelemetryData

{- |
The type of internal telemetry data.
-}
data MyTelemetryData
  = MyTelemetryData'LogRecord {logRecord :: !LogRecord}
  | MyTelemetryData'Metric {metric :: !SomeMetric}

{- |
Use a `Logger` to log a message with a severity.
-}
writeLog :: (HasCallStack) => Logger m -> Severity -> Text -> m ()
writeLog logger severity body =
  withFrozenCallStack $
    logger
      <& MyTelemetryData'LogRecord
        { logRecord =
            LogRecord
              { body
              , maybeSeverity = Just severity
              , maybeTimeUnixNano = Nothing
              , attrs = ["call-stack" ~= prettyCallStack callStack]
              }
        }

{- |
Use a `Logger` to log an internal metric.
-}
writeMetric ::
  forall m metricType.
  (KnownMetricType metricType) =>
  Logger m ->
  -- | The metric name.
  String ->
  metricType ->
  m ()
writeMetric logger metricName value =
  logger
    <& MyTelemetryData'Metric
      SomeMetric
        { metricName
        , metric =
            Metric
              { value
              , maybeTimeUnixNano = Nothing
              , maybeStartTimeUnixNano = Nothing
              , attrs = []
              }
        }

{- |
A `Logger` that writes each `LogRecord` to a `IO.stderr` and ignores all other telemetry data.

__TODO:__ Support the remaining telemetry data.
-}
stderrLogger :: Logger IO
stderrLogger = handleLogger IO.stderr

{- |
A `Logger` that writes each `LogRecord` to a `IO.Handle` and ignores all other telemetry data.

__TODO:__ Support the remaining telemetry data.
-}
handleLogger ::
  IO.Handle ->
  Logger IO
handleLogger handle = CCA.LogAction $ \case
  MyTelemetryData'LogRecord logRecord -> liftIO $ do
    withSeverityColor logRecord.maybeSeverity handle $ \handleWithColor ->
      TIO.hPutStrLn handleWithColor $ formatLogRecord logRecord
    IO.hFlush handle
  MyTelemetryData'Metric{} -> pure ()

{- |
Filter a @`Logger` m@ by a `Severity`.
-}
filterBySeverity ::
  (Applicative m) =>
  Severity ->
  Logger m ->
  Logger m
filterBySeverity severityThreshold =
  cfilter severityFilter
 where
  severityFilter = \case
    MyTelemetryData'LogRecord{..} ->
      maybe False (>= severityThreshold) logRecord.maybeSeverity
    _otherwise -> True

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

{- |
A `Logger` that writes the internal telemetry data to a channel.
-}
chanLogger :: TChan MyTelemetryData -> Logger IO
chanLogger chan =
  CCA.LogAction $
    atomically . writeTChan chan <=< addTimeUnixNano

{- |
A `Souce` that reads the data from a channel.
-}
chanSource :: (MonadIO m) => TChan a -> SourceT m a
chanSource chan = repeatedly $ do
  a <- liftIO $ atomically $ readTChan chan
  yield a

{- |
Add the current Unix timestamp in nanoseconds to telemetry data.
-}
addTimeUnixNano :: MyTelemetryData -> IO MyTelemetryData
addTimeUnixNano myTelemetryData =
  case myTelemetryData of
    MyTelemetryData'LogRecord{logRecord = LogRecord{..}}
      | isNothing maybeTimeUnixNano -> do
          timeUnixNano <- getTimeUnixNano
          pure $
            MyTelemetryData'LogRecord
              LogRecord{maybeTimeUnixNano = Just timeUnixNano, ..}
      | otherwise -> pure myTelemetryData
    MyTelemetryData'Metric{metric = SomeMetric{metricName, metric = Metric{..}}}
      | isNothing maybeTimeUnixNano -> do
          timeUnixNano <- getTimeUnixNano
          pure $
            MyTelemetryData'Metric
              SomeMetric
                { metricName
                , metric = Metric{maybeTimeUnixNano = Just timeUnixNano, ..}
                }
      | otherwise -> pure myTelemetryData

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
