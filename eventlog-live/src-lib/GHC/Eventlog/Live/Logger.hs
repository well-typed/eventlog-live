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
  InternalTelemetry (..),
  InternalMetric (..),
  ExportResult (..),
  logMetric,
  logTick,
  logTrace,
  logDebug,
  logInfo,
  logWarn,
  logError,
  logFatal,
  logException,
  filterBySeverity,
  stderrLogger,
  handleLogger,
  queueLogger,
  queueSource,
) where

import Colog.Core.Action (cfilter, (<&))
import Colog.Core.Action qualified as CCA (LogAction (..))
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, readTQueue, writeTQueue)
import Control.Exception (Exception (..), bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Ix (Ix (..))
import Data.Machine (SourceT, repeatedly, yield)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.Eventlog.Live.Machine.Core (Tick (..))
import GHC.Eventlog.Live.Types.Attribute (AttrValue (..), (~=))
import GHC.Eventlog.Live.Types.Attribute qualified as A
import GHC.Eventlog.Live.Types.Logs (LogRecord (..))
import GHC.Eventlog.Live.Types.Logs qualified as LogRecord
import GHC.Eventlog.Live.Types.Metrics (Metric (..))
import GHC.Eventlog.Live.Types.Metrics qualified as Metric
import GHC.Eventlog.Live.Types.Severity (Severity (..), toSeverityString)
import GHC.IsList (IsList (..))
import GHC.RTS.Events (Timestamp)
import GHC.Stack (CallStack, callStack, prettyCallStack)
import GHC.Stack.Types (HasCallStack)
import System.Clock (Clock (..), TimeSpec (..), getTime)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, hSetSGR)
import System.Exit (exitFailure)
import System.IO qualified as IO

newtype Logger m = Logger {unLogger :: CCA.LogAction m (Tick InternalTelemetry)}
  deriving newtype (Semigroup, Monoid)

{- |
The type of internal telemetry data.
-}
data InternalTelemetry
  = InternalTelemetry'LogRecord !LogRecord
  | forall a. InternalTelemetry'Metric !(InternalMetric a) !(Metric a)

{- |
Tags for the known internal metrics.
-}
data InternalMetric a where
  EventCount :: InternalMetric Word
  ExportLogs :: InternalMetric ExportResult
  ExportMetrics :: InternalMetric ExportResult
  ExportSpans :: InternalMetric ExportResult
  ExportSamples :: InternalMetric ExportResult

{- |
The result of an export.
-}
data ExportResult = ExportResult
  { exported :: !Int64
  , rejected :: !Int64
  }
  deriving (Show)

instance Semigroup ExportResult where
  r1 <> r2 = ExportResult{exported = r1.exported + r2.exported, rejected = r1.rejected + r2.rejected}

instance Monoid ExportResult where
  mempty = ExportResult{exported = 0, rejected = 0}

{- |
Use a `Logger` to log a message with a severity.
-}
writeLog :: (HasCallStack) => Logger m -> Severity -> Text -> m ()
writeLog logger severity message =
  writeLogRecord
    LogRecord
      { value = message
      , maybeSeverity = Just severity
      , maybeTimeUnixNano = Nothing
      , attrs = maybe mempty (\cs -> ["call-stack" ~= prettyCallStack cs]) (cleanCallStack callStack)
      }
 where
  writeLogRecord = (logger.unLogger <&) . Item . InternalTelemetry'LogRecord

{- |
Use a `Logger` to log an internal metric.
-}
logMetric :: Logger m -> InternalMetric a -> a -> m ()
logMetric logger internalMetric value =
  writeMetric
    Metric
      { value = value
      , maybeTimeUnixNano = Nothing
      , maybeStartTimeUnixNano = Nothing
      , attrs = mempty
      }
 where
  writeMetric = (logger.unLogger <&) . Item . InternalTelemetry'Metric internalMetric

{- |
Internal helper.

Remove all log functions from the `CallStack`.
-}
cleanCallStack :: CallStack -> Maybe CallStack
cleanCallStack =
  toMaybeCallStack . dropWhile ((`elem` logFunctions) . fst) . toList
 where
  toMaybeCallStack locations = if null locations then Nothing else Just (fromList locations)
  logFunctions :: [String]
  logFunctions = ["writeLog", "logTrace", "logDebug", "logInfo", "logWarn", "logError", "logFatal", "logException"]

{- |
Use a `Logger` to log a `Tick`.
-}
logTick :: (Applicative m) => Logger m -> Tick x -> m ()
logTick logger = \case Tick -> logger.unLogger <& Tick; Item{} -> pure ()

{- |
Use a `Logger` to log a message with `TRACE` severity.
-}
logTrace :: (HasCallStack) => Logger m -> Text -> m ()
logTrace = flip writeLog TRACE

{- |
Use a `Logger` to log a message with `DEBUG` severity.
-}
logDebug :: (HasCallStack) => Logger m -> Text -> m ()
logDebug = flip writeLog DEBUG

{- |
Use a `Logger` to log a message with `INFO` severity.
-}
logInfo :: (HasCallStack) => Logger m -> Text -> m ()
logInfo = flip writeLog INFO

{- |
Use a `Logger` to log a message with `WARN` severity.
-}
logWarn :: (HasCallStack) => Logger m -> Text -> m ()
logWarn = flip writeLog WARN

{- |
Use a `Logger` to log a message with `ERROR` severity.
-}
logError :: (HasCallStack) => Logger m -> Text -> m ()
logError = flip writeLog ERROR

{- |
Use a `Logger` to log a message with `FATAL` severity and exit.
-}
logFatal :: (HasCallStack) => Logger IO -> Text -> IO x
logFatal logger message = writeLog logger FATAL message >> exitFailure

{- |
Use a `Logger` to log an exception.
-}
logException :: (Exception e) => Logger m -> e -> m ()
logException logger e =
  logError logger (T.pack $ displayException e)

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
handleLogger handle = Logger . CCA.LogAction $ \case
  Tick ->
    pure ()
  Item (toLogRecord -> logRecord) -> liftIO $ do
    withSeverityColor logRecord.maybeSeverity handle $ \handleWithColor ->
      TIO.hPutStrLn handleWithColor $ formatLogRecord logRecord
    IO.hFlush handle

{- |
Convert `InternalTelemetry` to a `LogRecord` for the `handleLogger`.
-}
toLogRecord :: InternalTelemetry -> LogRecord
toLogRecord = \case
  InternalTelemetry'LogRecord logRecord -> logRecord
  InternalTelemetry'Metric internalMetric metric -> toLogRecord'Metric metric internalMetric
 where
  toLogRecord'Metric :: Metric a -> InternalMetric a -> LogRecord
  toLogRecord'Metric metric = \case
    EventCount ->
      logRecord DEBUG $ "Received " <> T.show metric.value <> " events."
    ExportLogs
      | ExportResult{..} <- metric.value ->
          if rejected > 0
            then logRecord ERROR $ "Exported " <> T.show exported <> " logs (" <> T.show rejected <> " rejected)."
            else logRecord DEBUG $ "Exported " <> T.show exported <> " logs."
    ExportMetrics
      | ExportResult{..} <- metric.value ->
          if rejected > 0
            then logRecord ERROR $ "Exported " <> T.show exported <> " metrics (" <> T.show rejected <> " rejected)."
            else logRecord DEBUG $ "Exported " <> T.show exported <> " metrics."
    ExportSpans
      | ExportResult{..} <- metric.value ->
          if rejected > 0
            then logRecord ERROR $ "Exported " <> T.show exported <> " spans (" <> T.show rejected <> " rejected)."
            else logRecord DEBUG $ "Exported " <> T.show exported <> " spans."
    ExportSamples
      | ExportResult{..} <- metric.value ->
          if rejected > 0
            then logRecord ERROR $ "Exported " <> T.show exported <> " samples (" <> T.show rejected <> " rejected)."
            else logRecord DEBUG $ "Exported " <> T.show exported <> " samples."
   where
    logRecord :: Severity -> Text -> LogRecord
    logRecord severity value =
      LogRecord{maybeTimeUnixNano = metric.maybeTimeUnixNano, maybeSeverity = Just severity, attrs = metric.attrs, ..}

{- |
Filter a @`Logger` m@ by a `Severity`.
-}
filterBySeverity ::
  (Applicative m) =>
  Severity ->
  Logger m ->
  Logger m
filterBySeverity severityThreshold =
  Logger . cfilter severityFilter . (.unLogger)
 where
  severityFilter = \case
    Item (InternalTelemetry'LogRecord logRecord) ->
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
      maybe "" (\severity -> "[" <> TLB.fromString (toSeverityString severity) <> "] ") logRecord.maybeSeverity
    , -- format the body
      TLB.fromText logRecord.value
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
A `Logger` that writes the internal telemetry data to a queue.
-}
queueLogger :: TQueue (Tick InternalTelemetry) -> Logger IO
queueLogger queue =
  Logger . CCA.LogAction $ \x -> do
    traverse addTimeUnixNano x >>= \x' ->
      atomically (writeTQueue queue x')

{- |
A `Souce` that reads the data from a queue.
-}
queueSource :: (MonadIO m) => TQueue a -> SourceT m a
queueSource queue = repeatedly $ do
  a <- liftIO (atomically $ readTQueue queue)
  yield a

{- |
Add the current Unix timestamp in nanoseconds to telemetry data.
-}
addTimeUnixNano :: InternalTelemetry -> IO InternalTelemetry
addTimeUnixNano i
  | has maybeTimeUnixNano'InternalTelemetry i = pure i
  | otherwise = set maybeTimeUnixNano'InternalTelemetry i . Just <$> getTimeUnixNano

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

--------------------------------------------------------------------------------
-- Lenses for InternalTelemetry
--
-- NOTE: If you are tempted to export these, just replace them with lens-family2
--------------------------------------------------------------------------------

type Lens s a = forall f. (Functor f) => (a -> f a) -> s -> f s

get :: Lens s a -> s -> a
get l s = getConst $ l Const s
{-# INLINE get #-}

has :: Lens s (Maybe a) -> s -> Bool
has l s = isJust (get l s)
{-# INLINE has #-}

set :: Lens s a -> s -> a -> s
set l s a = runIdentity $ l (const $ Identity a) s
{-# INLINE set #-}

maybeTimeUnixNano'InternalTelemetry :: Lens InternalTelemetry (Maybe Timestamp)
maybeTimeUnixNano'InternalTelemetry f = \case
  InternalTelemetry'LogRecord l -> InternalTelemetry'LogRecord <$> maybeTimeUnixNano'LogRecord f l
  InternalTelemetry'Metric t m -> InternalTelemetry'Metric t <$> maybeTimeUnixNano'Metric f m
{-# INLINE maybeTimeUnixNano'InternalTelemetry #-}

maybeTimeUnixNano'LogRecord :: Lens LogRecord (Maybe Timestamp)
maybeTimeUnixNano'LogRecord f l =
  fmap (\x -> l{LogRecord.maybeTimeUnixNano = x}) (f l.maybeTimeUnixNano)
{-# INLINE maybeTimeUnixNano'LogRecord #-}

maybeTimeUnixNano'Metric :: Lens (Metric a) (Maybe Timestamp)
maybeTimeUnixNano'Metric f m =
  fmap (\x -> m{Metric.maybeTimeUnixNano = x}) (f m.maybeTimeUnixNano)
{-# INLINE maybeTimeUnixNano'Metric #-}
