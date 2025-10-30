{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Stats
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Stats (
  EventCount,
  eventCountTick,
  Stat (..),
  processStats,
) where

import Control.Exception (Exception (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Machine (ProcessT, await, construct, mapping, repeatedly, (~>))
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import GHC.Eventlog.Live.Logger (logDebug, logError)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Otelcol.Exporter (ExportMetricsResult (..), ExportTraceResult (..))
import GHC.Eventlog.Live.Verbosity (Verbosity)
import StrictList qualified as Strict
import System.Console.ANSI (hNowSupportsANSI)
import System.Console.ANSI qualified as ANSI
import System.IO qualified as IO

{- |
This type represents a count of input events.
-}
newtype EventCount = EventCount {value :: Int64}

{- |
Count the number of events seen between each tick.
-}
eventCountTick :: (Monad m) => ProcessT m (Tick a) EventCount
eventCountTick = M.batchByTickList ~> mapping (EventCount . fromIntegral . length)

{- |
This type represents the various stats produced by the pipeline.
-}
data Stat
  = EventCountStat !EventCount
  | ExportMetricsResultStat !ExportMetricsResult
  | ExportTraceResultStat !ExportTraceResult

{- |
Internal helper.
This type represents the aggregate stats kept by the stats processor.
-}
data Stats = Stats
  { windowSize :: !Int
  , eventCounts :: !(Strict.List Int64)
  , acceptedDataPointsInWindow :: !(Strict.List Int64)
  , rejectedDataPointsTotal :: !Int64
  , acceptedSpansInWindow :: !(Strict.List Int64)
  , rejectedSpansTotal :: !Int64
  , errors :: !(Strict.List Text)
  , displayedLines :: !(First Int)
  }
  deriving (Show)

{- |
Internal helper.
This instance implements the left-biased union of stats.
In @new <> old@, the @new@ argument should contain the latest data.
-}
instance Semigroup Stats where
  (<>) :: Stats -> Stats -> Stats
  new <> old = Stats{..}
   where
    windowSize = max new.windowSize old.windowSize

    trim :: Strict.List a -> Strict.List a
    trim = Strict.take windowSize

    eventCounts = trim (new.eventCounts <> old.eventCounts)
    acceptedDataPointsInWindow = trim (new.acceptedDataPointsInWindow <> old.acceptedDataPointsInWindow)
    rejectedDataPointsTotal = new.rejectedDataPointsTotal + old.rejectedDataPointsTotal
    acceptedSpansInWindow = trim (new.acceptedSpansInWindow <> old.acceptedSpansInWindow)
    rejectedSpansTotal = new.rejectedSpansTotal + old.rejectedSpansTotal
    errors = trim (new.errors <> old.errors)
    displayedLines = new.displayedLines <> old.displayedLines

{- |
Internal helper.
Construct a `Stats` object from an `EventCount`.
-}
fromEventCount :: EventCount -> Stats
fromEventCount eventCount =
  mempty{eventCounts = singletonStrictList eventCount.value}

{- |
Internal helper.
Construct a `Stats` object from an `ExportMetricsResult`.
-}
fromExportMetricsResult ::
  ExportMetricsResult ->
  Stats
fromExportMetricsResult exportMetricResult =
  mempty
    { acceptedDataPointsInWindow = singletonStrictList exportMetricResult.acceptedDataPoints
    , rejectedDataPointsTotal = exportMetricResult.rejectedDataPoints
    , errors = maybeToStrictList $ T.pack . displayException <$> exportMetricResult.maybeSomeException
    }

{- |
Internal helper.
Construct a `Stats` object from an `ExportTraceResult`.
-}
fromExportTraceResult ::
  ExportTraceResult ->
  Stats
fromExportTraceResult exportTraceResult =
  mempty
    { acceptedSpansInWindow = singletonStrictList exportTraceResult.acceptedSpans
    , rejectedSpansTotal = exportTraceResult.rejectedSpans
    , errors = maybeToStrictList $ T.pack . displayException <$> exportTraceResult.maybeSomeException
    }

{- |
Internal helper.
Construct a singleton `Strict.List`.
-}
singletonStrictList :: a -> Strict.List a
singletonStrictList = (`Strict.Cons` Strict.Nil)

{- |
Internal helper.
Variant of `Data.Maybe.maybeToList` for `Strict.List`.
-}
maybeToStrictList :: Maybe a -> Strict.List a
maybeToStrictList = maybe Strict.Nil singletonStrictList

instance Monoid Stats where
  mempty :: Stats
  mempty =
    Stats
      { windowSize = 10
      , eventCounts = mempty
      , acceptedDataPointsInWindow = mempty
      , rejectedDataPointsTotal = 0
      , acceptedSpansInWindow = mempty
      , rejectedSpansTotal = 0
      , errors = mempty
      , displayedLines = First Nothing
      }

{- |
Process and display stats.

__Warning:__ This machine prints to stdout and is intended to be the /only/ function printing to stdout.
-}
processStats ::
  (MonadIO m) =>
  Verbosity ->
  Bool ->
  Int ->
  Int ->
  ProcessT m Stat Void
processStats verbosity stats batchIntervalMs windowSize
  | stats =
      -- If --stats is ENABLED, maintain and display `Stats`.
      let go stats0 =
            await >>= \stat -> do
              let stats1 = updateStats stats0 stat
              stats2 <- liftIO (displayStats verbosity batchIntervalMs stats1)
              go stats2
       in construct $ go mempty{windowSize = windowSize}
  | otherwise =
      -- If --stats is DISABLED, log all incoming `Stat` values.
      repeatedly $ await >>= logStat verbosity

{- |
Internal helper.
Update the current stats based on new input.
-}
updateStats :: Stats -> Stat -> Stats
updateStats old = \case
  EventCountStat eventCount -> fromEventCount eventCount <> old
  ExportMetricsResultStat exportMetricsResult -> fromExportMetricsResult exportMetricsResult <> old
  ExportTraceResultStat exportTraceResult -> fromExportTraceResult exportTraceResult <> old

{- |
Internal helper.
Log a statistic.
-}
logStat :: (MonadIO m) => Verbosity -> Stat -> m ()
logStat verbosity = \case
  EventCountStat eventCount ->
    logDebug verbosity $ "received " <> showText eventCount.value <> " events"
  ExportMetricsResultStat exportMetricsResult -> do
    -- Log exported events.
    logDebug verbosity $ "exported " <> showText exportMetricsResult.acceptedDataPoints <> " metrics"
    -- Log rejected events.
    when (exportMetricsResult.rejectedDataPoints > 0) $
      logError verbosity $
        "rejected " <> showText exportMetricsResult.rejectedDataPoints <> " metrics"
    -- Log exception.
    for_ exportMetricsResult.maybeSomeException $ \someException -> do
      logError verbosity . T.pack $ displayException someException
  ExportTraceResultStat exportTraceResult -> do
    -- Log exported events.
    logDebug verbosity $ "exported " <> showText exportTraceResult.acceptedSpans <> " metrics"
    -- Log rejected events.
    when (exportTraceResult.rejectedSpans > 0) $
      logError verbosity $
        "rejected " <> showText exportTraceResult.rejectedSpans <> " metrics"
    -- Log exception.
    for_ exportTraceResult.maybeSomeException $ \someException -> do
      logError verbosity . T.pack $ displayException someException

{- |
Internal helper.
Display the current stats.
This is intented to be the *only* function printing to the terminal.

TODO: The stats printer should only overwrite the numbers.
-}
displayStats :: Verbosity -> Int -> Stats -> IO Stats
displayStats verbosity intervalMs old@Stats{..} = do
  -- Check if `displayedLines` is empty...
  case displayedLines of
    First Nothing ->
      -- ...if so, this is the first time this function has been evaluated...
      -- ...so we should perform the `warnIfStderrSupportsANSI` check...
      warnIfStderrSupportsANSI verbosity
    First (Just numberOfLines) -> do
      -- ...if not, we should clear the previous lines of output...
      ANSI.cursorUp numberOfLines
      ANSI.clearFromCursorToScreenEnd

  -- Compute the moving average count of items _per second_,
  -- by computing the adjusted average of counts over n batches.
  let averagePerSecond :: Strict.List Int64 -> Int64
      averagePerSecond xs
        | n <= 0 = 0
        | otherwise = (totalXs * 1_000) `div` totalMs
       where
        !n = length xs
        !totalXs = sum xs
        !totalMs = fromIntegral intervalMs * fromIntegral n

  -- The moving average count of input events.
  let averageEventCounts = showText (averagePerSecond eventCounts)

  -- The moving average count of accepted data points.
  let acceptedDataPoints = showText (averagePerSecond acceptedDataPointsInWindow)

  -- The total count of rejected data points.
  let rejectedDataPoints = showText rejectedDataPointsTotal

  -- The moving average count of accepted spans.
  let acceptedSpans = showText (averagePerSecond acceptedSpansInWindow)

  -- The total count of rejected spans.
  let rejectedSpans = showText rejectedSpansTotal

  -- The maxmimum character width of all columns.
  let width =
        maximum . fmap T.length $
          [averageEventCounts, acceptedDataPoints, rejectedDataPoints, acceptedSpans, rejectedSpans]

  -- The left-pad function that uses the previous maximum character width.
  let leftPad text = T.replicate (width - T.length text) " " <> text

  -- The output lines for the stats.
  let outputLines =
        [ "Eventlog"
        , "  Received " <> leftPad averageEventCounts <> " (val/s)"
        , ""
        , "Metrics"
        , "  Exported " <> leftPad acceptedDataPoints <> " (val/s)"
        , "  Rejected " <> leftPad rejectedDataPoints <> " (total)"
        , ""
        , "Traces"
        , "  Exported " <> leftPad acceptedSpans <> " (val/s)"
        , "  Rejected " <> leftPad rejectedSpans <> " (total)"
        ]
          <> concatMap T.lines errors
  for_ outputLines TIO.putStrLn

  pure old{displayedLines = First (Just $ length outputLines)}

{- |
Check if `IO.stderr` supports ANSI codes. If it does, it is likely printed to
the same terminal as `IO.stdout`, which causes issues if @--stats@ is enabled.
-}
warnIfStderrSupportsANSI :: Verbosity -> IO ()
warnIfStderrSupportsANSI verbosity = do
  supportsANSI <- hNowSupportsANSI IO.stderr
  when supportsANSI $ do
    logError verbosity $
      "When statistics are enabled, stderr should be redirected to a file."

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper.
Show a value as `Text`.
-}
showText :: (Show a) => a -> Text
showText = T.pack . show
