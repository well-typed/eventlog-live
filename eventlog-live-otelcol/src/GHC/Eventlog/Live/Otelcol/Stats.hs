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
)
where

import Control.Exception (Exception (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Machine (ProcessT, await, construct, repeatedly, yield)
import Data.Monoid (First (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import GHC.Eventlog.Live.Logger (logDebug, logError, logWarning)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Otelcol.Exporter.Logs (ExportLogsResult (..))
import GHC.Eventlog.Live.Otelcol.Exporter.Metrics (ExportMetricsResult (..))
import GHC.Eventlog.Live.Otelcol.Exporter.Profiles (ExportProfileResult (..))
import GHC.Eventlog.Live.Otelcol.Exporter.Traces (ExportTraceResult (..))
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.Records (HasField (..))
import StrictList qualified as Strict
import System.Console.ANSI (hNowSupportsANSI)
import System.Console.ANSI qualified as ANSI
import System.IO qualified as IO
import Text.Layout.Table qualified as TBL
import Text.Printf (printf)

{- |
This type represents a count of input events.
-}
newtype EventCount
  = EventCount {value :: Int64}
  deriving (Show)

{- |
Count the number of events seen between each tick.
-}
eventCountTick :: (Monad m) => ProcessT m (Tick a) (Tick EventCount)
eventCountTick = construct $ go 0
 where
  go acc =
    await >>= \case
      M.Tick -> yield (M.Item $ EventCount acc) >> yield M.Tick >> go 0
      M.Item{} -> go (acc + 1)

{- |
This type represents the various stats produced by the pipeline.
-}
data Stat
  = EventCountStat !EventCount
  | ExportLogsResultStat !ExportLogsResult
  | ExportMetricsResultStat !ExportMetricsResult
  | ExportTraceResultStat !ExportTraceResult
  | ExportProfileResultStat !ExportProfileResult
  deriving (Show)

{- |
Internal helper.
This type represents the aggregate stats kept by the stats processor.
-}
data Stats = Stats
  { eventCounts :: Row
  , exportedLogRecords :: Row
  , rejectedLogRecords :: Row
  , exportedDataPoints :: Row
  , rejectedDataPoints :: Row
  , exportedSpans :: Row
  , rejectedSpans :: Row
  , exportedProfiles :: Row
  , rejectedProfiles :: Row
  , errors :: !(Strict.List Text)
  , displayedLines :: !(First Int)
  }
  deriving (Show)

{- |
Internal helper.
This type represents a single row of statistics.
-}
data Row = Row
  { total :: !Int64
  , peakRatePerBatch :: !Double
  , window :: !(Strict.List Int64)
  }
  deriving (Show)

instance HasField "ratePerBatch" Row Double where
  getField :: Row -> Double
  getField row = ratePerBatch row.window

{- |
Internal helper.
Computes the rate per batch from a window.
-}
ratePerBatch :: Strict.List Int64 -> Double
ratePerBatch window =
  let !n = length window
   in if n <= 0 then 0 else sum (realToFrac <$> window) / fromIntegral n

{- |
Internal helper.
Create a singleton `Row`.
-}
singletonRow :: Int64 -> Row
singletonRow total = Row{..}
 where
  peakRatePerBatch = realToFrac total
  window = singletonStrictList total

{- |
Internal helper.
This implements the left-biased union of rows.
In @unionRow new old@, the @new@ argument should contain the latest data.
-}
unionRow :: Int -> Row -> Row -> Row
unionRow windowSize new old = Row{..}
 where
  total = new.total + old.total
  window = Strict.take windowSize (new.window <> old.window)
  peakRatePerBatch = maximum [new.peakRatePerBatch, old.peakRatePerBatch, ratePerBatch window]

{- |
Internal helper.
This instance implements the left-biased union of stats.
In @new <> old@, the @new@ argument should contain the latest data.
-}
unionStats :: Int -> Stats -> Stats -> Stats
unionStats windowSize new old = Stats{..}
 where
  eventCounts = unionRow windowSize new.eventCounts old.eventCounts
  exportedLogRecords = unionRow windowSize new.exportedLogRecords old.exportedLogRecords
  rejectedLogRecords = unionRow windowSize new.rejectedLogRecords old.rejectedLogRecords
  exportedDataPoints = unionRow windowSize new.exportedDataPoints old.exportedDataPoints
  rejectedDataPoints = unionRow windowSize new.rejectedDataPoints old.rejectedDataPoints
  exportedSpans = unionRow windowSize new.exportedSpans old.exportedSpans
  rejectedSpans = unionRow windowSize new.rejectedSpans old.rejectedSpans
  exportedProfiles = unionRow windowSize new.exportedProfiles old.exportedProfiles
  rejectedProfiles = unionRow windowSize new.rejectedProfiles old.rejectedProfiles
  errors = Strict.take windowSize (new.errors <> old.errors)
  displayedLines = new.displayedLines <> old.displayedLines

{- |
Internal helper.
Construct a `Stats` object from an `EventCount`.
-}
fromEventCount :: EventCount -> Stats
fromEventCount eventCount = def{eventCounts = singletonRow eventCount.value}

{- |
Internal helper.
Construct a `Stats` object from an `ExportLogsResult`.
-}
fromExportLogsResult :: ExportLogsResult -> Stats
fromExportLogsResult exportLogsResult =
  def
    { exportedLogRecords = singletonRow exportLogsResult.exportedLogRecords
    , rejectedLogRecords = singletonRow exportLogsResult.rejectedLogRecords
    , errors = maybeToStrictList $ T.pack . displayException <$> exportLogsResult.maybeSomeException
    }

{- |
Internal helper.
Construct a `Stats` object from an `ExportMetricsResult`.
-}
fromExportMetricsResult :: ExportMetricsResult -> Stats
fromExportMetricsResult exportMetricResult =
  def
    { exportedDataPoints = singletonRow exportMetricResult.exportedDataPoints
    , rejectedDataPoints = singletonRow exportMetricResult.rejectedDataPoints
    , errors = maybeToStrictList $ T.pack . displayException <$> exportMetricResult.maybeSomeException
    }

{- |
Internal helper.
Construct a `Stats` object from an `ExportTraceResult`.
-}
fromExportTraceResult :: ExportTraceResult -> Stats
fromExportTraceResult exportTraceResult =
  def
    { exportedSpans = singletonRow exportTraceResult.exportedSpans
    , rejectedSpans = singletonRow exportTraceResult.rejectedSpans
    , errors = maybeToStrictList $ T.pack . displayException <$> exportTraceResult.maybeSomeException
    }

{- |
Internal helper.
Construct a `Stats` object from an `ExportProfileResult`.
-}
fromExportProfileResult :: ExportProfileResult -> Stats
fromExportProfileResult exportProfileResult =
  def
    { exportedProfiles = singletonRow exportProfileResult.exportedProfiles
    , rejectedProfiles = singletonRow exportProfileResult.rejectedProfiles
    , errors = maybeToStrictList $ T.pack . displayException <$> exportProfileResult.maybeSomeException
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

{- |
Internal helper.
This instance implements the empty row.
-}
instance Default Row where
  def :: Row
  def = Row{..}
   where
    total = 0
    peakRatePerBatch = 0
    window = Strict.Nil

{- |
Internal helper.
This implements the empty stats.
-}
instance Default Stats where
  def :: Stats
  def = Stats{..}
   where
    eventCounts = def
    exportedLogRecords = def
    rejectedLogRecords = def
    exportedDataPoints = def
    rejectedDataPoints = def
    exportedSpans = def
    rejectedSpans = def
    exportedProfiles = def
    rejectedProfiles = def
    errors = mempty
    displayedLines = First Nothing

{- |
Process and display stats.

__Warning:__ This machine prints to stdout and is intended to be the /only/ function printing to stdout.
-}
processStats ::
  (MonadIO m) =>
  Verbosity ->
  Bool ->
  Double ->
  Int ->
  ProcessT m Stat Void
processStats verbosity stats eventlogFlushIntervalS windowSize
  | stats =
      -- If --stats is ENABLED, maintain and display `Stats`.
      let go stats0 =
            await >>= \stat -> do
              -- Log the incoming `Stat` value.
              logStat verbosity stat
              -- Maintain and display the `Stats`.
              let stats1 = updateStats windowSize stats0 stat
              stats2 <- liftIO (displayStats verbosity eventlogFlushIntervalS stats1)
              go stats2
       in construct $ go def
  | otherwise =
      -- If --stats is DISABLED, log all incoming `Stat` values.
      repeatedly $ await >>= logStat verbosity

{- |
Internal helper.
Update the current stats based on new input.
-}
updateStats :: Int -> Stats -> Stat -> Stats
updateStats windowSize old = \case
  EventCountStat eventCount -> unionStats windowSize (fromEventCount eventCount) old
  ExportLogsResultStat exportLogsResults -> unionStats windowSize (fromExportLogsResult exportLogsResults) old
  ExportMetricsResultStat exportMetricsResult -> unionStats windowSize (fromExportMetricsResult exportMetricsResult) old
  ExportTraceResultStat exportTraceResult -> unionStats windowSize (fromExportTraceResult exportTraceResult) old
  ExportProfileResultStat exportProfileResult -> unionStats windowSize (fromExportProfileResult exportProfileResult) old

{- |
Internal helper.
Log a statistic.
-}
logStat :: (MonadIO m) => Verbosity -> Stat -> m ()
logStat verbosity = \case
  EventCountStat eventCount ->
    -- Log received events.
    when (eventCount.value > 0) $
      logDebug verbosity $
        "Received " <> showText eventCount.value <> " events."
  ExportLogsResultStat exportLogsResult -> do
    -- Log exported events.
    when (exportLogsResult.exportedLogRecords > 0) $
      logDebug verbosity $
        "Exported " <> showText exportLogsResult.exportedLogRecords <> " logs."
    -- Log rejected events.
    when (exportLogsResult.rejectedLogRecords > 0) $
      logError verbosity $
        "Rejected " <> showText exportLogsResult.rejectedLogRecords <> " logs."
    -- Log exception.
    for_ exportLogsResult.maybeSomeException $ \someException -> do
      logError verbosity . T.pack $ displayException someException
  ExportMetricsResultStat exportMetricsResult -> do
    -- Log exported events.
    when (exportMetricsResult.exportedDataPoints > 0) $
      logDebug verbosity $
        "Exported " <> showText exportMetricsResult.exportedDataPoints <> " metrics."
    -- Log rejected events.
    when (exportMetricsResult.rejectedDataPoints > 0) $
      logError verbosity $
        "Rejected " <> showText exportMetricsResult.rejectedDataPoints <> " metrics."
    -- Log exception.
    for_ exportMetricsResult.maybeSomeException $ \someException -> do
      logError verbosity . T.pack $ displayException someException
  ExportTraceResultStat exportTraceResult -> do
    -- Log exported events.
    when (exportTraceResult.exportedSpans > 0) $
      logDebug verbosity $
        "Exported " <> showText exportTraceResult.exportedSpans <> " spans."
    -- Log rejected events.
    when (exportTraceResult.rejectedSpans > 0) $
      logError verbosity $
        "Rejected " <> showText exportTraceResult.rejectedSpans <> " spans."
    -- Log exception.
    for_ exportTraceResult.maybeSomeException $ \someException -> do
      logError verbosity . T.pack $ displayException someException
  ExportProfileResultStat exportProfileResult -> do
    -- Log exported events.
    when (exportProfileResult.exportedProfiles > 0) $
      logDebug verbosity $
        "Exported " <> showText exportProfileResult.exportedProfiles <> " profiles."
    -- Log rejected events.
    when (exportProfileResult.rejectedProfiles > 0) $
      logError verbosity $
        "Rejected " <> showText exportProfileResult.rejectedProfiles <> " profiles."
    -- Log exception.
    for_ exportProfileResult.maybeSomeException $ \someException -> do
      logError verbosity . T.pack $ displayException someException

{- |
Internal helper.
Display the current stats.
This is intented to be the *only* function printing to the terminal.

TODO: The stats printer should only overwrite the numbers.
-}
displayStats :: Verbosity -> Double -> Stats -> IO Stats
displayStats verbosity eventlogFlushIntervalS stats = do
  -- Check if `displayedLines` is empty...
  case stats.displayedLines of
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
  let rate :: Row -> Text
      rate row = T.pack . printf "%0.2f" $ row.ratePerBatch / eventlogFlushIntervalS
  let peak :: Row -> Text
      peak row = T.pack . printf "%0.0f" $ row.peakRatePerBatch

  let cSpec :: [TBL.ColSpec]
      cSpec = [TBL.defColSpec, TBL.defColSpec, TBL.numCol, TBL.numCol, TBL.numCol]
  let hSpec :: TBL.HeaderSpec TBL.LineStyle (Maybe Text)
      hSpec = TBL.titlesH [Just "Item", Just "Action", Just "Total (item)", Just "Rate (item/s)", Just "Peak (item/x)"]
  let mkRow :: Maybe Text -> Maybe Text -> Row -> TBL.RowGroup (Maybe Text)
      mkRow item result row = TBL.rowG [item, result, Just (showText row.total), Just (rate row), Just (peak row)]
  let rSpec :: [TBL.RowGroup (Maybe Text)]
      rSpec =
        [ mkRow (Just "Events") (Just "Received") stats.eventCounts
        , mkRow (Just "Logs") (Just "Exported") stats.exportedLogRecords
        , mkRow Nothing (Just "Rejected") stats.rejectedLogRecords
        , mkRow (Just "Metrics") (Just "Exported") stats.exportedDataPoints
        , mkRow Nothing (Just "Rejected") stats.rejectedDataPoints
        , mkRow (Just "Traces") (Just "Exported") stats.exportedSpans
        , mkRow Nothing (Just "Rejected") stats.rejectedSpans
        , mkRow (Just "Profile") (Just "Exported") stats.exportedProfiles
        , mkRow Nothing (Just "Rejected") stats.rejectedProfiles
        ]
  let tSpec :: TBL.TableSpec TBL.LineStyle TBL.LineStyle String (Maybe Text) (Maybe Text)
      tSpec = TBL.columnHeaderTableS cSpec TBL.unicodeS hSpec rSpec
  let table = TBL.tableLinesB tSpec :: [Text]
  for_ table TIO.putStrLn
  pure stats{displayedLines = First (Just $ length table)}

{- |
Check if `IO.stderr` supports ANSI codes. If it does, it is likely printed to
the same terminal as `IO.stdout`, which causes issues if @--stats@ is enabled.
-}
warnIfStderrSupportsANSI :: Verbosity -> IO ()
warnIfStderrSupportsANSI verbosity = do
  supportsANSI <- hNowSupportsANSI IO.stderr
  when supportsANSI $ do
    logWarning verbosity $
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
