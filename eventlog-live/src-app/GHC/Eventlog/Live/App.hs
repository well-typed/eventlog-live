{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.App
Description : The implementation of @eventlog-live-otlp@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.App (
  main,
) where

import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Exception (bracket_)
import Control.Monad.Trans.Except (runExceptT)
import Data.DList qualified as D
import Data.Default (Default (..))
import Data.Machine (ProcessT, asParts, mapping, stopped, (~>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (absurd)
import GHC.Debug.Stub.Compat (withMyGhcDebug)
import GHC.Eventlog.Live.App.Control (ControlServerApi (..), startControlServer)
import GHC.Eventlog.Live.App.Environment (OpenTelemetrySdkOptions (..), ServiceName (..), lookupLogLevel, lookupOpenTelemetrySdkOptions)
import GHC.Eventlog.Live.App.Exporter.Otlp (exportTelemetry)
import GHC.Eventlog.Live.App.Exporter.Otlp.Core (withExporters)
import GHC.Eventlog.Live.App.Options
import GHC.Eventlog.Live.App.Stats (Stat (..), eventCountTick, processStats)
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (writeLog)
import GHC.Eventlog.Live.Logger qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Processor (InstrumentationScope (..), Resource (..), processEventlogTelemetry, processInternalTelemetry)
import GHC.Eventlog.Live.Source (runWithEventlogSourceHandle, withEventlogSourceHandle)
import GHC.Eventlog.Live.Types.Attribute (AttrValue (..), (~=))
import GHC.Eventlog.Live.Types.Severity (Severity (..))
import GHC.Eventlog.Socket.Compat (startMyEventlogSocket)
import GHC.IsList (IsList (..))
import GHC.RTS.Events (Event (..))
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP
import Options.Applicative qualified as O
import Paths_eventlog_live qualified as App
import System.Exit (die, exitFailure)

--------------------------------------------------------------------------------
-- Instrumentation Scope
--------------------------------------------------------------------------------

-- 2025-09-22:
-- Once `cabal2nix` supports Cabal 3.12, this can once again use the value from:
-- `PackageInfo_eventlog_live.name`.
appName :: Text
appName = "eventlog-live-otlp"

appScope :: InstrumentationScope
appScope = InstrumentationScope{name = appName, version = App.version}

{- |
The main function for @eventlog-live-otlp@.
-}
main :: IO ()
main = do
  -- Parse the command-line options
  Options{..} <- O.execParser options

  -- Create a queue for internal telemetry
  internalTelemetryQueue <- newTQueueIO

  -- Construct a logger
  logLevel <- either die pure =<< runExceptT lookupLogLevel
  let logger =
        M.filterBySeverity logLevel $
          M.stderrLogger <> M.queueLogger internalTelemetryQueue

  -- Lookup the OpenTelemetry SDK options
  OpenTelemetrySdkOptions{..} <-
    either die pure =<< runExceptT (lookupOpenTelemetrySdkOptions logger)

  -- Instument THIS PROGRAM with eventlog-socket and/or ghc-debug.
  let MyDebugOptions{..} = myDebugOptions
  startMyEventlogSocket logger maybeMyEventlogSocket
  withMyGhcDebug logger maybeMyGhcDebugSocket $ do
    --
    -- Start the control server.
    controlServerApi <- startControlServer logger controlOptions

    -- Read the configuration file.
    let readConfigFile configFile = do
          writeLog logger DEBUG $
            "Reading configuration file from " <> T.pack configFile
          let onConfigError :: String -> IO x
              onConfigError errMsg = do
                writeLog logger FATAL (T.pack errMsg)
                exitFailure
          config <-
            either onConfigError pure =<< C.readConfigFile configFile
          writeLog logger DEBUG $
            "Configuration file:\n" <> C.prettyConfig config
          pure config

    -- Read the configuration file and add derived settings.
    fullConfig <-
      C.toFullConfig eventlogFlushIntervalS
        <$> maybe (pure def) readConfigFile maybeConfigFile
    writeLog logger DEBUG $
      "Batch interval is " <> T.pack (show fullConfig.batchIntervalMs) <> "ms"
    writeLog logger DEBUG $
      "Eventlog flush interval is " <> T.pack (show fullConfig.eventlogFlushIntervalX) <> "x"

    -- Determine the window size for statistics
    let windowSizeX =
          (10 *) . maximum @[] $
            [ fullConfig.eventlogFlushIntervalX
            , C.maximumAggregationBatches fullConfig
            , C.maximumExportBatches fullConfig
            ]

    -- Find the service name, if any:
    let !serviceName =
          fromMaybe (ServiceName "undefined") $
            (.serviceName) =<< maybeResourceAttributes

    -- Create a resource to represent the monitored process.
    let eventlogResource :: Resource
        eventlogResource =
          Resource
            { attrs =
                fromList $
                  uncurry (~=) <$> maybe [] (.attributes) maybeResourceAttributes
            }

    -- Create a resource to represent the eventlog-live process.
    let internalResource :: Resource
        internalResource =
          Resource
            { attrs =
                [ "service.name" ~= AttrText (appName <> "-for-" <> serviceName.serviceName)
                , "service.version" ~= App.version
                ]
            }

    -- Create machine that indexes CostCentre data.
    let indexCostCentreEvents ::
          DB.Table CC.CostCentreId CC.CostCentre ->
          ProcessT IO (Tick (M.WithStartTime Event)) (Tick x)
        indexCostCentreEvents ccdb
          -- If a cost-centre database was provided, don't index any new entries.
          | isJust maybeCCDBPath = stopped
          | otherwise = M.liftTick (DB.indexer (CC.toCostCentre . (.value)) def ccdb ~> mapping absurd)

    -- Create machine that indexes InfoProv data.
    let indexInfoProvEvents ::
          DB.Table IP.InfoProvId IP.InfoProv ->
          ProcessT IO (Tick (M.WithStartTime Event)) (Tick x)
        indexInfoProvEvents ipedb
          -- If an IPE database was provided, don't index any new entries.
          | isJust maybeIpeDBPath = stopped
          | otherwise = M.liftTick (DB.indexer (IP.toInfoProv . (.value)) def ipedb ~> mapping absurd)

    -- Create the full machine to process eventlog data.
    let processAndExportTelemetry ccdb ipedb exporters =
          M.liftTick M.withStartTime
            ~> M.fanoutTick
              [ -- Log a warning if no input has been received after 10 ticks.
                M.validateInput logger 10
              , -- Count the number of input events between each tick.
                eventCountTick
                  ~> mapping (fmap (D.singleton . EventCountStat))
              , -- Process eventlog and internal telemetry...
                M.fanoutTickCC
                  [ M.fanoutTick
                      [ -- Process CostCentre events.
                        indexCostCentreEvents ccdb
                      , -- Process InfoProv events.
                        indexInfoProvEvents ipedb
                      , -- Process the eventlog events.
                        processEventlogTelemetry logger fullConfig eventlogResource appScope maybeHeapProfBreakdown ccdb ipedb
                          ~> M.liftTick (mapping D.singleton)
                      ]
                  , processInternalTelemetry fullConfig internalResource appScope internalTelemetryQueue
                      ~> M.liftTick (mapping D.singleton)
                  ]
                  ~> M.liftTick asParts
                  -- ...and export it.
                  ~> exportTelemetry logger fullConfig exporters
              ]
            -- Process the statistics
            -- TODO: windowSize should be the maximum of all aggregation and export intervals
            ~> M.liftTick (asParts ~> processStats logger stats eventlogFlushIntervalS windowSizeX)
            -- Validate the consistency of the tick
            ~> M.validateTicks logger
            ~> M.dropTick

    -- Open a connection to the OpenTelemetry Collector.
    withExporters logger exporterOptions $ \exporters -> do
      DB.withNewSession def $ \session -> do
        withCostCentreTable maybeCCDBPath session $ \ccdb ->
          withInfoProvTable maybeIpeDBPath session $ \ipedb ->
            withEventlogSourceHandle
              logger
              eventlogSocketTimeoutS
              eventlogSocketTimeoutExponent
              eventlogSourceOptions
              $ \eventlogSourceHandle -> do
                -- Notify the control server of the connection status.
                let newConnection = controlServerApi.notifyNewConnection serviceName eventlogSourceHandle
                let endConnection = controlServerApi.notifyEndConnection serviceName
                bracket_ newConnection endConnection $
                  -- Run the eventlog processor.
                  runWithEventlogSourceHandle
                    logger
                    eventlogSourceHandle
                    fullConfig.batchIntervalMs
                    Nothing
                    maybeEventlogLogFile
                    (processAndExportTelemetry ccdb ipedb exporters)

withCostCentreTable :: Maybe FilePath -> DB.Session -> (DB.Table CC.CostCentreId CC.CostCentre -> IO ()) -> IO ()
withCostCentreTable maybeCCDBPath session =
  maybe
    (DB.withNewTable session def)
    (\ccDBPath -> DB.withTableFrom session ccDBPath def)
    maybeCCDBPath

withInfoProvTable :: Maybe FilePath -> DB.Session -> (DB.Table IP.InfoProvId IP.InfoProv -> IO ()) -> IO ()
withInfoProvTable maybeIpeDBPath session =
  maybe
    (DB.withNewTable session def)
    (\ipeDBPath -> DB.withTableFrom session ipeDBPath def)
    maybeIpeDBPath
