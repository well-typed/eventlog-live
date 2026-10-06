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

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue)
import Control.Exception (bracket_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (runExceptT)
import Data.DList qualified as D
import Data.Default (Default (..))
import Data.Machine (ProcessT, asParts, await, mapping, repeatedly, runT_, stopped, traversing, (~>))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void, absurd)
import GHC.Debug.Stub.Compat (withMyGhcDebug)
import GHC.Eventlog.Live.App.Control (ControlServerApi (..), startControlServer)
import GHC.Eventlog.Live.App.Environment (OpenTelemetrySdkOptions (..), ServiceName (..), lookupLogLevel, lookupOpenTelemetrySdkOptions)
import GHC.Eventlog.Live.App.Exporter.Otlp (exportTelemetry)
import GHC.Eventlog.Live.App.Exporter.Otlp.Core (withExporters)
import GHC.Eventlog.Live.App.Options
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (Logger, filterBySeverity, logDebug, logFatal, logTick, queueLogger, queueSource, stderrLogger)
import GHC.Eventlog.Live.Machine.Core (Tick (..), dropTick, onlyTick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.Validate (validateInput)
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Processor (InstrumentationScope (..), Resource (..), processEventlogTelemetry, processInternalTelemetry)
import GHC.Eventlog.Live.Source (runWithEventlogSourceHandle, withEventlogSourceHandle)
import GHC.Eventlog.Live.Types.Attribute (AttrValue (..), (~=))
import GHC.Eventlog.Socket.Compat (startMyEventlogSocket)
import GHC.IsList (IsList (..))
import GHC.RTS.Events (Event (..))
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP
import Options.Applicative qualified as O
import Paths_eventlog_live qualified as App
import System.Exit (die)

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

  -- Create a queue for export requests
  exportRequestQueue <- newTQueueIO

  -- Construct a logger
  logLevel <- either die pure =<< runExceptT lookupLogLevel
  let logger =
        filterBySeverity logLevel $
          stderrLogger <> queueLogger internalTelemetryQueue

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
          logDebug logger $
            "Reading configuration file from " <> T.pack configFile
          let onConfigError :: String -> IO x
              onConfigError errMsg = do
                logFatal logger (T.pack errMsg)
          config <-
            either onConfigError pure =<< C.readConfigFile configFile
          logDebug logger $
            "Configuration file:\n" <> C.prettyConfig config
          pure config

    -- Read the configuration file and add derived settings.
    fullConfig <-
      C.toFullConfig eventlogFlushIntervalS
        <$> maybe (pure def) readConfigFile maybeConfigFile
    logDebug logger $
      "Batch interval is " <> T.pack (show fullConfig.batchIntervalMs) <> "ms"
    logDebug logger $
      "Eventlog flush interval is " <> T.pack (show fullConfig.eventlogFlushIntervalX) <> "x"

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

    -- Create machine to process eventlog into export requests.
    let eventlogProcessor ccdb ipedb =
          M.liftTick M.withStartTime
            ~> M.fanoutTick
              [ -- Log a warning if no input has been received after 10 ticks.
                validateInput logger 10
              , -- Log ticks.
                logTicks logger
              , -- If no cost-centre database was provided, index the cost-centre events.
                indexCostCentreEvents (ccdb `onlyIf` isNothing maybeCCDBPath)
              , -- If no info-prov database was provided, index the info-prov events.
                indexInfoProvEvents (ipedb `onlyIf` isNothing maybeIpeDBPath)
              , -- Process the eventlog events.
                processEventlogTelemetry logger fullConfig eventlogResource appScope maybeHeapProfBreakdown ccdb ipedb
                  ~> M.liftTick (mapping D.singleton)
              ]
            ~> M.liftTick asParts
            ~> enqueue exportRequestQueue

    -- Create thread to process eventlog into export requests.
    eventlogProcessorFinished <- newEmptyMVar
    let runEventlogProcessor = do
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
                        (eventlogProcessor ccdb ipedb)
          putMVar eventlogProcessorFinished ()
    _eventlogProcessor <- forkIO runEventlogProcessor

    -- Create a resource to represent the eventlog-live process.
    let internalResource :: Resource
        internalResource =
          Resource
            { attrs =
                [ "service.name" ~= AttrText (appName <> "-for-" <> serviceName.serviceName)
                , "service.version" ~= App.version
                ]
            }

    -- Create machine to process eventlog into export requests.
    let internalTelemetryProcessor =
          queueSource internalTelemetryQueue
            ~> traversing (\case Tick -> pure Tick; Item x -> print x >> pure (Item x))
            ~> processInternalTelemetry fullConfig internalResource appScope
            -- NOTE: eventlogProcessor writes ticks to the exportRequestQueue,
            --       and we don't want to duplicate those.
            ~> dropTick
            ~> mapping Item
            ~> enqueue exportRequestQueue

    -- Create thread to process internal telemetry into export requests.
    internalTelemetryProcessorFinished <- newEmptyMVar
    let runInternalTelemetryProcessor = do
          runT_ internalTelemetryProcessor
          putMVar internalTelemetryProcessorFinished ()
    _internalTelemetryProcessor <- forkIO runInternalTelemetryProcessor

    -- Create machine to process export requests.
    exportRequestProcessorFinished <- newEmptyMVar
    let runExportRequestProcessor = do
          withExporters logger exporterOptions $ \exporters ->
            runT_ $
              queueSource exportRequestQueue
                ~> exportTelemetry logger fullConfig exporters
          putMVar exportRequestProcessorFinished ()
    _exportRequestProcessor <- forkIO runExportRequestProcessor

    -- Wait for these threads to finish, then exit.
    () <- takeMVar eventlogProcessorFinished
    () <- takeMVar internalTelemetryProcessorFinished
    () <- takeMVar exportRequestProcessorFinished
    pure ()

-- A machine that enqueues values in a `TQueue`.
enqueue :: TQueue a -> ProcessT IO a Void
enqueue queue = repeatedly $ await >>= liftIO . atomically . writeTQueue queue

-- Log all ticks.
logTicks :: (Monad m) => Logger m -> ProcessT m (Tick x) y
logTicks logger = onlyTick ~> logTicks'
 where
  logTicks' = repeatedly $ await >>= lift . logTick logger

-- Run an action with access to a cost-centre table.
withCostCentreTable :: Maybe FilePath -> DB.Session -> (DB.Table CC.CostCentreId CC.CostCentre -> IO ()) -> IO ()
withCostCentreTable maybeCCDBPath session =
  maybe
    (DB.withNewTable session def)
    (\ccDBPath -> DB.withTableFrom session ccDBPath def)
    maybeCCDBPath

-- Run an action with access to an info-prov table.
withInfoProvTable :: Maybe FilePath -> DB.Session -> (DB.Table IP.InfoProvId IP.InfoProv -> IO ()) -> IO ()
withInfoProvTable maybeIpeDBPath session =
  maybe
    (DB.withNewTable session def)
    (\ipeDBPath -> DB.withTableFrom session ipeDBPath def)
    maybeIpeDBPath

-- Create machine that indexes CostCentre data.
indexCostCentreEvents ::
  Maybe (DB.Table CC.CostCentreId CC.CostCentre) ->
  ProcessT IO (Tick (M.WithStartTime Event)) x
indexCostCentreEvents =
  -- If a cost-centre database was not provided, don't index any new entries.
  maybe stopped (\ccdb -> dropTick ~> DB.indexer (CC.toCostCentre . (.value)) def ccdb ~> mapping absurd)

-- Create machine that indexes InfoProv data.
indexInfoProvEvents ::
  Maybe (DB.Table IP.InfoProvId IP.InfoProv) ->
  ProcessT IO (Tick (M.WithStartTime Event)) x
indexInfoProvEvents =
  -- If an IPE database was not provided, don't index any new entries.
  maybe stopped (\ipedb -> dropTick ~> DB.indexer (IP.toInfoProv . (.value)) def ipedb ~> mapping absurd)

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

onlyIf :: a -> Bool -> Maybe a
onlyIf a b = if b then Just a else Nothing
