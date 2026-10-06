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

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, tryReadTMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueue, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO, readTVar, stateTVar)
import Control.Exception (SomeException, bracket_, handle)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (STM, atomically)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (encode)
import Data.DList qualified as D
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Machine (ProcessT, asParts, await, mapping, repeatedly, stopped, (~>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (absurd)
import GHC.Debug.Stub.Compat (withMyGhcDebug)
import GHC.Eventlog.Live.App.Control (ControlServerApi (..), startControlServer)
import GHC.Eventlog.Live.App.Environment (OpenTelemetrySdkOptions (..), ServiceName (..), lookupLogLevel, lookupOpenTelemetrySdkOptions)
import GHC.Eventlog.Live.App.Exporter.Otlp (exportToOtlp)
import GHC.Eventlog.Live.App.Exporter.Otlp.Core (withExporters)
import GHC.Eventlog.Live.App.Options
import GHC.Eventlog.Live.App.Stats (Stat (..), eventCountTick, processStats)
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (Logger, writeException, writeLog)
import GHC.Eventlog.Live.Logger qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Processor (ExportRequest, InstrumentationScope (..), Resource (..), processEventlogTelemetry, processInternalTelemetry)
import GHC.Eventlog.Live.Source (runWithEventlogSourceHandle, withEventlogSourceHandle)
import GHC.Eventlog.Live.Types.Attribute (AttrValue (..), (~=))
import GHC.Eventlog.Live.Types.Severity (Severity (..))
import GHC.Eventlog.Socket.Compat (startMyEventlogSocket)
import GHC.IsList (IsList (..))
import GHC.RTS.Events (Event (..))
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP
import Network.WebSockets qualified as WS
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

-- TODO:
-- Remove the exporters from the pipeline.
-- The pipeline should by writing export requests to a TQueue.
-- Any exporters should be separate threads that read from this TQueue.
-- For the internal telemetry, there should be a separate pipeline that reads
-- from the internal telemetry channel, aggregates and batches, and writes to
-- the export request channel.
-- This removes the need for concurrent machines.
-- An exporter pipeline should read from the export request channel and forward
-- these requests to the relevant export functions.
-- This saves us having to implement a broadcast queue and the memory leaks
-- that would come with that.

data ViewerServerApi = ViewerServerApi
  { exportToViewers :: ExportRequest -> IO ()
  , stop :: IO ()
  }

startViewerServer :: Logger IO -> IO ViewerServerApi
startViewerServer logger = do
  stVar <- newTVarIO initViewerServerState
  writeLog logger INFO $ "Starting viewer server"

  threadId <-
    forkIO $
      handle (writeException @SomeException logger) $
        WS.runServer "127.0.0.1" 30180 $
          viewerApp logger stVar

  let exportToViewers :: ExportRequest -> IO ()
      exportToViewers exportRequest = do
        writeLog logger INFO $ "Export request to all viewers"
        atomically $ do
          viewers <- (.viewers) <$> readTVar stVar
          for_ viewers $ \viewer -> do
            writeTQueue viewer.queue exportRequest

  let stop :: IO ()
      stop = killThread threadId

  pure ViewerServerApi{..}

newtype ViewerId = ViewerId {unViewerId :: Int}
  deriving (Eq, Num)

data ViewerState = ViewerState
  { queue :: TQueue ExportRequest
  , cancel :: TMVar ()
  }

data ViewerServerState = ViewerServerState
  { nextViewerId :: !ViewerId
  , viewers :: IntMap ViewerState
  }

initViewerServerState :: ViewerServerState
initViewerServerState =
  ViewerServerState{nextViewerId = ViewerId 0, viewers = IM.empty}

newViewer ::
  TVar ViewerServerState -> STM ViewerId
newViewer stVar = do
  queue <- newTQueue
  cancel <- newEmptyTMVar
  stateTVar stVar $ \st ->
    ( st.nextViewerId
    , ViewerServerState
        { nextViewerId = st.nextViewerId + 1
        , viewers = IM.insert st.nextViewerId.unViewerId ViewerState{..} st.viewers
        }
    )

removeViewer ::
  TVar ViewerServerState -> ViewerId -> STM ()
removeViewer stVar viewerId =
  modifyTVar stVar $ \ViewerServerState{..} ->
    ViewerServerState{viewers = IM.delete viewerId.unViewerId viewers, ..}

viewerApp ::
  Logger IO -> TVar ViewerServerState -> WS.ServerApp
viewerApp logger stVar pending = do
  conn <- WS.acceptRequest pending
  viewerId <- atomically (newViewer stVar)
  writeLog logger INFO $ "Created viewer " <> T.show viewerId.unViewerId <> "."

  let handleConnectionException :: WS.ConnectionException -> IO ()
      handleConnectionException = \case
        WS.CloseRequest{} -> do
          writeLog logger INFO $ "Viewer " <> T.show viewerId.unViewerId <> " requested that the connection be closed."
          atomically (removeViewer stVar viewerId)
        WS.ConnectionClosed{} -> do
          writeLog logger INFO $ "Viewer " <> T.show viewerId.unViewerId <> " closed the connection."
          atomically (removeViewer stVar viewerId)
        wsErr -> writeException logger wsErr

  let nextExportRequest :: STM (Maybe ExportRequest)
      nextExportRequest = do
        (IM.lookup viewerId.unViewerId . (.viewers) <$> readTVar stVar) >>= \case
          Nothing -> pure Nothing
          Just me ->
            tryReadTMVar me.cancel >>= \case
              Just () -> removeViewer stVar viewerId >> pure Nothing
              Nothing -> Just <$> readTQueue me.queue

  let viewerLoop :: IO ()
      viewerLoop =
        atomically nextExportRequest >>= \case
          Nothing -> do
            pure () -- exit viewerLoop
          Just er -> do
            writeLog logger INFO $ "Viewer " <> T.show viewerId.unViewerId <> " received export request."
            let erBytes = encode er
            WS.sendTextData conn erBytes
            viewerLoop

  handle handleConnectionException viewerLoop

{- |
The main function for @eventlog-live-otlp@.
-}
main :: IO ()
main = do
  -- Parse the command-line options
  Options{..} <- O.execParser options

  -- Create a channel for internal telemetry
  myTelemetryDataChan <- newTChanIO

  -- Construct a logger
  logLevel <- either die pure =<< runExceptT lookupLogLevel
  let logger =
        M.filterBySeverity logLevel $
          M.stderrLogger <> M.chanLogger myTelemetryDataChan

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

    -- Start the viewer server.
    ViewerServerApi{..} <- startViewerServer logger

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
                  , processInternalTelemetry fullConfig internalResource appScope myTelemetryDataChan
                      ~> M.liftTick (mapping D.singleton)
                  ]
                  ~> M.liftTick asParts
                  -- ...and export it.
                  ~> M.liftTick (repeatedly $ await >>= \er -> liftIO (exportToViewers er))
                  -- ~> M.fanoutTick
                  --   [ M.liftTick (repeatedly $ await >>= \er -> liftIO (exportToViewers er))
                  --   , exportToOtlp logger fullConfig exporters
                  --   ]
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
        let withCostCentreTable =
              case maybeCCDBPath of
                Nothing -> DB.withNewTable session def
                Just ccDBPath -> DB.withTableFrom session ccDBPath def
        let withInfoProvTable =
              case maybeIpeDBPath of
                Nothing -> DB.withNewTable session def
                Just ipeDBPath -> DB.withTableFrom session ipeDBPath def
        withCostCentreTable $ \ccdb ->
          withInfoProvTable $ \ipedb ->
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
