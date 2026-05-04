{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing -fconstraint-solver-iterations=0 #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol (
  main,
) where

import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Exception (bracket_)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Default (Default (..))
import Data.Foldable qualified as F
import Data.Machine (Process, ProcessT, asParts, mapping, (~>))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import GHC.Debug.Stub.Compat (withMyGhcDebug)
import GHC.Eventlog.Live.Data.Attribute (AttrValue (AttrText), (~=))
import GHC.Eventlog.Live.Data.LogRecord (LogRecord (..))
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (MyTelemetryData, writeLog)
import GHC.Eventlog.Live.Logger qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Profile qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otelcol.Control (ControlServerApi (..), startControlServer)
import GHC.Eventlog.Live.Otelcol.Exporter.Logs (exportResourceLogs)
import GHC.Eventlog.Live.Otelcol.Exporter.Metrics (exportResourceMetrics)
import GHC.Eventlog.Live.Otelcol.Exporter.Profiles (exportResourceProfiles)
import GHC.Eventlog.Live.Otelcol.Exporter.Traces (exportResourceSpans)
import GHC.Eventlog.Live.Otelcol.Options
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core
import GHC.Eventlog.Live.Otelcol.Processor.Common.Logs (ToLogRecord (..), toExportLogsServiceRequest, toResourceLogs, toScopeLogs)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Metrics (toExportMetricsServiceRequest, toResourceMetrics, toScopeMetrics)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Profiles (toExportProfileServiceRequest)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Traces (toExportTracesServiceRequest, toResourceSpans, toScopeSpans)
import GHC.Eventlog.Live.Otelcol.Processor.Heap (processHeapEvents)
import GHC.Eventlog.Live.Otelcol.Processor.Logs (processLogEvents)
import GHC.Eventlog.Live.Otelcol.Processor.Profiles (processCallStackData, processProfileEvents)
import GHC.Eventlog.Live.Otelcol.Processor.Threads (processThreadEvents)
import GHC.Eventlog.Live.Otelcol.Stats (Stat (..), eventCountTick, processStats)
import GHC.Eventlog.Live.Source (runWithEventlogSourceHandle, withEventlogSourceHandle)
import GHC.Eventlog.Socket.Compat (startMyEventlogSocket)
import GHC.RTS.Events (Event (..))
import Lens.Family2 ((.~))
import Network.GRPC.Client qualified as G
import Network.GRPC.Common qualified as G
import Options.Applicative qualified as O
import Paths_eventlog_live_otelcol qualified as EventlogLive
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService_Fields qualified as OPS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT

{- |
The main function for @eventlog-live-otelcol@.
-}
main :: IO ()
main = do
  Options{..} <- O.execParser options

  -- Construct the logging action
  myTelemetryDataChan <- newTChanIO
  let logger =
        M.filterBySeverity severityThreshold $
          M.stderrLogger <> M.chanLogger myTelemetryDataChan

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
          config <- C.readConfigFile logger configFile
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
          (10 *) . maximum $
            [ fullConfig.eventlogFlushIntervalX
            , C.maximumAggregationBatches fullConfig
            , C.maximumExportBatches fullConfig
            ]

    -- Resolve the service name (or use a default).
    let serviceName :: ServiceName
        serviceName = fromMaybe (ServiceName "undefined") maybeServiceName

    -- Create a resource to represent the monitored process.
    let eventlogResource :: OR.Resource
        eventlogResource =
          messageWith
            [ OM.attributes
                .~ mapMaybe
                  toMaybeKeyValue
                  [ "service.name" ~= AttrText serviceName.serviceName
                  ]
            ]

    -- Create machine that processes eventlog data into telemetry data
    let processEventlogTelemetry :: ProcessT IO (Tick Event) (Tick ResourceTelemetryData)
        processEventlogTelemetry =
          M.liftTick (M.withStartTime logger)
            ~> M.fanoutTick
              [ -- Process the heap events.
                processHeapEvents logger maybeHeapProfBreakdown fullConfig
                  ~> mapping (fmap (fmap TelemetryData'Metric))
              , -- Process the log events.
                processLogEvents fullConfig
                  ~> mapping (fmap (fmap TelemetryData'Log))
              , -- Process the thread events.
                processThreadEvents logger fullConfig
                  ~> mapping (fmap (fmap (either TelemetryData'Metric TelemetryData'Span)))
              , -- Process the profile events.
                processProfileEvents logger fullConfig
                  ~> mapping (fmap (fmap TelemetryData'Profile))
              ]
            ~> M.liftTick (asResourceTelemetryData eventlogResource eventlogLiveScope)

    -- Create a resource to represent the eventlog-live process.
    let internalResource :: OR.Resource
        internalResource =
          messageWith
            [ OM.attributes
                .~ mapMaybe
                  toMaybeKeyValue
                  [ "service.name" ~= AttrText (eventlogLiveName <> "-for-" <> serviceName.serviceName)
                  , "service.version" ~= eventlogLiveVersion
                  ]
            ]

    -- Create the machine that processes internal telemetry data
    --
    -- NOTE: This process only takes a stream of inputs to use their tick.
    let processInternalTelemetry :: ProcessT IO (Tick x) (Tick ResourceTelemetryData)
        processInternalTelemetry =
          M.mergeWithTickCC (M.chanSource myTelemetryDataChan)
            ~> processInternalTelemetryData fullConfig
            ~> M.liftTick (asResourceTelemetryData internalResource eventlogLiveScope)

    -- Create the full machine to process eventlog data.
    let processAndExportTelemetry conn =
          M.fanoutTick
            [ -- Log a warning if no input has been received after 10 ticks.
              M.validateInput logger 10
            , -- Count the number of input events between each tick.
              eventCountTick
                ~> mapping (fmap (D.singleton . EventCountStat))
            , -- Process eventlog and internal telemetry...
              M.fanoutTickCC
                [ processEventlogTelemetry ~> mapping (fmap D.singleton)
                , processInternalTelemetry ~> mapping (fmap D.singleton)
                ]
                ~> M.liftTick asParts
                -- ...and export it.
                ~> exportResourceTelemetryData fullConfig conn
            ]
            -- Process the statistics
            -- TODO: windowSize should be the maximum of all aggregation and export intervals
            ~> M.liftTick (asParts ~> processStats logger stats eventlogFlushIntervalS windowSizeX)
            -- Validate the consistency of the tick
            ~> M.validateTicks logger
            ~> M.dropTick

    -- Open a connection to the OpenTelemetry Collector.
    let OpenTelemetryCollectorOptions{..} = openTelemetryCollectorOptions
    G.withConnection G.def openTelemetryCollectorServer $ \conn -> do
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
              (processAndExportTelemetry conn)

data TelemetryData
  = TelemetryData'Log OL.LogRecord
  | TelemetryData'Metric OM.Metric
  | TelemetryData'Span OT.Span
  | TelemetryData'Profile M.CallStackData

data ResourceTelemetryData
  = ResourceTelemetryData'Log OL.ResourceLogs
  | ResourceTelemetryData'Metric OM.ResourceMetrics
  | ResourceTelemetryData'Span OT.ResourceSpans
  | ResourceTelemetryData'Profile OP.ProfilesData

{- |
Internal helper.
Export resource telemetry data and yield statistics.
-}
exportResourceTelemetryData ::
  FullConfig ->
  G.Connection ->
  ProcessT IO (Tick ResourceTelemetryData) (Tick (DList Stat))
exportResourceTelemetryData fullConfig connection =
  M.fanoutTick
    [ -- Export logs.
      runIf (C.shouldExportLogs fullConfig) $
        M.liftTick (mapping getResourceLogs ~> asParts ~> mapping D.singleton)
          -- NOTE: This is required to combine different resource telemetry
          --       streams. However, it has the "unfortunate" side-effect of
          --       making it impossible to not batch once per interval.
          ~> M.batchByTick
          ~> M.liftTick (mapping (toExportLogsServiceRequest . D.toList))
          ~> exportResourceLogs connection
          ~> M.liftTick (mapping (D.singleton . ExportLogsResultStat))
    , -- Export metrics.
      runIf (C.shouldExportMetrics fullConfig) $
        M.liftTick (mapping getResourceMetrics ~> asParts ~> mapping D.singleton)
          -- NOTE: See note above.
          ~> M.batchByTick
          ~> M.liftTick (mapping (toExportMetricsServiceRequest . D.toList))
          ~> exportResourceMetrics connection
          ~> M.liftTick (mapping (D.singleton . ExportMetricsResultStat))
    , -- Export spans.
      runIf (C.shouldExportTraces fullConfig) $
        M.liftTick (mapping getResourceSpans ~> asParts ~> mapping D.singleton)
          -- NOTE: See note above.
          ~> M.batchByTick
          ~> M.liftTick (mapping (toExportTracesServiceRequest . D.toList))
          ~> exportResourceSpans connection
          ~> M.liftTick (mapping (D.singleton . ExportTraceResultStat))
    , -- Export profiles.
      runIf (C.shouldExportProfiles fullConfig) $
        M.liftTick (mapping getResourceProfiles ~> asParts ~> mapping toExportProfileServiceRequest)
          ~> exportResourceProfiles connection
          ~> M.liftTick (mapping (D.singleton . ExportProfileResultStat))
    ]

getResourceLogs :: ResourceTelemetryData -> Maybe OL.ResourceLogs
getResourceLogs = \case
  (ResourceTelemetryData'Log resourceLogs) -> Just resourceLogs
  _otherwise -> Nothing

getResourceMetrics :: ResourceTelemetryData -> Maybe OM.ResourceMetrics
getResourceMetrics = \case
  (ResourceTelemetryData'Metric resourceMetrics) -> Just resourceMetrics
  _otherwise -> Nothing

getResourceSpans :: ResourceTelemetryData -> Maybe OT.ResourceSpans
getResourceSpans = \case
  (ResourceTelemetryData'Span resourceSpans) -> Just resourceSpans
  _otherwise -> Nothing

getResourceProfiles :: ResourceTelemetryData -> Maybe OP.ProfilesData
getResourceProfiles = \case
  (ResourceTelemetryData'Profile profilesData) -> Just profilesData
  _otherwise -> Nothing

{- |
Internal helper.
Repack a stream of `TelemetryData` to batched `ResourceTelemetryData`.
-}
asResourceTelemetryData ::
  (Foldable f) =>
  OR.Resource ->
  OC.InstrumentationScope ->
  Process (f TelemetryData) ResourceTelemetryData
asResourceTelemetryData resource instrumentationScope =
  mapping (toResourceTelemetryData . F.toList) ~> asParts
 where
  toResourceTelemetryData ::
    [TelemetryData] ->
    [ResourceTelemetryData]
  toResourceTelemetryData telemetryData =
    catMaybes [maybeResourceLogs, maybeResourceMetrics, maybeResourceSpans, maybeProfiles]
   where
    (logRecords, metrics, spans, profiles) = partitionTelemetryData telemetryData

    maybeResourceLogs = do
      scopeLogs <- toScopeLogs instrumentationScope logRecords
      resourceLogs <- toResourceLogs resource [scopeLogs]
      pure $ ResourceTelemetryData'Log resourceLogs
    maybeResourceMetrics = do
      scopeMetrics <- toScopeMetrics instrumentationScope metrics
      resourceMetrics <- toResourceMetrics resource [scopeMetrics]
      pure $ ResourceTelemetryData'Metric resourceMetrics
    maybeResourceSpans = do
      scopeSpans <- toScopeSpans instrumentationScope spans
      resourceSpans <- toResourceSpans resource [scopeSpans]
      pure $ ResourceTelemetryData'Span resourceSpans
    maybeProfiles = do
      (resourceProfile, dictionary) <-
        ifNonEmpty profiles $
          processCallStackData resource instrumentationScope profiles
      pure $
        ResourceTelemetryData'Profile $
          messageWith
            [ OPS.dictionary .~ dictionary
            , OPS.resourceProfiles .~ [resourceProfile]
            ]

{- |
Partition a stream of `TelemetryData` batches to individual batches for each kind of telemetry data.
-}
partitionTelemetryData :: [TelemetryData] -> ([OL.LogRecord], [OM.Metric], [OT.Span], [M.CallStackData])
partitionTelemetryData = go ([], [], [], [])
 where
  go :: ([OL.LogRecord], [OM.Metric], [OT.Span], [M.CallStackData]) -> [TelemetryData] -> ([OL.LogRecord], [OM.Metric], [OT.Span], [M.CallStackData])
  go (logs, metrics, spans, profiles) = \case
    [] -> (reverse logs, reverse metrics, reverse spans, reverse profiles)
    (TelemetryData'Log log : rest) -> go (log : logs, metrics, spans, profiles) rest
    (TelemetryData'Metric metric : rest) -> go (logs, metric : metrics, spans, profiles) rest
    (TelemetryData'Span span : rest) -> go (logs, metrics, span : spans, profiles) rest
    (TelemetryData'Profile profile : rest) -> go (logs, metrics, spans, profile : profiles) rest

{- |
Internal helper.
Process internal telemetry data.
-}
processInternalTelemetryData ::
  FullConfig ->
  Process (Tick MyTelemetryData) (Tick (DList TelemetryData))
processInternalTelemetryData fullConfig =
  M.fanoutTick
    [ -- Process internal log messages.
      M.liftTick (mapping getMyLogRecord ~> asParts ~> mapping (D.singleton . TelemetryData'Log . toLogRecord))
        ~> M.batchByTicks (C.processorExportBatches (.logs) (.internalLogMessage) fullConfig)
        -- TODO: Any internal metrics should be processed below.
    ]

getMyLogRecord :: MyTelemetryData -> Maybe LogRecord
getMyLogRecord = \case
  M.MyTelemetryData'LogRecord{..} -> Just logRecord
  M.MyTelemetryData'Metric{} -> Nothing

--------------------------------------------------------------------------------
-- Instrumentation Scope
--------------------------------------------------------------------------------

-- 2025-09-22:
-- Once `cabal2nix` supports Cabal 3.12, this can once again use the value from:
-- `PackageInfo_eventlog_live_otelcol.name`.
eventlogLiveName :: Text
eventlogLiveName = "eventlog-live-otelcol"

eventlogLiveVersion :: Text
eventlogLiveVersion = T.pack (showVersion EventlogLive.version)

eventlogLiveScope :: OC.InstrumentationScope
eventlogLiveScope =
  messageWith
    [ OC.name .~ eventlogLiveName
    , OC.version .~ eventlogLiveVersion
    ]
