module GHC.Eventlog.Live.App.Exporter.Otlp (
  exportTelemetryData,
) where

import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (ProcessT, asParts, mapping, (~>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text qualified as T
import Data.Version (showVersion)
import GHC.Eventlog.Live.App.Environment (PerSignal, Signal (..), forSignal)
import GHC.Eventlog.Live.App.Exporter.Otlp.Core (Exporter, messageWith, toMaybeKeyValues)
import GHC.Eventlog.Live.App.Exporter.Otlp.Logs (exportResourceLogs, toExportLogsServiceRequest, toLogRecords, toResourceLogs, toScopeLogs)
import GHC.Eventlog.Live.App.Exporter.Otlp.Metrics (exportResourceMetrics, toExportMetricsServiceRequest, toMetric, toResourceMetrics, toScopeMetrics)
import GHC.Eventlog.Live.App.Exporter.Otlp.Profiles (exportResourceProfiles, toExportProfileServiceRequest, toProfiles, toProfilesData, toResourceProfiles, toScopeProfiles)
import GHC.Eventlog.Live.App.Exporter.Otlp.Traces (exportResourceSpans, toExportTracesServiceRequest, toResourceSpans, toScopeSpans, toSpans)
import GHC.Eventlog.Live.App.Stats (Stat (..))
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Data.Logs (SomeLogs)
import GHC.Eventlog.Live.Data.Metric (SomeMetric)
import GHC.Eventlog.Live.Data.Sample (SomeSamples)
import GHC.Eventlog.Live.Data.Span (SomeSpans)
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Processor (InstrumentationScope (..), Resource (..), TelemetryData (..))
import GHC.Eventlog.Live.Processor.Core
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as OR
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT

{- |
Convert a `Resource` to an OTLP `OR.Resource`.
-}
toResource :: Resource -> OR.Resource
toResource Resource{..} =
  messageWith [OR.attributes .~ toMaybeKeyValues attrs]

toInstrumentationScope :: InstrumentationScope -> OC.InstrumentationScope
toInstrumentationScope InstrumentationScope{..} =
  messageWith
    [ OC.name .~ name
    , OC.version .~ T.pack (showVersion version)
    ]

data ResourceTelemetryData
  = ResourceTelemetryData'Log OL.ResourceLogs
  | ResourceTelemetryData'Metric OM.ResourceMetrics
  | ResourceTelemetryData'Span OT.ResourceSpans
  | ResourceTelemetryData'Profile OP.ProfilesData

{- |
Internal helper.
Export resource telemetry data and yield statistics.
-}
exportTelemetryData ::
  Logger IO ->
  FullConfig ->
  PerSignal (Maybe Exporter) ->
  ProcessT IO (Tick (Resource, InstrumentationScope, DList TelemetryData)) (Tick (DList Stat))
exportTelemetryData logger fullConfig exporters =
  M.liftTick (mapping (toResourceTelemetryData fullConfig) ~> asParts)
    ~> M.fanoutTick
      [ -- Export logs.
        runIf (C.shouldExportLogs fullConfig) $
          runWith (exporters `forSignal` LOGS) $ \logsExporter ->
            M.liftTick (mapping getResourceLogs ~> asParts ~> mapping D.singleton)
              -- NOTE: This is required to combine different resource telemetry
              --       streams. However, it has the "unfortunate" side-effect of
              --       making it impossible to not batch once per interval.
              ~> M.batchByTick
              ~> M.liftTick (mapping (toExportLogsServiceRequest . D.toList))
              ~> exportResourceLogs logger logsExporter
              ~> M.liftTick (mapping (D.singleton . ExportLogsResultStat))
      , -- Export metrics.
        runIf (C.shouldExportMetrics fullConfig) $
          runWith (exporters `forSignal` METRICS) $ \metricsExporter ->
            M.liftTick (mapping getResourceMetrics ~> asParts ~> mapping D.singleton)
              -- NOTE: See note above.
              ~> M.batchByTick
              ~> M.liftTick (mapping (toExportMetricsServiceRequest . D.toList))
              ~> exportResourceMetrics logger metricsExporter
              ~> M.liftTick (mapping (D.singleton . ExportMetricsResultStat))
      , -- Export spans.
        runIf (C.shouldExportTraces fullConfig) $
          runWith (exporters `forSignal` TRACES) $ \tracesExporter ->
            M.liftTick (mapping getResourceSpans ~> asParts ~> mapping D.singleton)
              -- NOTE: See note above.
              ~> M.batchByTick
              ~> M.liftTick (mapping (toExportTracesServiceRequest . D.toList))
              ~> exportResourceSpans logger tracesExporter
              ~> M.liftTick (mapping (D.singleton . ExportTraceResultStat))
      , -- Export profiles.
        runIf (C.shouldExportProfiles fullConfig) $
          runWith (exporters `forSignal` PROFILES) $ \profilesExporter ->
            M.liftTick (mapping getResourceProfiles ~> asParts ~> mapping toExportProfileServiceRequest)
              ~> exportResourceProfiles logger profilesExporter
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

Repack `TelemetryData` into batched `ResourceTelemetryData`.
-}
toResourceTelemetryData ::
  FullConfig ->
  (Resource, InstrumentationScope, DList TelemetryData) ->
  [ResourceTelemetryData]
toResourceTelemetryData
  fullConfig
  (toResource -> resource, toInstrumentationScope -> instrumentationScope, D.toList -> telemetryData) =
    catMaybes [maybeResourceLogs, maybeResourceMetrics, maybeResourceSpans, maybeProfiles]
   where
    (someLogs, someMetrics, someSpans, someSamples) = partitionTelemetryData telemetryData

    maybeResourceLogs = do
      let logRecords = concatMap (toLogRecords fullConfig) someLogs
      scopeLogs <- toScopeLogs instrumentationScope logRecords
      resourceLogs <- toResourceLogs resource [scopeLogs]
      pure $ ResourceTelemetryData'Log resourceLogs
    maybeResourceMetrics = do
      let metrics = mapMaybe (toMetric fullConfig) someMetrics
      scopeMetrics <- toScopeMetrics instrumentationScope metrics
      resourceMetrics <- toResourceMetrics resource [scopeMetrics]
      pure $ ResourceTelemetryData'Metric resourceMetrics
    maybeResourceSpans = do
      let spans = concatMap (toSpans fullConfig) someSpans
      scopeSpans <- toScopeSpans instrumentationScope spans
      resourceSpans <- toResourceSpans resource [scopeSpans]
      pure $ ResourceTelemetryData'Span resourceSpans
    maybeProfiles = do
      (profiles, dictionary) <- toProfiles fullConfig someSamples
      scopeProfiles <- toScopeProfiles instrumentationScope profiles
      resourceProfiles <- toResourceProfiles resource [scopeProfiles]
      profilesData <- toProfilesData [resourceProfiles] dictionary
      pure $ ResourceTelemetryData'Profile profilesData

{- |
Partition a stream of `TelemetryData` batches to individual batches for each kind of telemetry data.
-}
partitionTelemetryData :: [TelemetryData] -> ([SomeLogs], [SomeMetric], [SomeSpans], [SomeSamples])
partitionTelemetryData = go ([], [], [], [])
 where
  go :: ([SomeLogs], [SomeMetric], [SomeSpans], [SomeSamples]) -> [TelemetryData] -> ([SomeLogs], [SomeMetric], [SomeSpans], [SomeSamples])
  go (logsRev, metricsRev, spansRev, samplesRev) = \case
    [] -> (reverse logsRev, reverse metricsRev, reverse spansRev, reverse samplesRev)
    (TelemetryData'Log log_ : rest) -> go (log_ : logsRev, metricsRev, spansRev, samplesRev) rest
    (TelemetryData'Metric metric : rest) -> go (logsRev, metric : metricsRev, spansRev, samplesRev) rest
    (TelemetryData'Span spans : rest) -> go (logsRev, metricsRev, spans : spansRev, samplesRev) rest
    (TelemetryData'Sample sample : rest) -> go (logsRev, metricsRev, spansRev, sample : samplesRev) rest
