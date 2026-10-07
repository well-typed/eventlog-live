{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Processor (
  Resource (..),
  Telemetry (..),
  ExportRequest (..),
  InstrumentationScope (..),
  processEventlogTelemetry,
  processInternalTelemetry,
) where

import Data.Aeson.Types (Encoding, KeyValue (..), ToJSON (..), Value (..), pairs)
import Data.Coerce (coerce)
import Data.DList qualified as D
import Data.Machine (ProcessT, asParts, mapping, (~>))
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Version (Version)
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Logger (ExportResult (..), InternalMetric (..), InternalTelemetry (..), Logger)
import GHC.Eventlog.Live.Machine.Core (Tick, deltaToCumulative)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime)
import GHC.Eventlog.Live.Processor.Core.Logs qualified as CL
import GHC.Eventlog.Live.Processor.Core.Metrics (MetricProcessors (..), select)
import GHC.Eventlog.Live.Processor.Core.Metrics qualified as CM
import GHC.Eventlog.Live.Processor.Heap (processHeapEvents)
import GHC.Eventlog.Live.Processor.Logs (processLogEvents)
import GHC.Eventlog.Live.Processor.Profiles (processProfileEvents)
import GHC.Eventlog.Live.Processor.Threads (processThreadEvents)
import GHC.Eventlog.Live.Types.Attribute (Attrs, IsAttrValue (toAttrValue))
import GHC.Eventlog.Live.Types.Logs (LogRecord (..), SomeLogs)
import GHC.Eventlog.Live.Types.Metrics (Metric, SomeMetrics)
import GHC.Eventlog.Live.Types.Profiles (SomeSamples)
import GHC.Eventlog.Live.Types.Traces (SomeSpans)
import GHC.RTS.Events (Event (..), HeapProfBreakdown)
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP

newtype Resource
  = Resource {attrs :: Attrs}
  deriving newtype (ToJSON)

data InstrumentationScope
  = InstrumentationScope {name :: Text, version :: Version}

instance ToJSON InstrumentationScope where
  toJSON :: InstrumentationScope -> Value
  toJSON = Object . instrumentationScopeToKV

  toEncoding :: InstrumentationScope -> Encoding
  toEncoding = pairs . instrumentationScopeToKV

instrumentationScopeToKV :: (KeyValue e kv, Monoid kv) => InstrumentationScope -> kv
instrumentationScopeToKV s =
  mconcat
    [ "name" .= s.name
    , "version" .= toAttrValue s.version
    ]
{-# INLINE instrumentationScopeToKV #-}

data Telemetry
  = Telemetry'Logs SomeLogs
  | Telemetry'Metrics SomeMetrics
  | Telemetry'Spans SomeSpans
  | Telemetry'Samples SomeSamples

instance ToJSON Telemetry where
  toJSON :: Telemetry -> Value
  toJSON = \case
    Telemetry'Logs x -> toJSON x
    Telemetry'Metrics x -> toJSON x
    Telemetry'Spans x -> toJSON x
    Telemetry'Samples x -> toJSON x

  toEncoding :: Telemetry -> Encoding
  toEncoding = \case
    Telemetry'Logs x -> toEncoding x
    Telemetry'Metrics x -> toEncoding x
    Telemetry'Spans x -> toEncoding x
    Telemetry'Samples x -> toEncoding x

data ExportRequest = ExportRequest
  { resource :: !Resource
  , scope :: !InstrumentationScope
  , telemetry :: ![Telemetry]
  }

exportRequestToKV :: (KeyValue e kv, Monoid kv) => ExportRequest -> kv
exportRequestToKV r =
  mconcat
    [ "resource" .= r.resource
    , "scope" .= r.scope
    , "telemetry" .= r.telemetry
    ]
{-# INLINE exportRequestToKV #-}

instance ToJSON ExportRequest where
  toJSON :: ExportRequest -> Value
  toJSON = Object . exportRequestToKV

  toEncoding :: ExportRequest -> Encoding
  toEncoding = pairs . exportRequestToKV

-- Create machine that processes eventlog data into telemetry data
processEventlogTelemetry ::
  Logger IO ->
  FullConfig ->
  Resource ->
  InstrumentationScope ->
  Maybe HeapProfBreakdown ->
  DB.Table CC.CostCentreId CC.CostCentre ->
  DB.Table IP.InfoProvId IP.InfoProv ->
  ProcessT IO (Tick (WithStartTime Event)) (Tick ExportRequest)
processEventlogTelemetry logger fullConfig resource scope maybeHeapProfBreakdown ccdb ipedb =
  M.fanoutTick
    [ -- Process the heap events.
      processHeapEvents logger (Just ipedb) maybeHeapProfBreakdown fullConfig
        ~> mapping (fmap (fmap Telemetry'Metrics))
    , -- Process the log events.
      processLogEvents fullConfig
        ~> mapping (fmap (fmap Telemetry'Logs))
    , -- Process the thread events.
      processThreadEvents logger fullConfig
        ~> mapping (fmap (fmap (either Telemetry'Metrics Telemetry'Spans)))
    , -- Process the profile events.
      processProfileEvents logger ccdb ipedb fullConfig
        ~> mapping (fmap (fmap Telemetry'Samples))
    ]
    ~> M.liftTick (mapping $ ExportRequest resource scope . D.toList)

{- |
Create the machine that processes internal telemetry data

NOTE: This process only takes a stream of inputs to use their tick.
-}
processInternalTelemetry ::
  FullConfig ->
  Resource ->
  InstrumentationScope ->
  ProcessT IO (Tick InternalTelemetry) (Tick ExportRequest)
processInternalTelemetry fullConfig resource scope =
  M.fanoutTick
    [ -- Process internal log messages.
      CL.process (Proxy @"internalLogMessage") processLogRecords fullConfig
        ~> M.liftTick (mapping $ D.singleton . Telemetry'Logs)
    , -- Process internal metrics.
      M.fanoutTick
        [ -- Process internal event counts.
          CM.process (Proxy @"internalEventCount") processEventCount fullConfig
            ~> M.liftTick (mapping D.singleton)
        , -- Process internal exported log counts.
          CM.processAllWith fullConfig processExportLogs $
            select (Proxy @"internalExportedLogs") (.exported)
              :&: select (Proxy @"internalRejectedLogs") (.rejected)
              :&: End
        , -- Process internal exported metric counts.
          CM.processAllWith fullConfig processExportMetrics $
            select (Proxy @"internalExportedMetrics") (.exported)
              :&: select (Proxy @"internalRejectedMetrics") (.rejected)
              :&: End
        , -- Process internal exported sample counts.
          CM.processAllWith fullConfig processExportSamples $
            select (Proxy @"internalExportedSamples") (.exported)
              :&: select (Proxy @"internalRejectedSamples") (.rejected)
              :&: End
        , -- Process internal exported span counts.
          CM.processAllWith fullConfig processExportSpans $
            select (Proxy @"internalExportedSpans") (.exported)
              :&: select (Proxy @"internalRejectedSpans") (.rejected)
              :&: End
        ]
        ~> M.liftTick (mapping (fmap Telemetry'Metrics))
    ]
    ~> M.liftTick (mapping $ ExportRequest resource scope . D.toList)
 where
  processLogRecords =
    mapping getLogRecord ~> asParts
   where
    getLogRecord :: InternalTelemetry -> Maybe LogRecord
    getLogRecord = \case InternalTelemetry'LogRecord l -> Just l; _ -> Nothing

  processEventCount =
    mapping (coerce @_ @(Maybe (Metric (Sum Word))) . getEventCount)
      ~> asParts
      ~> deltaToCumulative @_ @Metric @(Sum Word)
      ~> mapping (coerce @_ @(Metric Word))
   where
    getEventCount :: InternalTelemetry -> Maybe (Metric Word)
    getEventCount = \case InternalTelemetry'Metric EventCount m -> Just m; _ -> Nothing

  processExportLogs =
    mapping getExportLogs ~> asParts ~> deltaToCumulative
   where
    getExportLogs :: InternalTelemetry -> Maybe (Metric ExportResult)
    getExportLogs = \case InternalTelemetry'Metric ExportLogs m -> Just m; _ -> Nothing

  processExportMetrics =
    mapping getExportMetrics ~> asParts ~> deltaToCumulative
   where
    getExportMetrics :: InternalTelemetry -> Maybe (Metric ExportResult)
    getExportMetrics = \case InternalTelemetry'Metric ExportMetrics m -> Just m; _ -> Nothing

  processExportSpans =
    mapping getExportSpans ~> asParts ~> deltaToCumulative
   where
    getExportSpans :: InternalTelemetry -> Maybe (Metric ExportResult)
    getExportSpans = \case InternalTelemetry'Metric ExportSpans m -> Just m; _ -> Nothing

  processExportSamples =
    mapping getExportSamples ~> asParts ~> deltaToCumulative
   where
    getExportSamples :: InternalTelemetry -> Maybe (Metric ExportResult)
    getExportSamples = \case InternalTelemetry'Metric ExportSamples m -> Just m; _ -> Nothing
