{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Processor (
  Resource (..),
  TelemetryData (..),
  ExportRequest (..),
  InstrumentationScope (..),
  processEventlogTelemetry,
  processInternalTelemetry,
) where

import Control.Concurrent.STM (TChan)
import Data.Aeson.Types (Encoding, KeyValue (..), ToJSON (..), Value (..), pairs)
import Data.DList qualified as D
import Data.Machine (Process, ProcessT, asParts, mapping, (~>))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Version (Version)
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Logger (Logger, MyTelemetryData (..), chanSource)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime)
import GHC.Eventlog.Live.Processor.Core.Logs qualified as CL
import GHC.Eventlog.Live.Processor.Heap (processHeapEvents)
import GHC.Eventlog.Live.Processor.Logs (processLogEvents)
import GHC.Eventlog.Live.Processor.Profiles (processProfileEvents)
import GHC.Eventlog.Live.Processor.Threads (processThreadEvents)
import GHC.Eventlog.Live.Types.Attribute (Attrs, IsAttrValue (toAttrValue))
import GHC.Eventlog.Live.Types.Logs (LogRecord (..), SomeLogs)
import GHC.Eventlog.Live.Types.Metrics (SomeMetrics)
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

data TelemetryData
  = TelemetryData'Log SomeLogs
  | TelemetryData'Metric SomeMetrics
  | TelemetryData'Span SomeSpans
  | TelemetryData'Sample SomeSamples

instance ToJSON TelemetryData where
  toJSON :: TelemetryData -> Value
  toJSON = \case
    TelemetryData'Log x -> toJSON x
    TelemetryData'Metric x -> toJSON x
    TelemetryData'Span x -> toJSON x
    TelemetryData'Sample x -> toJSON x

  toEncoding :: TelemetryData -> Encoding
  toEncoding = \case
    TelemetryData'Log x -> toEncoding x
    TelemetryData'Metric x -> toEncoding x
    TelemetryData'Span x -> toEncoding x
    TelemetryData'Sample x -> toEncoding x

data ExportRequest = ExportRequest
  { resource :: !Resource
  , scope :: !InstrumentationScope
  , telemetry :: ![TelemetryData]
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
        ~> mapping (fmap (fmap TelemetryData'Metric))
    , -- Process the log events.
      processLogEvents fullConfig
        ~> mapping (fmap (fmap TelemetryData'Log))
    , -- Process the thread events.
      processThreadEvents logger fullConfig
        ~> mapping (fmap (fmap (either TelemetryData'Metric TelemetryData'Span)))
    , -- Process the profile events.
      processProfileEvents logger ccdb ipedb fullConfig
        ~> mapping (fmap (fmap TelemetryData'Sample))
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
  TChan MyTelemetryData ->
  ProcessT IO (Tick x) (Tick ExportRequest)
processInternalTelemetry fullConfig resource scope myTelemetryDataChan =
  M.mergeWithTickCC (chanSource myTelemetryDataChan)
    ~> M.fanoutTick
      [ CL.process (Proxy @"internalLogMessage") processInternalLogRecords fullConfig
          ~> M.liftTick (mapping (D.singleton . TelemetryData'Log))
      ]
    ~> M.liftTick (mapping $ ExportRequest resource scope . D.toList)
 where
  processInternalLogRecords :: Process MyTelemetryData LogRecord
  processInternalLogRecords = mapping getInternalLogRecord ~> asParts

  getInternalLogRecord :: MyTelemetryData -> Maybe LogRecord
  getInternalLogRecord = \case
    MyTelemetryData'LogRecord{..} -> Just logRecord
