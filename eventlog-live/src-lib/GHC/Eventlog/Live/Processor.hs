module GHC.Eventlog.Live.Processor (
  Resource (..),
  TelemetryData (..),
  InstrumentationScope (..),
  processEventlogTelemetry,
  processInternalTelemetry,
) where

import Control.Concurrent.STM (TChan)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (Process, ProcessT, asParts, mapping, (~>))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Version (Version)
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.Eventlog.Live.Data.Logs (LogRecord (..), SomeLogs)
import GHC.Eventlog.Live.Data.Metric (SomeMetric)
import GHC.Eventlog.Live.Data.Sample (SomeSamples)
import GHC.Eventlog.Live.Data.Span (SomeSpans)
import GHC.Eventlog.Live.Logger (Logger, MyTelemetryData (..), chanSource)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime)
import GHC.Eventlog.Live.Processor.Core.Logs qualified as CL
import GHC.Eventlog.Live.Processor.Heap (processHeapEvents)
import GHC.Eventlog.Live.Processor.Logs (processLogEvents)
import GHC.Eventlog.Live.Processor.Profiles (processProfileEvents)
import GHC.Eventlog.Live.Processor.Threads (processThreadEvents)
import GHC.RTS.Events (Event (..), HeapProfBreakdown)
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP

newtype Resource
  = Resource {attrs :: Attrs}

data InstrumentationScope
  = InstrumentationScope {name :: Text, version :: Version}

data TelemetryData
  = TelemetryData'Log SomeLogs
  | TelemetryData'Metric SomeMetric
  | TelemetryData'Span SomeSpans
  | TelemetryData'Sample SomeSamples

-- Create machine that processes eventlog data into telemetry data
processEventlogTelemetry ::
  Logger IO ->
  FullConfig ->
  Maybe HeapProfBreakdown ->
  DB.Table CC.CostCentreId CC.CostCentre ->
  DB.Table IP.InfoProvId IP.InfoProv ->
  ProcessT IO (Tick (WithStartTime Event)) (Tick (DList TelemetryData))
processEventlogTelemetry logger fullConfig maybeHeapProfBreakdown ccdb ipedb =
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

{- |
Create the machine that processes internal telemetry data

NOTE: This process only takes a stream of inputs to use their tick.
-}
processInternalTelemetry ::
  FullConfig ->
  TChan MyTelemetryData ->
  ProcessT IO (Tick x) (Tick (DList TelemetryData))
processInternalTelemetry fullConfig myTelemetryDataChan =
  M.mergeWithTickCC (chanSource myTelemetryDataChan)
    ~> M.fanoutTick
      [ CL.process (Proxy @C.InternalLogMessageLog) processInternalLogRecords fullConfig
          ~> M.liftTick (mapping (D.singleton . TelemetryData'Log))
      ]
 where
  processInternalLogRecords :: Process MyTelemetryData LogRecord
  processInternalLogRecords = mapping getInternalLogRecord ~> asParts

  getInternalLogRecord :: MyTelemetryData -> Maybe LogRecord
  getInternalLogRecord = \case
    MyTelemetryData'LogRecord{..} -> Just logRecord
