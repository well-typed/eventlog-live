{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Logs
Description : Log Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Logs (
  processLogEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (Process, ProcessT, mapping, (~>))
import GHC.Eventlog.Live.Machine.Analysis.Log qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core (runIf)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Logs (ToLogRecord (..))
import GHC.RTS.Events (Event (..))
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL

--------------------------------------------------------------------------------
-- processLogEvents
--------------------------------------------------------------------------------

processLogEvents ::
  (MonadIO m) =>
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processLogEvents fullConfig =
  M.fanoutTick
    [ processThreadLabel fullConfig
    , processUserMarker fullConfig
    , processUserMessage fullConfig
    ]

--------------------------------------------------------------------------------
-- UserMessage

processUserMessage :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processUserMessage fullConfig =
  runIf (C.processorEnabled (.logs) (.userMessage) fullConfig) $
    M.liftTick M.processUserMessageData
      ~> M.liftTick (mapping (D.singleton . toLogRecord))
      ~> M.batchByTicks (C.processorExportBatches (.logs) (.userMessage) fullConfig)

--------------------------------------------------------------------------------
-- UserMarker

processUserMarker :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processUserMarker fullConfig =
  runIf (C.processorEnabled (.logs) (.userMarker) fullConfig) $
    M.liftTick M.processUserMarkerData
      ~> M.liftTick (mapping (D.singleton . toLogRecord))
      ~> M.batchByTicks (C.processorExportBatches (.logs) (.userMarker) fullConfig)

--------------------------------------------------------------------------------
-- ThreadLabel

processThreadLabel :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OL.LogRecord))
processThreadLabel fullConfig =
  runIf (C.processorEnabled (.logs) (.threadLabel) fullConfig) $
    M.liftTick M.processThreadLabelData
      ~> M.liftTick (mapping (D.singleton . toLogRecord))
      ~> M.batchByTicks (C.processorExportBatches (.logs) (.threadLabel) fullConfig)
