{- |
Module      : GHC.Eventlog.Live.Processor.Core.Logs
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Processor.Core.Logs (
  process,
)
where

import Data.DList qualified as D
import Data.Machine (ProcessT, mapping, (~>))
import Data.Proxy (Proxy)
import GHC.Eventlog.Live.Config (FullConfig)
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Processor.Core (runIf)
import GHC.Eventlog.Live.Types.Logs (IsLogRecord, KnownLog, SomeLogs (..), logConfig, toLogRecord)

--------------------------------------------------------------------------------
-- Generic processor for logs
--------------------------------------------------------------------------------

process ::
  (Monad m, IsLogRecord l) =>
  (KnownLog log) =>
  Proxy log ->
  ProcessT m i l ->
  FullConfig ->
  ProcessT m (Tick i) (Tick SomeLogs)
process (log_ :: Proxy log) processor fullConfig =
  runIf (C.processorEnabled (.logs) (logConfig @log) fullConfig) $
    M.liftTick (processor ~> mapping (D.singleton . toLogRecord))
      ~> M.batchByTicks (C.processorExportBatches (.logs) (logConfig @log) fullConfig)
      ~> M.liftTick (mapping $ SomeLogs log_ . D.toList)
