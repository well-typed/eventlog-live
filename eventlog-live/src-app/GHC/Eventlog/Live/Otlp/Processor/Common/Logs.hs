{- |
Module      : GHC.Eventlog.Live.Otlp.Processor.Common.Logs
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otlp.Processor.Common.Logs (
  SomeLogs (..),
  process,
)
where

import Data.DList qualified as D
import Data.Kind (Type)
import Data.Machine (ProcessT, mapping, (~>))
import Data.Proxy (Proxy)
import GHC.Eventlog.Live.Data.LogRecord (IsLogRecord, LogRecord, toLogRecord)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Otlp.Config (FullConfig, KnownLog, logConfig)
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Processor.Common.Core (runIf)

--------------------------------------------------------------------------------
-- Existential wrapper for logs
--------------------------------------------------------------------------------

type SomeLogs :: Type
data SomeLogs
  = forall log. (KnownLog log) => SomeLogs !(Proxy log) [LogRecord]

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
