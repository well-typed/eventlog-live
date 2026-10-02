{- |
Module      : GHC.Eventlog.Live.Processor.Core.Traces
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Processor.Core.Traces (
  process,
)
where

import Control.Monad.IO.Class (MonadIO)
import Data.DList qualified as D
import Data.Machine (ProcessT, mapping, (~>))
import Data.Proxy (Proxy)
import GHC.Eventlog.Live.Config (FullConfig)
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Processor.Core (runIf)
import GHC.Eventlog.Live.Types.Traces (IsSpan, KnownTrace, SomeSpans (..), asSpan, traceConfig)

--------------------------------------------------------------------------------
-- Existential wrapper for spans
--------------------------------------------------------------------------------

process ::
  (MonadIO m, IsSpan s k) =>
  (KnownTrace trace) =>
  Proxy trace ->
  FullConfig ->
  ProcessT m (Tick s) (Tick SomeSpans)
process (trace :: Proxy trace) fullConfig =
  runIf (C.processorEnabled (.traces) (traceConfig @trace) fullConfig) $
    M.liftTick asSpan
      ~> M.liftTick (mapping D.singleton)
      ~> M.batchByTicks (C.processorExportBatches (.traces) (traceConfig @trace) fullConfig)
      ~> M.liftTick (mapping $ SomeSpans trace . D.toList)
