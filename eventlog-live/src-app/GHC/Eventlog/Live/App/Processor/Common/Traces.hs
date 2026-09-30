{- |
Module      : GHC.Eventlog.Live.App.Processor.Common.Traces
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.App.Processor.Common.Traces (
  SomeSpans (..),
  process,
)
where

import Control.Monad.IO.Class (MonadIO)
import Data.DList qualified as D
import Data.Kind (Type)
import Data.Machine (ProcessT, mapping, (~>))
import Data.Proxy (Proxy)
import GHC.Eventlog.Live.App.Config (FullConfig, KnownTrace (..), traceConfig)
import GHC.Eventlog.Live.App.Config qualified as C
import GHC.Eventlog.Live.App.Processor.Common.Core (runIf)
import GHC.Eventlog.Live.Data.Span (IsSpan, Span (..), asSpan)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M

--------------------------------------------------------------------------------
-- Existential wrapper for spans
--------------------------------------------------------------------------------

type SomeSpans :: Type
data SomeSpans
  = forall trace. (KnownTrace trace) => SomeSpans !(Proxy trace) [Span]

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
