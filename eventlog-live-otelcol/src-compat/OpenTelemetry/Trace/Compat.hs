module OpenTelemetry.Trace.Compat () where

import GHC.Eventlog.Live.Machines
import OpenTelemetry.Trace qualified as OT
import OpenTelemetry.Trace.Core (Span)
import OpenTelemetry.Trace.Core qualified as OTC

class ToSpan m s where
  toSpan :: (Monad m) => s -> m Span
