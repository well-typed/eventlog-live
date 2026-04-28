{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Common.Traces
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Common.Traces (
  asSpan,
  ToSpan (..),
  toExportTracesServiceRequest,
  toResourceSpans,
  toScopeSpans,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Machine (ProcessT, await, construct, yield)
import Data.Maybe (mapMaybe)
import Data.ProtoLens (Message (..))
import GHC.Eventlog.Live.Data.Attribute ((~=))
import GHC.Eventlog.Live.Machine.Analysis.Capability (CapabilityUsageSpan)
import GHC.Eventlog.Live.Machine.Analysis.Capability qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Thread (ThreadStateSpan (..))
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core (ifNonEmpty, messageWith, toMaybeKeyValue)
import GHC.RTS.Events (ThreadId)
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as OL
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OTS
import System.Random (StdGen, initStdGen)
import System.Random.Compat (uniformByteString)

toExportTracesServiceRequest :: [OT.ResourceSpans] -> OTS.ExportTraceServiceRequest
toExportTracesServiceRequest = (defMessage &) . (OTS.resourceSpans .~)

toResourceSpans :: OR.Resource -> [OT.ScopeSpans] -> Maybe OT.ResourceSpans
toResourceSpans resource scopeSpans =
  ifNonEmpty scopeSpans $
    messageWith [OL.resource .~ resource, OT.scopeSpans .~ scopeSpans]

toScopeSpans :: OC.InstrumentationScope -> [OT.Span] -> Maybe OT.ScopeSpans
toScopeSpans instrumentationScope spans =
  ifNonEmpty spans $
    messageWith [OT.scope .~ instrumentationScope, OT.spans .~ spans]

--------------------------------------------------------------------------------
-- Interpret spans

-- | The `asSpan` machine processes values @v@ into OpenTelemetry spans `OT.Span`.
asSpan :: (ToSpan v, MonadIO m, Hashable (Key v)) => FullConfig -> ProcessT m v OT.Span
asSpan fullConfig = construct $ go (mempty, Nothing)
 where
  -- go :: (HashMap (Key v) ByteString, Maybe StdGen) -> PlanT (Is v) OT.Span m Void
  go (traceIds, maybeGen) = do
    -- Ensure the StdGen is initialised
    gen0 <- maybe (liftIO initStdGen) pure maybeGen
    -- Receive the next value
    i <- await
    -- Ensure the next value has a trace ID
    let ensureTraceId :: Maybe ByteString -> ((ByteString, StdGen), Maybe ByteString)
        ensureTraceId = wrap . maybe (uniformByteString 16 gen0) (,gen0)
         where
          wrap out@(traceId, _gen) = (out, Just traceId)
    let ((traceId, gen1), traceIds') = HM.alterF ensureTraceId (toKey i) traceIds
    -- Ensure the next value has a span ID
    let (spanId, gen2) = uniformByteString 8 gen1
    -- Yield a span
    yield $ toSpan fullConfig i traceId spanId
    -- Continue
    go (traceIds', Just gen2)

class ToSpan v where
  -- | The `Key` type is used to index a `HashMap` in the default definition of `asSpan`.
  type Key v

  -- | The `toKey` function extracts a `Key` from the input value.
  toKey ::
    -- | The input value.
    v ->
    Key v

  toSpan ::
    -- | The configuration.
    FullConfig ->
    -- | The input value.
    v ->
    -- | The trace ID.
    ByteString ->
    -- | The span ID.
    ByteString ->
    OT.Span

--------------------------------------------------------------------------------
-- Interpret capability usage spans

instance ToSpan CapabilityUsageSpan where
  type Key CapabilityUsageSpan = Int

  toKey :: CapabilityUsageSpan -> Int
  toKey = (.cap)

  toSpan :: FullConfig -> CapabilityUsageSpan -> ByteString -> ByteString -> OT.Span
  toSpan fullConfig i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ C.processorName (.traces) (.capabilityUsage) fullConfig <> " " <> M.showCapabilityUserCategory user
      , OT.kind .~ OT.Span'SPAN_KIND_INTERNAL
      , OT.startTimeUnixNano .~ i.startTimeUnixNano
      , OT.endTimeUnixNano .~ i.endTimeUnixNano
      , OT.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "capability" ~= i.cap
            , "user" ~= user
            ]
      , OT.status
          .~ messageWith
            [ OT.code .~ OT.Status'STATUS_CODE_OK
            ]
      ]
   where
    user = M.capabilityUser i

--------------------------------------------------------------------------------
-- Interpret thread state spans

instance ToSpan ThreadStateSpan where
  type Key ThreadStateSpan = ThreadId

  toKey :: ThreadStateSpan -> ThreadId
  toKey = (.thread)

  toSpan :: FullConfig -> ThreadStateSpan -> ByteString -> ByteString -> OT.Span
  toSpan fullConfig i traceId spanId =
    messageWith
      [ OT.traceId .~ traceId
      , OT.spanId .~ spanId
      , OT.name .~ C.processorName (.traces) (.threadState) fullConfig <> " " <> M.showThreadStateCategory i.threadState
      , OT.kind .~ OT.Span'SPAN_KIND_INTERNAL
      , OT.startTimeUnixNano .~ i.startTimeUnixNano
      , OT.endTimeUnixNano .~ i.endTimeUnixNano
      , OT.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "capability" ~= M.threadStateCap i.threadState
            , "thread" ~= show i.thread
            , "status" ~= (show <$> M.threadStateStatus i.threadState)
            ]
      , OT.status
          .~ messageWith
            [ OT.code .~ OT.Status'STATUS_CODE_OK
            ]
      ]
