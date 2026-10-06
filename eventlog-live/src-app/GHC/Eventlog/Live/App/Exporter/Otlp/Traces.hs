{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.App.Exporter.Otlp.Traces (
  -- * Export
  exportResourceSpans,

  -- * Conversion to OTLP
  toExportTracesServiceRequest,
  toResourceSpans,
  toScopeSpans,
  toSpans,
) where

import Control.Exception (SomeException (..), catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, traversing)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import Data.Vector qualified as V
import GHC.Eventlog.Live.App.Exporter.Otlp.Core (CanExportToConsole, CanExportToOltpViaHttpProtobuf (..), ExportError (..), Exporter (..), export, ifNonEmpty, messageWith, toMaybeKeyValues)
import GHC.Eventlog.Live.Config (FullConfig)
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (ExportResult (..), InternalMetric (..), Logger, logException, logMetric)
import GHC.Eventlog.Live.Types.Traces (SomeSpans (..), Span (..), traceConfig)
import Lens.Family2 ((.~), (^.))
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as OTS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OT

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter for Traces

exportResourceSpans ::
  Logger IO ->
  Exporter ->
  ProcessT IO OTS.ExportTraceServiceRequest ()
exportResourceSpans logger exporter =
  traversing sendResourceSpans
 where
  sendResourceSpans :: OTS.ExportTraceServiceRequest -> IO ()
  sendResourceSpans exportTraceServiceRequest =
    doExport `catch` handleSomeException
   where
    doExport :: IO ()
    doExport = do
      resp <- export @OTS.TraceService @"export" logger exporter exportTraceServiceRequest
      let !exported = countSpansInExportTraceServiceRequest exportTraceServiceRequest
      let !rejected = resp ^. OTS.partialSuccess . OTS.rejectedSpans
      liftIO (logMetric logger ExportSpans ExportResult{..})
      unless (rejected == 0) $
        liftIO (logException logger $ ExportError $ resp ^. OTS.partialSuccess . OTS.errorMessage)

    handleSomeException :: SomeException -> IO ()
    handleSomeException = logException logger

--------------------------------------------------------------------------------
-- CanExportToConsole

instance CanExportToConsole OTS.TraceService "export"

--------------------------------------------------------------------------------
-- CanExportToOltpViaGrpc

type instance G.RequestMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- CanExportToOltpViaHttpProtobuf

instance CanExportToOltpViaHttpProtobuf OTS.TraceService "export" where
  apiPath :: String
  apiPath = "/v1/traces"

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

{- |
Internal helper.
Count the number of `OT.Span` values in an `OTS.ExportTraceServiceRequest`.
-}
{-# SPECIALIZE countSpansInExportTraceServiceRequest :: OTS.ExportTraceServiceRequest -> Int64 #-}
{-# SPECIALIZE countSpansInExportTraceServiceRequest :: OTS.ExportTraceServiceRequest -> Word #-}
countSpansInExportTraceServiceRequest :: (Integral i) => OTS.ExportTraceServiceRequest -> i
countSpansInExportTraceServiceRequest exportTraceServiceRequest =
  getSum $ foldMap (Sum . countSpansInResourceSpans) (exportTraceServiceRequest ^. OTS.vec'resourceSpans)

{- |
Internal helper.
Count the number of `OT.Span` values in an `OT.ResourceSpans`.
-}
{-# SPECIALIZE countSpansInResourceSpans :: OT.ResourceSpans -> Int64 #-}
{-# SPECIALIZE countSpansInResourceSpans :: OT.ResourceSpans -> Word #-}
countSpansInResourceSpans :: (Integral i) => OT.ResourceSpans -> i
countSpansInResourceSpans resourceSpans =
  getSum $ foldMap (Sum . countSpansInScopeSpans) (resourceSpans ^. OT.vec'scopeSpans)

{- |
Internal helper.
Count the number of `OT.Span` values in an `OT.ScopeSpans`.
-}
{-# SPECIALIZE countSpansInScopeSpans :: OT.ScopeSpans -> Int64 #-}
{-# SPECIALIZE countSpansInScopeSpans :: OT.ScopeSpans -> Word #-}
countSpansInScopeSpans :: (Integral i) => OT.ScopeSpans -> i
countSpansInScopeSpans scopeSpans =
  fromIntegral $
    V.length (scopeSpans ^. OT.vec'spans)

--------------------------------------------------------------------------------
-- Conversion to OTLP
--------------------------------------------------------------------------------

toExportTracesServiceRequest :: [OT.ResourceSpans] -> OTS.ExportTraceServiceRequest
toExportTracesServiceRequest resourceSpans =
  messageWith [OTS.resourceSpans .~ resourceSpans]
{-# INLINE toExportTracesServiceRequest #-}

toResourceSpans :: OR.Resource -> [OT.ScopeSpans] -> Maybe OT.ResourceSpans
toResourceSpans resource scopeSpans =
  ifNonEmpty scopeSpans $
    messageWith [OT.resource .~ resource, OT.scopeSpans .~ scopeSpans]
{-# INLINE toResourceSpans #-}

toScopeSpans :: OC.InstrumentationScope -> [OT.Span] -> Maybe OT.ScopeSpans
toScopeSpans instrumentationScope spans =
  ifNonEmpty spans $
    messageWith [OT.scope .~ instrumentationScope, OT.spans .~ spans]
{-# INLINE toScopeSpans #-}

toSpans :: FullConfig -> SomeSpans -> [OT.Span]
toSpans fullConfig (SomeSpans (_trace :: Proxy trace) spans) =
  map toSpan spans
 where
  toSpan :: Span -> OT.Span
  toSpan s =
    messageWith
      [ OT.name .~ C.processorName (.traces) (traceConfig $ Proxy @trace) fullConfig
      , OT.traceId .~ s.traceId
      , OT.spanId .~ s.spanId
      , OT.startTimeUnixNano .~ s.startTimeUnixNano
      , OT.endTimeUnixNano .~ s.endTimeUnixNano
      , OT.attributes .~ toMaybeKeyValues s.attrs
      ]
