{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Otelcol.Exporter (
  -- * Metrics
  ExportMetricsResult (..),
  RejectedMetricsError (..),
  exportResourceMetrics,

  -- * Traces
  ExportTraceResult (..),
  RejectedSpansError (..),
  exportResourceSpans,

  -- * Helpers

  -- ** Counting Metrics
  countDataPointsInExportMetricsServiceRequest,
  countDataPointsInResourceMetrics,
  countDataPointsInScopeMetrics,
  countDataPointsInMetric,

  -- ** Counting Spans
  countSpansInExportTraceServiceRequest,
  countSpansInResourceSpans,
  countSpansInScopeSpans,
) where

import Control.Exception (Exception (..), SomeException (..), catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, await, repeatedly, yield)
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import Data.Vector qualified as V
import Lens.Family2 ((^.))
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Network.GRPC.Common.Protobuf qualified as G
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as OTS
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OT
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporters
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Result for Metrics

data ExportMetricsResult
  = ExportMetricsResult
  { acceptedDataPoints :: !Int64
  , rejectedDataPoints :: !Int64
  , maybeSomeException :: Maybe SomeException
  }
  deriving (Show)

pattern ExportMetricsSuccess :: Int64 -> ExportMetricsResult
pattern ExportMetricsSuccess acceptedDataPoints =
  ExportMetricsResult acceptedDataPoints 0 Nothing

pattern ExportMetricsError :: Int64 -> Int64 -> SomeException -> ExportMetricsResult
pattern ExportMetricsError acceptedDataPoints rejectedDataPoints someException =
  ExportMetricsResult acceptedDataPoints rejectedDataPoints (Just someException)

data RejectedMetricsError
  = RejectedMetricsError
  { rejectedDataPoints :: !Int64
  , errorMessage :: !Text
  }
  deriving (Show)

instance Exception RejectedMetricsError where
  displayException :: RejectedMetricsError -> String
  displayException RejectedMetricsError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedDataPoints errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Metrics

exportResourceMetrics ::
  G.Connection ->
  ProcessT IO OMS.ExportMetricsServiceRequest ExportMetricsResult
exportResourceMetrics conn =
  repeatedly $
    await >>= \exportMetricsServiceRequest ->
      liftIO (sendResourceMetrics exportMetricsServiceRequest)
        >>= yield
 where
  sendResourceMetrics :: OMS.ExportMetricsServiceRequest -> IO ExportMetricsResult
  sendResourceMetrics exportMetricsServiceRequest =
    doGrpc `catch` handleGrpcError
   where
    !totalDataPoints = countDataPointsInExportMetricsServiceRequest exportMetricsServiceRequest

    doGrpc :: IO ExportMetricsResult
    doGrpc = do
      G.nonStreaming conn (G.rpc @(Protobuf OMS.MetricsService "export")) (G.Proto exportMetricsServiceRequest) >>= \case
        G.Proto resp
          | resp ^. OMS.partialSuccess . OMS.rejectedDataPoints == 0 -> do
              pure $ ExportMetricsSuccess totalDataPoints
          | otherwise -> do
              let !rejectedDataPoints = resp ^. OMS.partialSuccess . OMS.rejectedDataPoints
              let !acceptedDataPoints = totalDataPoints - rejectedDataPoints
              let !rejectedMetricsError = RejectedMetricsError{errorMessage = resp ^. OMS.partialSuccess . OMS.errorMessage, ..}
              pure $ ExportMetricsError acceptedDataPoints rejectedDataPoints (SomeException rejectedMetricsError)

    handleGrpcError :: G.GrpcError -> IO ExportMetricsResult
    handleGrpcError grpcError = pure $ ExportMetricsError 0 totalDataPoints (SomeException grpcError)

type instance G.RequestMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Result for Traces

data ExportTraceResult
  = ExportTraceResult
  { acceptedSpans :: !Int64
  , rejectedSpans :: !Int64
  , maybeSomeException :: Maybe SomeException
  }
  deriving (Show)

pattern ExportTraceSuccess :: Int64 -> ExportTraceResult
pattern ExportTraceSuccess acceptedSpans =
  ExportTraceResult acceptedSpans 0 Nothing

pattern ExportTraceError :: Int64 -> Int64 -> SomeException -> ExportTraceResult
pattern ExportTraceError acceptedSpans rejectedSpans someException =
  ExportTraceResult acceptedSpans rejectedSpans (Just someException)

data RejectedSpansError
  = RejectedSpansError
  { rejectedSpans :: !Int64
  , errorMessage :: !Text
  }
  deriving (Show)

instance Exception RejectedSpansError where
  displayException :: RejectedSpansError -> String
  displayException RejectedSpansError{..} =
    printf "Error: OpenTelemetry Collector rejectedSpans %d data points with message: %s" rejectedSpans errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Traces

exportResourceSpans ::
  G.Connection ->
  ProcessT IO OTS.ExportTraceServiceRequest ExportTraceResult
exportResourceSpans conn =
  repeatedly $
    await >>= \exportTraceServiceRequest ->
      liftIO (sendResourceSpans exportTraceServiceRequest)
        >>= yield
 where
  sendResourceSpans :: OTS.ExportTraceServiceRequest -> IO ExportTraceResult
  sendResourceSpans exportTraceServiceRequest =
    doGrpc `catch` handleGrpcError
   where
    !totalSpans = countSpansInExportTraceServiceRequest exportTraceServiceRequest

    doGrpc :: IO ExportTraceResult
    doGrpc = do
      G.nonStreaming conn (G.rpc @(Protobuf OTS.TraceService "export")) (G.Proto exportTraceServiceRequest) >>= \case
        G.Proto resp
          | resp ^. OTS.partialSuccess . OTS.rejectedSpans == 0 ->
              pure $ ExportTraceSuccess totalSpans
          | otherwise -> do
              let !rejectedSpans = resp ^. OTS.partialSuccess . OTS.rejectedSpans
              let !acceptedSpans = totalSpans - rejectedSpans
              let !rejectedMetricsError = RejectedSpansError{errorMessage = resp ^. OTS.partialSuccess . OTS.errorMessage, ..}
              pure $ ExportTraceError acceptedSpans rejectedSpans (SomeException rejectedMetricsError)

    handleGrpcError :: G.GrpcError -> IO ExportTraceResult
    handleGrpcError grpcError = pure $ ExportTraceError 0 totalSpans (SomeException grpcError)

type instance G.RequestMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OMS.ExportMetricsServiceRequest`.
-}
{-# SPECIALIZE countDataPointsInExportMetricsServiceRequest :: OMS.ExportMetricsServiceRequest -> Int64 #-}
{-# SPECIALIZE countDataPointsInExportMetricsServiceRequest :: OMS.ExportMetricsServiceRequest -> Word #-}
countDataPointsInExportMetricsServiceRequest :: (Integral i) => OMS.ExportMetricsServiceRequest -> i
countDataPointsInExportMetricsServiceRequest exportMetricsServiceRequest =
  getSum $ foldMap (Sum . countDataPointsInResourceMetrics) (exportMetricsServiceRequest ^. OMS.vec'resourceMetrics)

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OM.ResourceMetrics`.
-}
{-# SPECIALIZE countDataPointsInResourceMetrics :: OM.ResourceMetrics -> Int64 #-}
{-# SPECIALIZE countDataPointsInResourceMetrics :: OM.ResourceMetrics -> Word #-}
countDataPointsInResourceMetrics :: (Integral i) => OM.ResourceMetrics -> i
countDataPointsInResourceMetrics resourceMetrics =
  getSum $ foldMap (Sum . countDataPointsInScopeMetrics) (resourceMetrics ^. OM.vec'scopeMetrics)

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OM.ScopeMetrics`.
-}
{-# SPECIALIZE countDataPointsInScopeMetrics :: OM.ScopeMetrics -> Int64 #-}
{-# SPECIALIZE countDataPointsInScopeMetrics :: OM.ScopeMetrics -> Word #-}
countDataPointsInScopeMetrics :: (Integral i) => OM.ScopeMetrics -> i
countDataPointsInScopeMetrics scopeMetrics =
  getSum $ foldMap (Sum . countDataPointsInMetric) (scopeMetrics ^. OM.vec'metrics)

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OM.Metric`.
-}
{-# SPECIALIZE countDataPointsInMetric :: OM.Metric -> Int64 #-}
{-# SPECIALIZE countDataPointsInMetric :: OM.Metric -> Word #-}
countDataPointsInMetric :: (Integral i) => OM.Metric -> i
countDataPointsInMetric metric =
  fromIntegral $
    case metric ^. OM.maybe'data' of
      Nothing -> 0
      Just (OM.Metric'Gauge gauge) ->
        V.length (gauge ^. OM.vec'dataPoints)
      Just (OM.Metric'Sum sum_) ->
        V.length (sum_ ^. OM.vec'dataPoints)
      Just (OM.Metric'Histogram histogram) ->
        V.length (histogram ^. OM.vec'dataPoints)
      Just (OM.Metric'ExponentialHistogram exponentialHistogram) ->
        V.length (exponentialHistogram ^. OM.vec'dataPoints)
      Just (OM.Metric'Summary summary) ->
        V.length (summary ^. OM.vec'dataPoints)

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
