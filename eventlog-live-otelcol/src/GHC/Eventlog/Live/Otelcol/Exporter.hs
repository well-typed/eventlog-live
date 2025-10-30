{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Otelcol.Exporter (
  ExportMetricsError (..),
  exportResourceMetrics,
  ExportTraceError (..),
  exportResourceSpans,
) where

import Control.Exception (Exception (..), catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Machine (ProcessT, await, repeatedly, yield)
import Data.Text (Text)
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
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporters
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Error for Metrics

data ExportMetricsError
  = ExportMetricsError
  { rejectedDataPoints :: Int64
  , errorMessage :: Text
  }
  deriving (Show)

instance Exception ExportMetricsError where
  displayException :: ExportMetricsError -> String
  displayException ExportMetricsError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedDataPoints errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Metrics

exportResourceMetrics :: G.Connection -> ProcessT IO OMS.ExportMetricsServiceRequest (Either G.GrpcError ExportMetricsError)
exportResourceMetrics conn =
  repeatedly $
    await >>= \exportMetricsServiceRequest ->
      liftIO (sendResourceMetrics exportMetricsServiceRequest)
        >>= traverse_ yield
 where
  sendResourceMetrics :: OMS.ExportMetricsServiceRequest -> IO (Maybe (Either G.GrpcError ExportMetricsError))
  sendResourceMetrics exportMetricsServiceRequest =
    fmap (fmap Right) doGrpc `catch` handleGrpcError
   where
    doGrpc :: IO (Maybe ExportMetricsError)
    doGrpc = do
      G.nonStreaming conn (G.rpc @(Protobuf OMS.MetricsService "export")) (G.Proto exportMetricsServiceRequest) >>= \case
        G.Proto resp
          | resp ^. OMS.partialSuccess . OMS.rejectedDataPoints == 0 -> pure Nothing
          | otherwise -> pure $ Just ExportMetricsError{..}
         where
          rejectedDataPoints = resp ^. OMS.partialSuccess . OMS.rejectedDataPoints
          errorMessage = resp ^. OMS.partialSuccess . OMS.errorMessage
    handleGrpcError :: G.GrpcError -> IO (Maybe (Either G.GrpcError ExportMetricsError))
    handleGrpcError = pure . pure . Left

type instance G.RequestMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Error for Traces

data ExportTraceError
  = ExportTraceError
  { rejectedSpans :: Int64
  , errorMessage :: Text
  }
  deriving (Show)

instance Exception ExportTraceError where
  displayException :: ExportTraceError -> String
  displayException ExportTraceError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedSpans errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Traces

exportResourceSpans :: G.Connection -> ProcessT IO OTS.ExportTraceServiceRequest (Either G.GrpcError ExportTraceError)
exportResourceSpans conn =
  repeatedly $
    await >>= \exportTraceServiceRequest ->
      liftIO (sendResourceSpans exportTraceServiceRequest)
        >>= traverse_ yield
 where
  sendResourceSpans :: OTS.ExportTraceServiceRequest -> IO (Maybe (Either G.GrpcError ExportTraceError))
  sendResourceSpans exportTraceServiceRequest =
    fmap (fmap Right) doGrpc `catch` handleGrpcError
   where
    doGrpc :: IO (Maybe ExportTraceError)
    doGrpc = do
      G.nonStreaming conn (G.rpc @(Protobuf OTS.TraceService "export")) (G.Proto exportTraceServiceRequest) >>= \case
        G.Proto resp
          | resp ^. OTS.partialSuccess . OTS.rejectedSpans == 0 -> pure Nothing
          | otherwise -> pure $ Just ExportTraceError{..}
         where
          rejectedSpans = resp ^. OTS.partialSuccess . OTS.rejectedSpans
          errorMessage = resp ^. OTS.partialSuccess . OTS.errorMessage
    handleGrpcError :: G.GrpcError -> IO (Maybe (Either G.GrpcError ExportTraceError))
    handleGrpcError = pure . pure . Left

type instance G.RequestMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
