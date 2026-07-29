{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Otlp.Exporter.Traces (
  -- * Traces
  ExportTraceResult (..),
  RejectedSpansError (..),
  exportResourceSpans,
) where

import Control.Exception (Exception (..), SomeException (..), catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, await, construct, yield)
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Core (Tick (..))
import GHC.Eventlog.Live.Otlp.Exporter.Core (CanExportViaHttpProtobuf (..), OtlpExporter (..), export)
import Lens.Family2 ((^.))
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as OTS
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OT
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Result for Traces

data ExportTraceResult
  = ExportTraceResult
  { exportedSpans :: !Int64
  , rejectedSpans :: !Int64
  , maybeSomeException :: Maybe SomeException
  }
  deriving (Show)

pattern ExportTraceSuccess :: Int64 -> ExportTraceResult
pattern ExportTraceSuccess exportedSpans =
  ExportTraceResult exportedSpans 0 Nothing

pattern ExportTraceError :: Int64 -> Int64 -> SomeException -> ExportTraceResult
pattern ExportTraceError exportedSpans rejectedSpans someException =
  ExportTraceResult exportedSpans rejectedSpans (Just someException)

data RejectedSpansError
  = RejectedSpansError
  { rejectedSpans :: !Int64
  , errorMessage :: !Text
  }
  deriving (Show)

instance Exception RejectedSpansError where
  displayException :: RejectedSpansError -> String
  displayException RejectedSpansError{..} =
    printf "Error: OpenTelemetry Collector rejectedSpans %d spans with message: %s" rejectedSpans errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Traces

exportResourceSpans ::
  Logger IO ->
  OtlpExporter ->
  ProcessT IO (Tick OTS.ExportTraceServiceRequest) (Tick ExportTraceResult)
exportResourceSpans logger exporter =
  construct $ go False
 where
  go exportedResourceSpans =
    await >>= \case
      Tick -> do
        unless exportedResourceSpans $
          yield (Item $ ExportTraceSuccess 0)
        yield Tick
        go False
      Item exportTraceServiceRequest -> do
        exportTraceResult <- liftIO (sendResourceSpans exportTraceServiceRequest)
        yield (Item exportTraceResult)
        go True

  sendResourceSpans :: OTS.ExportTraceServiceRequest -> IO ExportTraceResult
  sendResourceSpans exportTraceServiceRequest =
    doGrpc `catch` handleSomeException
   where
    !exportedSpans = countSpansInExportTraceServiceRequest exportTraceServiceRequest

    doGrpc :: IO ExportTraceResult
    doGrpc = do
      resp <- export @OTS.TraceService @"export" logger exporter exportTraceServiceRequest
      if resp ^. OTS.partialSuccess . OTS.rejectedSpans == 0
        then
          pure $ ExportTraceSuccess exportedSpans
        else do
          let !rejectedSpans = resp ^. OTS.partialSuccess . OTS.rejectedSpans
          let !rejectedMetricsError = RejectedSpansError{errorMessage = resp ^. OTS.partialSuccess . OTS.errorMessage, ..}
          pure $ ExportTraceError exportedSpans rejectedSpans (SomeException rejectedMetricsError)

    handleSomeException :: SomeException -> IO ExportTraceResult
    handleSomeException someException = pure $ ExportTraceError 0 exportedSpans someException

type instance G.RequestMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OTS.TraceService meth) = G.NoMetadata

instance CanExportViaHttpProtobuf OTS.TraceService "export" where
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
