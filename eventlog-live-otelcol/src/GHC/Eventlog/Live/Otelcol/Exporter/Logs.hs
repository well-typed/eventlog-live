{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Otelcol.Exporter.Logs (
  -- * Logs
  ExportLogsResult (..),
  RejectedLogsError (..),
  exportResourceLogs,
) where

import Control.Exception (Exception (..), SomeException (..), catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, await, construct, yield)
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Eventlog.Live.Machine.Core (Tick (..))
import Lens.Family2 ((^.))
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Network.GRPC.Common.Protobuf qualified as G
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as OLS
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as OLS
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as OL
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporters
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Result for Logs

data ExportLogsResult
  = ExportLogsResult
  { exportedLogRecords :: !Int64
  , rejectedLogRecords :: !Int64
  , maybeSomeException :: Maybe SomeException
  }
  deriving (Show)

pattern ExportLogsSuccess :: Int64 -> ExportLogsResult
pattern ExportLogsSuccess exportedLogRecords =
  ExportLogsResult exportedLogRecords 0 Nothing

pattern ExportLogsError :: Int64 -> Int64 -> SomeException -> ExportLogsResult
pattern ExportLogsError exportedLogRecords rejectedLogRecords someException =
  ExportLogsResult exportedLogRecords rejectedLogRecords (Just someException)

data RejectedLogsError
  = RejectedLogsError
  { rejectedLogRecords :: !Int64
  , errorMessage :: !Text
  }
  deriving (Show)

instance Exception RejectedLogsError where
  displayException :: RejectedLogsError -> String
  displayException RejectedLogsError{..} =
    printf "Error: OpenTelemetry Collector rejected %d log records with message: %s" rejectedLogRecords errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Logs

exportResourceLogs ::
  G.Connection ->
  ProcessT IO (Tick OLS.ExportLogsServiceRequest) (Tick ExportLogsResult)
exportResourceLogs conn = construct $ go False
 where
  go exportedResourceLogs =
    await >>= \case
      Tick -> do
        unless exportedResourceLogs $
          yield (Item $ ExportLogsSuccess 0)
        yield Tick
        go False
      Item exportLogsServiceRequest -> do
        exportLogsResult <- liftIO (sendResourceLogs exportLogsServiceRequest)
        yield (Item exportLogsResult)
        go True

  sendResourceLogs :: OLS.ExportLogsServiceRequest -> IO ExportLogsResult
  sendResourceLogs exportLogsServiceRequest =
    doGrpc `catch` handleSomeException
   where
    !exportedLogRecords = countLogRecordsInExportLogsServiceRequest exportLogsServiceRequest

    doGrpc :: IO ExportLogsResult
    doGrpc = do
      G.nonStreaming conn (G.rpc @(Protobuf OLS.LogsService "export")) (G.Proto exportLogsServiceRequest) >>= \case
        G.Proto resp
          | resp ^. OLS.partialSuccess . OLS.rejectedLogRecords == 0 -> do
              pure $ ExportLogsSuccess exportedLogRecords
          | otherwise -> do
              let !rejectedLogRecords = resp ^. OLS.partialSuccess . OLS.rejectedLogRecords
              let !rejectedLogsError = RejectedLogsError{errorMessage = resp ^. OLS.partialSuccess . OLS.errorMessage, ..}
              pure $ ExportLogsError exportedLogRecords rejectedLogRecords (SomeException rejectedLogsError)

    handleSomeException :: SomeException -> IO ExportLogsResult
    handleSomeException someException = pure $ ExportLogsError 0 exportedLogRecords someException

type instance G.RequestMetadata (Protobuf OLS.LogsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OLS.LogsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OLS.LogsService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

{- |
Internal helper.
Count the number of `OL.NumberDataPoint` values in an `OLS.ExportLogsServiceRequest`.
-}
{-# SPECIALIZE countLogRecordsInExportLogsServiceRequest :: OLS.ExportLogsServiceRequest -> Int64 #-}
{-# SPECIALIZE countLogRecordsInExportLogsServiceRequest :: OLS.ExportLogsServiceRequest -> Word #-}
countLogRecordsInExportLogsServiceRequest :: (Integral i) => OLS.ExportLogsServiceRequest -> i
countLogRecordsInExportLogsServiceRequest exportLogsServiceRequest =
  getSum $ foldMap (Sum . countLogRecordsInResourceLogs) (exportLogsServiceRequest ^. OLS.vec'resourceLogs)

{- |
Internal helper.
Count the number of `OL.NumberDataPoint` values in an `OL.ResourceLogs`.
-}
{-# SPECIALIZE countLogRecordsInResourceLogs :: OL.ResourceLogs -> Int64 #-}
{-# SPECIALIZE countLogRecordsInResourceLogs :: OL.ResourceLogs -> Word #-}
countLogRecordsInResourceLogs :: (Integral i) => OL.ResourceLogs -> i
countLogRecordsInResourceLogs resourceLogs =
  getSum $ foldMap (Sum . countLogRecordsInScopeLogs) (resourceLogs ^. OL.vec'scopeLogs)

{- |
Internal helper.
Count the number of `OL.NumberDataPoint` values in an `OL.ScopeLogs`.
-}
{-# SPECIALIZE countLogRecordsInScopeLogs :: OL.ScopeLogs -> Int64 #-}
{-# SPECIALIZE countLogRecordsInScopeLogs :: OL.ScopeLogs -> Word #-}
countLogRecordsInScopeLogs :: (Integral i) => OL.ScopeLogs -> i
countLogRecordsInScopeLogs scopeLogs =
  fromIntegral $
    V.length (scopeLogs ^. OL.vec'logRecords)
