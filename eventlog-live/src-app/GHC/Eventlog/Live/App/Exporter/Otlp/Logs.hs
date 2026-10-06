{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.App.Exporter.Otlp.Logs (
  -- * Export
  exportResourceLogs,

  -- * Conversion to OTLP
  toExportLogsServiceRequest,
  toResourceLogs,
  toScopeLogs,
  toLogRecords,
) where

import Control.Exception (SomeException (..), catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Data (Proxy (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, traversing)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (..))
import Data.Vector qualified as V
import GHC.Eventlog.Live.App.Exporter.Otlp.Core (CanExportToConsole, CanExportToOltpViaHttpProtobuf (..), ExportError (..), Exporter (..), export, ifNonEmpty, messageWith, toMaybeKeyValues)
import GHC.Eventlog.Live.Config (FullConfig)
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (ExportResult (..), InternalMetric (..), Logger, logException, logMetric)
import GHC.Eventlog.Live.Types.Attribute ((~=))
import GHC.Eventlog.Live.Types.Logs (LogRecord (..), SomeLogs (..), logConfig)
import GHC.Eventlog.Live.Types.Severity (Severity)
import GHC.Eventlog.Live.Types.Severity qualified as DS
import Lens.Family2 ((.~), (^.))
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as OLS
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as OLS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as OL
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter for Logs

exportResourceLogs ::
  Logger IO ->
  Exporter ->
  ProcessT IO OLS.ExportLogsServiceRequest ()
exportResourceLogs logger exporter =
  traversing sendResourceLogs
 where
  sendResourceLogs :: OLS.ExportLogsServiceRequest -> IO ()
  sendResourceLogs exportLogsServiceRequest =
    doExport `catch` handleSomeException
   where
    doExport :: IO ()
    doExport = do
      resp <- export @OLS.LogsService @"export" logger exporter exportLogsServiceRequest
      let !exported = countLogRecordsInExportLogsServiceRequest exportLogsServiceRequest
      let !rejected = resp ^. OLS.partialSuccess . OLS.rejectedLogRecords
      liftIO (logMetric logger ExportLogs ExportResult{..})
      unless (rejected == 0) $
        liftIO (logException logger $ ExportError $ resp ^. OLS.partialSuccess . OLS.errorMessage)

    handleSomeException :: SomeException -> IO ()
    handleSomeException = logException logger

--------------------------------------------------------------------------------
-- CanExportToConsole

instance CanExportToConsole OLS.LogsService "export"

--------------------------------------------------------------------------------
-- CanExportToOltpViaGrpc

type instance G.RequestMetadata (Protobuf OLS.LogsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OLS.LogsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OLS.LogsService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- CanExportToOltpViaHttpProtobuf

instance CanExportToOltpViaHttpProtobuf OLS.LogsService "export" where
  apiPath :: String
  apiPath = "/v1/logs"

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

--------------------------------------------------------------------------------
-- Conversion to OTLP
--------------------------------------------------------------------------------

toExportLogsServiceRequest :: [OL.ResourceLogs] -> OLS.ExportLogsServiceRequest
toExportLogsServiceRequest resourceLogs =
  messageWith [OL.resourceLogs .~ resourceLogs]

toResourceLogs :: OR.Resource -> [OL.ScopeLogs] -> Maybe OL.ResourceLogs
toResourceLogs resource scopeLogs =
  ifNonEmpty scopeLogs $
    messageWith [OL.resource .~ resource, OL.scopeLogs .~ scopeLogs]

toScopeLogs :: OC.InstrumentationScope -> [OL.LogRecord] -> Maybe OL.ScopeLogs
toScopeLogs instrumentationScope logRecords =
  ifNonEmpty logRecords $
    messageWith [OL.scope .~ instrumentationScope, OL.logRecords .~ logRecords]

toLogRecords :: FullConfig -> SomeLogs -> [OL.LogRecord]
toLogRecords fullConfig (SomeLogs (_log :: Proxy log) logRecords) =
  [toLogRecord logRecord{attrs = ["name" ~= name] <> logRecord.attrs} | logRecord <- logRecords]
 where
  name = C.processorName (.logs) (logConfig $ Proxy @log) fullConfig

toLogRecord :: LogRecord -> OL.LogRecord
toLogRecord l =
  messageWith
    [ OL.body .~ messageWith [OC.stringValue .~ l.value]
    , OL.timeUnixNano .~ fromMaybe 0 l.maybeTimeUnixNano
    , OL.observedTimeUnixNano .~ fromMaybe 0 l.maybeTimeUnixNano
    , OL.severityNumber .~ toSeverityNumber l.maybeSeverity
    , OL.attributes .~ toMaybeKeyValues l.attrs
    ]
 where
  toSeverityNumber :: Maybe Severity -> OL.SeverityNumber
  toSeverityNumber = maybe OL.SEVERITY_NUMBER_UNSPECIFIED (toEnum . (.value) . DS.toSeverityNumber)
