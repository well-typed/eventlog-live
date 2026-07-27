{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Common.Logs
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Common.Logs (
  ToLogRecord (..),
  toExportLogsServiceRequest,
  toResourceLogs,
  toScopeLogs,
)
where

import Data.Function ((&))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ProtoLens (Message (..))
import Data.Text (Text)
import GHC.Eventlog.Live.Data.Attribute ((~=))
import GHC.Eventlog.Live.Data.LogRecord (LogRecord (..))
import GHC.Eventlog.Live.Data.Severity (Severity)
import GHC.Eventlog.Live.Data.Severity qualified as DS
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core (ifNonEmpty, messageWith, toMaybeKeyValue)
import GHC.IsList (IsList (..))
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as OLS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as OL
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR

toExportLogsServiceRequest :: [OL.ResourceLogs] -> OLS.ExportLogsServiceRequest
toExportLogsServiceRequest = (defMessage &) . (OL.resourceLogs .~)

toResourceLogs :: OR.Resource -> [OL.ScopeLogs] -> Maybe OL.ResourceLogs
toResourceLogs resource scopeLogs =
  ifNonEmpty scopeLogs $
    messageWith [OL.resource .~ resource, OL.scopeLogs .~ scopeLogs]

toScopeLogs :: OC.InstrumentationScope -> [OL.LogRecord] -> Maybe OL.ScopeLogs
toScopeLogs instrumentationScope logRecords =
  ifNonEmpty logRecords $
    messageWith [OL.scope .~ instrumentationScope, OL.logRecords .~ logRecords]

--------------------------------------------------------------------------------
-- Interpret logs

class ToLogRecord v where
  toLogRecord :: v -> OL.LogRecord

instance ToLogRecord LogRecord where
  toLogRecord :: LogRecord -> OL.LogRecord
  toLogRecord i =
    messageWith
      [ OL.body .~ messageWith [OC.stringValue .~ i.body]
      , OL.timeUnixNano .~ fromMaybe 0 i.maybeTimeUnixNano
      , -- TODO: this could be set to the actual observed time in the processor.
        OL.observedTimeUnixNano .~ fromMaybe 0 i.maybeTimeUnixNano
      , OL.attributes .~ mapMaybe toMaybeKeyValue (toList i.attrs)
      , OL.severityNumber .~ toSeverityNumber i.maybeSeverity
      ]
   where
    toSeverityNumber :: Maybe Severity -> OL.SeverityNumber
    toSeverityNumber = maybe OL.SEVERITY_NUMBER_UNSPECIFIED (toEnum . (.value) . DS.toSeverityNumber)

instance ToLogRecord M.ThreadLabel where
  toLogRecord :: M.ThreadLabel -> OL.LogRecord
  toLogRecord i =
    messageWith
      [ OL.body .~ messageWith [OC.stringValue .~ i.threadlabel]
      , OL.timeUnixNano .~ i.startTimeUnixNano
      , -- TODO: this could be set to the actual observed time in the processor.
        OL.observedTimeUnixNano .~ i.startTimeUnixNano
      , OL.attributes
          .~ mapMaybe
            toMaybeKeyValue
            [ "kind" ~= ("ThreadLabel" :: Text)
            , "thread" ~= i.thread
            ]
      ]
