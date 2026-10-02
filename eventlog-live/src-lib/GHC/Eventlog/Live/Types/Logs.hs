{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.LogRecord
Description : Representation for OTLP log records.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Types.Logs (
  -- * Logs
  SomeLogs (..),
  KnownLog (..),
  logConfig,

  -- * Log record superclass
  IsLogRecord,
  toLogRecord,

  -- * Generic log record type
  LogRecord (..),
) where

import Data.Aeson.Types (Encoding, KeyValue (..), KeyValueOmit (..), ToJSON (..), Value (..), pairs)
import Data.Default (Default)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import GHC.Eventlog.Live.Config
import GHC.Eventlog.Live.Types.Attribute (Attrs)
import GHC.Eventlog.Live.Types.Severity (Severity)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-------------------------------------------------------------------------------
-- KnownLog & Instances
-------------------------------------------------------------------------------

type SomeLogs :: Type
data SomeLogs
  = forall log. (KnownLog log) => SomeLogs !(Proxy log) [LogRecord]

instance ToJSON SomeLogs where
  toJSON :: SomeLogs -> Value
  toJSON = Object . someLogsToKV

  toEncoding :: SomeLogs -> Encoding
  toEncoding = pairs . someLogsToKV

  omitField :: SomeLogs -> Bool
  omitField (SomeLogs _log logs) = null logs

someLogsToKV :: (KeyValueOmit e kv, Monoid kv) => SomeLogs -> kv
someLogsToKV (SomeLogs (log_ :: Proxy log) logs) =
  mconcat $
    [ "type" .= ("log" :: Text)
    , "name" .= symbolVal log_
    , "values" .?= logs
    ]
{-# INLINE someLogsToKV #-}

type KnownLog :: Symbol -> Constraint
class
  ( HasField log Logs (Maybe (GetLogConf log))
  , IsLogProcessorConfig (GetLogConf log)
  , Show (GetLogConf log)
  , Default (GetLogConf log)
  , KnownSymbol log
  ) =>
  KnownLog log
  where
  type GetLogConf log :: Type

logConfig :: forall log. (KnownLog log) => Proxy log -> Logs -> Maybe (GetLogConf log)
logConfig (_log :: Proxy log) = getField @log
{-# INLINE logConfig #-}

instance KnownLog "threadLabel" where
  type GetLogConf "threadLabel" = ThreadLabelLog

instance KnownLog "userMarker" where
  type GetLogConf "userMarker" = UserMarkerLog

instance KnownLog "userMessage" where
  type GetLogConf "userMessage" = UserMessageLog

instance KnownLog "internalLogMessage" where
  type GetLogConf "internalLogMessage" = InternalLogMessageLog

--------------------------------------------------------------------------------
-- Log Records
--------------------------------------------------------------------------------

{- |
A log record is any type that has the fields of the generic `LogRecord` type.
-}
type IsLogRecord l =
  ( HasField "value" l Text
  , HasField "maybeTimeUnixNano" l (Maybe Timestamp)
  , HasField "maybeSeverity" l (Maybe Severity)
  , HasField "attrs" l Attrs
  )

toLogRecord :: (IsLogRecord l) => l -> LogRecord
toLogRecord l =
  LogRecord
    { value = l.value
    , maybeTimeUnixNano = l.maybeTimeUnixNano
    , maybeSeverity = l.maybeSeverity
    , attrs = l.attrs
    }

{- |
LogRecords combine a timestamp, message and a severity.
-}
data LogRecord = LogRecord
  { value :: !Text
  -- ^ The log message.
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  -- ^ The time at which the log was created.
  , maybeSeverity :: !(Maybe Severity)
  -- ^ The severity of the log.
  , attrs :: Attrs
  -- ^ A set of attributes.
  }
  deriving (Show)

instance ToJSON LogRecord where
  toJSON :: LogRecord -> Value
  toJSON = Object . logRecordToKV

  toEncoding :: LogRecord -> Encoding
  toEncoding = pairs . logRecordToKV

logRecordToKV :: (KeyValueOmit e kv, Monoid kv) => LogRecord -> kv
logRecordToKV l =
  mconcat $
    [ "value" .= l.value
    , "time_unix_nano" .?= l.maybeTimeUnixNano
    , "severity" .?= l.maybeSeverity
    , "attrs" .?= l.attrs
    ]
{-# INLINE logRecordToKV #-}
