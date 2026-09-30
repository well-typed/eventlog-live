{- |
Module      : GHC.Eventlog.Live.LogRecord
Description : Representation for OTLP log records.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.LogRecord (
  -- * Log record superclass
  IsLogRecord,
  toLogRecord,

  -- * Generic log record type
  LogRecord (..),
) where

import Data.Text (Text)
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.Eventlog.Live.Data.Severity (Severity)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField)

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
