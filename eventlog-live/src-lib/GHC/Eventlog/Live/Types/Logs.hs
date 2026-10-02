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

import Data.Default (Default)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import GHC.Eventlog.Live.Config
import GHC.Eventlog.Live.Types.Attribute (Attrs)
import GHC.Eventlog.Live.Types.Severity (Severity)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)

-------------------------------------------------------------------------------
-- KnownLog & Instances
-------------------------------------------------------------------------------

type SomeLogs :: Type
data SomeLogs
  = forall log. (KnownLog log) => SomeLogs !(Proxy log) [LogRecord]

type KnownLog :: Type -> Constraint
class
  ( HasField (GetLogName log) Logs (Maybe log)
  , IsLogProcessorConfig log
  , Show log
  , Default log
  , KnownSymbol (GetLogName log)
  ) =>
  KnownLog log
  where
  type GetLogName log :: Symbol

logConfig :: forall log. (KnownLog log) => Logs -> Maybe log
logConfig = getField @(GetLogName log)
{-# INLINE logConfig #-}

instance KnownLog ThreadLabelLog where
  type GetLogName ThreadLabelLog = "threadLabel"

instance KnownLog UserMarkerLog where
  type GetLogName UserMarkerLog = "userMarker"

instance KnownLog UserMessageLog where
  type GetLogName UserMessageLog = "userMessage"

instance KnownLog InternalLogMessageLog where
  type GetLogName InternalLogMessageLog = "internalLogMessage"

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
