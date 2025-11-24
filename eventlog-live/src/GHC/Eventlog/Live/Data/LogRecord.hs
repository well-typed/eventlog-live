{- |
Module      : GHC.Eventlog.Live.LogRecord
Description : Representation for metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.LogRecord (
  LogRecord (..),
) where

import Data.Text (Text)
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.Eventlog.Live.Data.Severity (Severity)
import GHC.RTS.Events (Timestamp)

{- |
LogRecords combine a timestamp, message and a severity.
-}
data LogRecord = LogRecord
  { body :: !Text
  -- ^ The log message.
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  -- ^ The time at which the log was created.
  , maybeSeverity :: !(Maybe Severity)
  -- ^ The severity of the log.
  , attrs :: Attrs
  -- ^ A set of attributes.
  }
  deriving (Show)
