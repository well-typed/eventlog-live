{- |
Module      : GHC.Eventlog.Live.Severity
Description : Representation for metrics.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Severity (
  Severity (..),
  SeverityNumber (..),
  toSeverityNumber,
  fromSeverityNumber,
  toSeverityString,
  fromSeverityString,
) where

import Data.Char (toUpper)
import Data.Ix (Ix)
import Text.Read (readMaybe)

{- |
The severity number as specified by the OpenTelemetry specification.

See: https://opentelemetry.io/docs/specs/otel/logs/data-model/#field-severitynumber
-}
newtype SeverityNumber = SeverityNumber {value :: Int}

{- |
The severity as specified by the OpenTelemetry specification.

See: https://opentelemetry.io/docs/specs/otel/logs/data-model/#displaying-severity
-}
data Severity
  = TRACE
  | TRACE2
  | TRACE3
  | TRACE4
  | DEBUG
  | DEBUG2
  | DEBUG3
  | DEBUG4
  | INFO
  | INFO2
  | INFO3
  | INFO4
  | WARN
  | WARN2
  | WARN3
  | WARN4
  | ERROR
  | ERROR2
  | ERROR3
  | ERROR4
  | FATAL
  | FATAL2
  | FATAL3
  | FATAL4
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Ix)

{- |
Convert from a `Severity` to a `SeverityNumber`.
-}
toSeverityNumber :: Severity -> SeverityNumber
toSeverityNumber = SeverityNumber . (+ 1) . fromEnum

{- |
Convert from a `SeverityNumber` to a `Severity`.
-}
fromSeverityNumber :: SeverityNumber -> Maybe Severity
fromSeverityNumber severityNumber
  | 1 <= severityNumber.value && severityNumber.value <= 24 =
      Just (toEnum $ severityNumber.value - 1)
  | otherwise = Nothing

{- |
Convert from a `Severity` to a `String`.
-}
toSeverityString :: Severity -> String
toSeverityString = show

{- |
Convert from a `String` to a `Severity`.
-}
fromSeverityString :: String -> Maybe Severity
fromSeverityString = readMaybe . fmap toUpper
