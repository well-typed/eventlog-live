{- |
Module      : GHC.Eventlog.Live.Span
Description : Representation for spans.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Span (
  IsSpan,
  duration,
) where

import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField)

{- |
A span is any type with a start and end time.
-}
type IsSpan s = (HasField "startTimeUnixNano" s Timestamp, HasField "endTimeUnixNano" s Timestamp)

{- |
Determine the duration of a span.
-}
duration :: (IsSpan s) => s -> Timestamp
duration s = if s.startTimeUnixNano < s.endTimeUnixNano then s.endTimeUnixNano - s.startTimeUnixNano else 0
{-# INLINEABLE duration #-}
