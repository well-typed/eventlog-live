module GHC.Eventlog.Live.Types.Time (
  TimeUnixNano,
) where

import Data.Word (Word64)

{- |
Time in nanoseconds since the Unix epoch.
-}
type TimeUnixNano = Word64
