{- |
Module      : GHC.Eventlog.Live.Data.Capability
Description : Reprsentation for capability data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Capability (
  CapNo (..),
  evCapNo,
  fromCapabilityId,
) where

import Data.Word (Word16)
import GHC.RTS.Events (Event (..))
import GHC.Stack.Profiler.Core.Eventlog (CapabilityId (..))

{- |
A capability number.

In the GHC RTS, all capabilities are assigned a `Word16` identifier.

In @ghc-events@, the event capability number is received as a `Word16`,
if present, and converted to an `Int`, using @-1@ for global events.

In @ghc-stack-profiler@, the capability number is added to the event,
and upcast to `Word64`.
-}
newtype CapNo = CapNo
  { value :: Word16
  }
  deriving (Show, Eq, Ord)

{- |
Get the capability number from a @ghc-events@ `Event`.
-}
evCapNo :: Event -> Maybe CapNo
evCapNo ev
  | Just cap <- ev.evCap
  , 0 <= cap && cap <= fromIntegral @Word16 maxBound =
      Just (CapNo $ fromIntegral cap)
  | otherwise = Nothing

{- |
Get the capability number from a @ghc-stack-profiler@ `CapabilityId`.
-}
fromCapabilityId :: CapabilityId -> CapNo
fromCapabilityId (MkCapabilityId cap) = CapNo $ fromIntegral cap
