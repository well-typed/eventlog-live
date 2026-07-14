{- |
Module      : GHC.Eventlog.Live.Data.Thread
Description : Reprsentation for thread data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Thread (
  ThreadId (..),
) where

import Data.Word (Word64)

{- |
A thread ID.

In the GHC RTS, all threads are assigned a `Word64` identifier.
However, thread IDs are posted to the eventlog as `Word32` values.

In @ghc-events@, the thread ID is received and represented as a `Word32` value.

In @ghc-stack-profiler@, the thread ID is retrieved and posted as a `Word64` value.
-}
newtype ThreadId = ThreadId
  { value :: Word64
  }
  deriving (Show, Eq, Ord)
