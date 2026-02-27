{- |
Module      : GHC.Eventlog.Live.Machine.Source
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Source (
  -- * Eventlog source
  eventlogSourceTick,
  defaultChunkSizeBytes,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Machine (MachineT (..), construct, yield)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Eventlog.Live.Machine.Core (Tick (..), TickInfo (..))
import GHC.Eventlog.Live.Source.Core (EventlogSourceData (..), EventlogSourceHandle, recv)

{- |
A source which reads chunks from a `Handle`.
When input is available, it yields an v`Item`.
It yields a v`Tick` at each increment of the batch interval.
-}
eventlogSourceTick ::
  (MonadIO m) =>
  -- | The batch interval in milliseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog source handle.
  EventlogSourceHandle ->
  MachineT m k (Tick BS.ByteString)
eventlogSourceTick batchIntervalMilli chunkSizeBytes h =
  construct $ start 0
 where
  batchIntervalMicro = milliToMicro batchIntervalMilli

  start tick = do
    startTimeMicro <- liftIO getMonotonicTimeMicro
    batch tick startTimeMicro

  batch tick startTimeMicro = batchLoop
   where
    getRemainingTimeMicro = do
      currentTimeMicro <- liftIO getMonotonicTimeMicro
      pure $ (startTimeMicro + batchIntervalMicro) - currentTimeMicro

    batchLoop = do
      remainingTimeMicro <- getRemainingTimeMicro
      if remainingTimeMicro <= 0
        then do
          yield TickWithInfo{tickInfo = TickInfo{tick}}
          start (tick + 1)
        else do
          liftIO (recv h remainingTimeMicro chunkSizeBytes) >>= \case
            EventlogSourceData chunk -> yield (Item chunk) >> batchLoop
            EventlogSourceTimeout -> batchLoop
            EventlogSourceClosed -> pure ()

{- |
Eventlog chunk size in bytes.
This should be equal to the page size.
-}
defaultChunkSizeBytes :: Int
defaultChunkSizeBytes = 4096

{- |
Internal helper.
Return monotonic time in microseconds, since some unspecified starting point
-}
getMonotonicTimeMicro :: IO Int
getMonotonicTimeMicro = nanoToMicro <$> getMonotonicTimeNSec

{- |
Internal helper.
Convert nanoseconds to microseconds.
If the size of @Int@ is at least as big as that of @Word64@, then
the conversion from 'Word64' to 'Int' is safe, due to the division by 1000.
-}
nanoToMicro :: Word64 -> Int
nanoToMicro = fromIntegral . (`div` 1_000)

{- |
Internal helper.
Convert milliseconds to microseconds.
-}
milliToMicro :: Int -> Int
milliToMicro = (* 1_000)
