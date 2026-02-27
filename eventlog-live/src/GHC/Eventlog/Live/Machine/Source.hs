{- |
Module      : GHC.Eventlog.Live.Machine.Source
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Source (
  -- * Eventlog source
  sourceHandleWait,
  sourceHandleBatch,
  defaultChunkSizeBytes,
) where

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.Machine (MachineT (..), PlanT, construct, yield)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Eventlog.Live.Machine.Core (Tick (..), TickInfo (..))
import GHC.Eventlog.Live.Source.Core (EventlogSourceHandle, EventlogSourceData (..), recv)
import System.IO (Handle, hWaitForInput)
import System.IO.Error (isEOFError)

-------------------------------------------------------------------------------
-- Socket source

{- |
A source which reads chunks from a `Handle`.
When an input is available, it yields an v`Item`.
When the timeout is reached, it yields a v`Tick`.
-}
sourceHandleWait ::
  forall m k.
  (MonadIO m) =>
  -- | The wait timeout in milliseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog socket handle.
  Handle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleWait timeoutMilli chunkSizeBytes handle =
  construct $ go 0
 where
  go :: Word -> PlanT k (Tick BS.ByteString) m ()
  go tick = do
    ready <- liftIO $ hWaitForInput' handle timeoutMilli
    case ready of
      Ready -> do
        bs <- liftIO $ BS.hGetSome handle chunkSizeBytes
        yield (Item bs)
        go tick
      NotReady -> do
        yield TickWithInfo{tickInfo = TickInfo{tick}}
        go (tick + 1)
      EOF ->
        pure ()

-------------------------------------------------------------------------------
-- Socket source with batches

{- |
A source which reads chunks from a `Handle`.
When input is available, it yields an v`Item`.
It yields a v`Tick` at each increment of the batch interval.
-}
sourceHandleBatch ::
  (MonadIO m) =>
  -- | The batch interval in milliseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog source handle.
  EventlogSourceHandle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleBatch batchIntervalMilli chunkSizeBytes h =
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

{- |
Internal helper.
Type to represent the state of a handle.
-}
data Ready = Ready | NotReady | EOF

{- |
Internal helper.
Wait for input from a `Handle` for a given number of milliseconds.
-}
hWaitForInput' ::
  -- | The handle.
  Handle ->
  -- | The timeout in milliseconds.
  Int ->
  IO Ready
hWaitForInput' handle timeoutMilli =
  catch (boolToReady <$> hWaitForInput handle timeoutMilli) handleEOFError
 where
  boolToReady True = Ready
  boolToReady False = NotReady
  handleEOFError err
    | isEOFError err = pure EOF
    | otherwise = throwIO err
