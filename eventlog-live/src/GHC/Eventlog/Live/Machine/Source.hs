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
import Data.Function (fix)
import Data.Machine (MachineT (..), construct, yield)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Eventlog.Live.Machine.Core (Tick (..))
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
  (MonadIO m) =>
  -- | The wait timeout in milliseconds.
  Int ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog socket handle.
  Handle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleWait timeoutMilli chunkSizeBytes handle =
  construct $ fix $ \loop -> do
    ready <- liftIO $ hWaitForInput' handle timeoutMilli
    case ready of
      Ready -> do
        bs <- liftIO $ BS.hGetSome handle chunkSizeBytes
        yield (Item bs)
        loop
      NotReady -> do
        yield Tick
        loop
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
  -- | The eventlog flush interval in seconds.
  Double ->
  -- | The number of bytes to read.
  Int ->
  -- | The eventlog socket handle.
  Handle ->
  MachineT m k (Tick BS.ByteString)
sourceHandleBatch eventlogFlushIntervalS chunkSizeBytes handle = construct start
 where
  eventlogFlushIntervalMs :: Int
  eventlogFlushIntervalMs = round (eventlogFlushIntervalS * 1_000)

  start = do
    startTimeMs <- liftIO getMonotonicTimeMilli
    batch startTimeMs
  batch startTimeMs = waitForInput
   where
    getRemainingTimeMilli = do
      currentTimeMilli <- liftIO getMonotonicTimeMilli
      pure $ (startTimeMs + eventlogFlushIntervalMs) - currentTimeMilli
    waitForInput = do
      remainingTimeMilli <- getRemainingTimeMilli
      if remainingTimeMilli <= 0
        then do
          yield Tick
          start
        else do
          ready <- liftIO (hWaitForInput' handle remainingTimeMilli)
          case ready of
            Ready -> do
              chunk <- liftIO $ BS.hGetSome handle chunkSizeBytes
              yield (Item chunk) >> waitForInput
            NotReady -> waitForInput
            EOF -> pure ()

{- |
Eventlog chunk size in bytes.
This should be equal to the page size.
-}
defaultChunkSizeBytes :: Int
defaultChunkSizeBytes = 4096

{- |
Internal helper.
Return monotonic time in milliseconds, since some unspecified starting point
-}
getMonotonicTimeMilli :: IO Int
getMonotonicTimeMilli = nanoToMilli <$> getMonotonicTimeNSec

{- |
Internal helper.
Convert nanoseconds to milliseconds.
The conversion from 'Word64' to 'Int' is safe.
It cannot overflow due to the division by 1_000_000.
-}
nanoToMilli :: Word64 -> Int
nanoToMilli = fromIntegral . (`div` 1_000_000)

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
