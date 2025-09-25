{- |
Module      : GHC.Eventlog.Live.Socket
Description : Utilities for running eventlog machines with sockets.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Socket (
  EventlogSocket (..),
  Tick (..),
  tryConnect,
  runWithEventlogSocket,
) where

import Control.Concurrent (threadDelay)
import Control.Exception qualified as E
import Control.Monad.IO.Unlift
import Data.Machine
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import GHC.Eventlog.Live.Machines
import GHC.Eventlog.Live.Options (EventlogSocket (..))
import GHC.RTS.Events (Event)
import Network.Socket qualified as S
import System.IO (Handle)
import System.IO qualified as IO
import Data.Machine.MealyT (scanMealyTM)
import Control.Monad (when)

{- |
Run an event processor with an eventlog socket.
-}
runWithEventlogSocket ::
  (MonadUnliftIO m) =>
  -- | The eventlog socket handle.
  EventlogSocket ->
  -- | The initial timeout in microseconds for exponential backoff.
  Double ->
  -- | The timeout exponent for exponential backoff.
  Double ->
  -- | The batch interval in milliseconds.
  Int ->
  -- | The number of bytes to read (defaults to 4KiB).
  Maybe Int ->
  -- | An optional file to which to stream binary eventlog data.
  Maybe FilePath ->
  -- | The event processor.
  ProcessT m (Tick Event) Void ->
  m ()
runWithEventlogSocket eventlogSocket timeoutExponent initialTimeoutMcs batchIntervalMs maybeChuckSizeBytes maybeOutputFile toEventSink = do
  liftIO $ print "Running with eventlog socket"
  -- TODO: Handle connection errors by waiting for the socket to be created.
  withEventlogSocket timeoutExponent initialTimeoutMcs eventlogSocket $ \eventlogHandle -> do
    let chuckSizeBytes = fromMaybe defaultChunkSizeBytes maybeChuckSizeBytes
    let fromSocket = sourceHandleBatch batchIntervalMs chuckSizeBytes eventlogHandle
    case maybeOutputFile of
      Nothing ->
        runT_ $
          fromSocket ~> fanout [decodeEventBatch ~> fanout [loggingSink, toEventSink ]]
      Just outputFile ->
        withRunInIO $ \runInIO ->
          IO.withFile outputFile IO.WriteMode $ \outputHandle -> do
            runInIO . runT_ $
              fromSocket
                ~> fanout
                  [ fileSinkBatch outputHandle
                  , decodeEventBatch ~> fanout [loggingSink, toEventSink]
                  ]

-- | Log progress about how many events have been processed
loggingSink :: forall m . MonadIO m => ProcessT m (Tick Event) Void
loggingSink = construct (go 0)
  where
    go !n = do
      e <- await
      when (n `mod` 100 == 0) $ liftIO $ print ("Processed " ++ show n ++ " events")
      go (n + 1)

{- |
Run an action with a `Handle` to an `EventlogSocket`.
-}
withEventlogSocket ::
  (MonadUnliftIO m) =>
  -- | The initial timeout in microseconds for exponential backoff.
  Double ->
  -- | The timeout exponent for exponential backoff.
  Double ->
  -- | The eventlog socket.
  EventlogSocket ->
  (Handle -> m ()) ->
  m ()
withEventlogSocket initialTimeoutMcs timeoutExponent eventlogSocket action = do
  withRunInIO $ \runInIO ->
    E.bracket (connectRetry initialTimeoutMcs timeoutExponent eventlogSocket) IO.hClose $ \handle ->
      runInIO $
        action handle

{- |
Connect to an `EventlogSocket` with retries and non-randomised exponential backoff.
-}
connectRetry ::
  -- | The initial timeout in microseconds for exponential backoff.
  Double ->
  -- | The timeout exponent for exponential backoff.
  Double ->
  -- | The eventlog socket.
  EventlogSocket ->
  IO Handle
connectRetry initialTimeoutMcs timeoutExponent eventlogSocket =
  connectLoop initialTimeoutMcs
 where
  waitFor :: Double -> IO ()
  waitFor timeoutMcs = threadDelay $ round $ timeoutMcs * 1_000_000

  connectLoop :: Double -> IO Handle
  connectLoop timeoutMcs = do
    E.catch (tryConnect eventlogSocket) $ \(_e :: E.IOException) -> do
      print ("Failed to connect to eventlog socket, retrying" ++ show eventlogSocket ++ " with timeout " ++ show timeoutMcs)
      waitFor timeoutMcs
      connectLoop (timeoutMcs * timeoutExponent)

{- |
Try to connect to an `EventlogSocket`.
-}
tryConnect :: EventlogSocket -> IO Handle
tryConnect = \case
  EventlogSocketUnix socketName ->
    E.bracketOnError (S.socket S.AF_UNIX S.Stream S.defaultProtocol) S.close $ \socket -> do
      S.connect socket (S.SockAddrUnix socketName)
      handle <- S.socketToHandle socket IO.ReadMode
      IO.hSetBuffering handle IO.NoBuffering
      pure handle
