{- |
Module      : GHC.Eventlog.Live.Socket
Description : Utilities for running eventlog machines with sockets.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Socket (
  EventlogSocket (..),
  Tick (..),
  connect,
  runWithEventlogSocket,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Machine (ProcessT, runT_, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import GHC.Eventlog.Live.Machines
import GHC.Eventlog.Live.Options (EventlogSocket (..))
import GHC.RTS.Events (Event)
import Network.Socket qualified as S
import System.IO (Handle)
import System.IO qualified as IO

{- |
Run an event processor with an eventlog socket.
-}
runWithEventlogSocket ::
  (MonadUnliftIO m) =>
  -- | The batch interval in milliseconds.
  Int ->
  -- | The number of bytes to read (defaults to 4KiB).
  Maybe Int ->
  -- | The eventlog socket handle.
  EventlogSocket ->
  -- | An optional file to which to stream binary eventlog data.
  Maybe FilePath ->
  -- | The event processor.
  ProcessT m (Tick Event) Void ->
  m ()
runWithEventlogSocket batchIntervalMs maybeChuckSizeBytes eventlogSocket maybeOutputFile toEventSink = do
  -- TODO: Handle connection errors by waiting for the socket to be created.
  eventlogHandle <- liftIO $ connect eventlogSocket
  let chuckSizeBytes = fromMaybe defaultChunkSizeBytes maybeChuckSizeBytes
  let fromSocket = sourceHandleBatch batchIntervalMs chuckSizeBytes eventlogHandle
  case maybeOutputFile of
    Nothing ->
      runT_ $
        fromSocket ~> decodeEventBatch ~> toEventSink
    Just outputFile ->
      withRunInIO $ \runInIO ->
        IO.withFile outputFile IO.WriteMode $ \outputHandle -> do
          runInIO . runT_ $
            fromSocket
              ~> fanout
                [ fileSinkBatch outputHandle
                , decodeEventBatch ~> toEventSink
                ]

{- |
Connect to an eventlog socket.
-}
connect :: EventlogSocket -> IO Handle
connect = \case
  EventlogSocketUnix socketName -> do
    socket <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
    S.connect socket (S.SockAddrUnix socketName)
    handle <- S.socketToHandle socket IO.ReadMode
    IO.hSetBuffering handle IO.NoBuffering
    pure handle
