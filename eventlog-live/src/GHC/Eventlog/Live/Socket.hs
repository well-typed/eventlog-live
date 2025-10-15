{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Socket
Description : Utilities for running eventlog machines with sockets.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Socket (
  EventlogSource (..),
  Tick (..),
  tryConnect,
  runWithEventlogSource,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception (..))
import Control.Exception qualified as E
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Foldable (traverse_)
import Data.Machine (ProcessT, runT_, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Eventlog.Live.Internal.Logger (logDebug, logInfo)
import GHC.Eventlog.Live.Machine.Core
import GHC.Eventlog.Live.Machine.Decoder
import GHC.Eventlog.Live.Machine.Sink
import GHC.Eventlog.Live.Machine.Source
import GHC.Eventlog.Live.Options (EventlogSource (..))
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.RTS.Events (Event)
import Network.Socket qualified as S
import System.IO (Handle)
import System.IO qualified as IO
import Text.Printf (printf)

{- |
Run an event processor with an eventlog socket.
-}
runWithEventlogSource ::
  (MonadUnliftIO m) =>
  -- | The logging verbosity.
  Verbosity ->
  -- | The eventlog socket handle.
  EventlogSource ->
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
runWithEventlogSource verbosity eventlogSocket timeoutExponent initialTimeoutMcs batchIntervalMs maybeChuckSizeBytes maybeOutputFile toEventSink = do
  withEventlogSource verbosity timeoutExponent initialTimeoutMcs eventlogSocket $ \eventlogSource -> do
    let chuckSizeBytes = fromMaybe defaultChunkSizeBytes maybeChuckSizeBytes
    let fromSocket = sourceHandleBatch batchIntervalMs chuckSizeBytes eventlogSource
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
Run an action with a `Handle` to an `EventlogSource`.
-}
withEventlogSource ::
  (MonadUnliftIO m) =>
  -- | The logging verbosity.
  Verbosity ->
  -- | The initial timeout in microseconds for exponential backoff.
  Double ->
  -- | The timeout exponent for exponential backoff.
  Double ->
  -- | The eventlog socket.
  EventlogSource ->
  (Handle -> m ()) ->
  m ()
withEventlogSource verbosity initialTimeoutMcs timeoutExponent eventlogSource action = do
  withRunInIO $ \runInIO ->
    case eventlogSource of
      EventlogStdin -> do
        logInfo verbosity "withEventlogSource" "Reading eventlog from stdin"
        let enter = do
              maybeStdinTextEncoding <- IO.hGetEncoding IO.stdin
              IO.hSetBinaryMode IO.stdin True
              pure maybeStdinTextEncoding
        let leave maybeStdinTextEncoding = do
              traverse_ (IO.hSetEncoding IO.stdin) maybeStdinTextEncoding
              IO.hSetNewlineMode IO.stdin IO.nativeNewlineMode
        E.bracket enter leave . const . runInIO . action $ IO.stdin
      EventlogFile eventlogFile -> do
        logInfo verbosity "withEventlogSource" $ "Reading eventlog from " <> T.pack eventlogFile
        IO.withBinaryFile eventlogFile IO.ReadMode $ \handle ->
          runInIO $ action handle
      EventlogSocketUnix eventlogSocketUnix -> do
        logInfo verbosity "withEventlogSource" $ "Waiting to connect on " <> prettyEventlogSocketUnix eventlogSocketUnix
        E.bracket (connectRetry verbosity initialTimeoutMcs timeoutExponent eventlogSocketUnix) IO.hClose $ \handle ->
          runInIO $ action handle

{- |
Connect to an `EventlogSource` with retries and non-randomised exponential backoff.
-}
connectRetry ::
  -- | The logging verbosity.
  Verbosity ->
  -- | The initial timeout in microseconds for exponential backoff.
  Double ->
  -- | The timeout exponent for exponential backoff.
  Double ->
  -- | The eventlog socket.
  FilePath ->
  IO Handle
connectRetry verbosity initialTimeoutMcs timeoutExponent eventlogSocketUnix =
  connectLoop initialTimeoutMcs
 where
  waitFor :: Double -> IO ()
  waitFor timeoutMcs = threadDelay $ round $ timeoutMcs * 1_000_000

  connectLoop :: Double -> IO Handle
  connectLoop timeoutMcs = do
    let connect = do
          logDebug verbosity "connectRetry" $ "Trying to connect on " <> prettyEventlogSocketUnix eventlogSocketUnix
          handle <- tryConnect eventlogSocketUnix
          logInfo verbosity "connectRetry" $ "Connected on " <> prettyEventlogSocketUnix eventlogSocketUnix
          pure handle
    let cleanup (e :: E.IOException) = do
          logDebug verbosity "connectRetry" $ "Failed to connect on " <> prettyEventlogSocketUnix eventlogSocketUnix <> ": " <> T.pack (displayException e)
          logDebug verbosity "connectRetry" $ "Waiting " <> prettyTimeoutMcs timeoutMcs <> " to retry..."
          waitFor timeoutMcs
          connectLoop (timeoutMcs * timeoutExponent)
    E.catch connect cleanup

{- |
Try to connect to a Unix socket.
-}
tryConnect :: FilePath -> IO Handle
tryConnect eventlogSocketUnix =
  E.bracketOnError (S.socket S.AF_UNIX S.Stream S.defaultProtocol) S.close $ \socket -> do
    S.connect socket (S.SockAddrUnix eventlogSocketUnix)
    handle <- S.socketToHandle socket IO.ReadMode
    IO.hSetBuffering handle IO.NoBuffering
    pure handle

{- |
Interal helper. Pretty-printer for timeout values in microseconds.
-}
prettyTimeoutMcs :: Double -> Text
prettyTimeoutMcs timeoutMcs
  | timeoutMcs > 8.64e10 = T.pack $ printf "%.2f days" (timeoutMcs / 8.64e10)
  | timeoutMcs > 3.6e9 = T.pack $ printf "%.2f hours" (timeoutMcs / 3.6e9)
  | timeoutMcs > 6e7 = T.pack $ printf "%.2f minutes" (timeoutMcs / 6e7)
  | timeoutMcs > 1e6 = T.pack $ printf "%.2f seconds" (timeoutMcs / 1e6)
  | timeoutMcs > 1e3 = T.pack $ printf "%.2f milliseconds" (timeoutMcs / 1e3)
  | otherwise = T.pack $ printf "%.2f microseconds" timeoutMcs

{- |
Internal helper. Pretty-printer for eventlog sockets.
-}
prettyEventlogSocketUnix :: FilePath -> Text
prettyEventlogSocketUnix eventlogSocketUnix = "Unix socket " <> T.pack eventlogSocketUnix
