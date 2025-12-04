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
import Data.Foldable (traverse_)
import Data.Machine (ProcessT, runT_, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Machine.Core
import GHC.Eventlog.Live.Machine.Decoder
import GHC.Eventlog.Live.Machine.Sink
import GHC.Eventlog.Live.Machine.Source
import GHC.Eventlog.Live.Options (EventlogSource (..))
import GHC.RTS.Events (Event)
import Network.Socket qualified as S
import System.IO (Handle)
import System.IO qualified as IO
import Text.Printf (printf)

{- |
Run an event processor with an eventlog socket.
-}
runWithEventlogSource ::
  -- | The logging action.
  Logger IO ->
  -- | The eventlog socket handle.
  EventlogSource ->
  -- | The initial timeout in seconds for exponential backoff.
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
  ProcessT IO (Tick Event) Void ->
  IO ()
runWithEventlogSource logger eventlogSocket timeoutExponent initialTimeoutS batchIntervalMs maybeChuckSizeBytes maybeOutputFile toEventSink = do
  withEventlogSource logger timeoutExponent initialTimeoutS eventlogSocket $ \eventlogSource -> do
    let chuckSizeBytes = fromMaybe defaultChunkSizeBytes maybeChuckSizeBytes
    let fromSocket = sourceHandleBatch batchIntervalMs chuckSizeBytes eventlogSource
    case maybeOutputFile of
      Nothing ->
        runT_ $
          fromSocket ~> decodeEventBatch logger ~> toEventSink
      Just outputFile ->
        IO.withFile outputFile IO.WriteMode $ \outputHandle -> do
          runT_ $
            fromSocket
              ~> fanout
                [ fileSinkBatch outputHandle
                , decodeEventBatch logger ~> toEventSink
                ]

{- |
Run an action with a `Handle` to an `EventlogSource`.
-}
withEventlogSource ::
  -- | The logging action.
  Logger IO ->
  -- | The initial timeout in seconds for exponential backoff.
  Double ->
  -- | The timeout exponent for exponential backoff.
  Double ->
  -- | The eventlog socket.
  EventlogSource ->
  (Handle -> IO ()) ->
  IO ()
withEventlogSource logger initialTimeoutS timeoutExponent eventlogSource action = do
  case eventlogSource of
    EventlogStdin -> do
      writeLog logger INFO $
        "Reading eventlog from stdin"
      let enter = do
            maybeStdinTextEncoding <- IO.hGetEncoding IO.stdin
            IO.hSetBinaryMode IO.stdin True
            pure maybeStdinTextEncoding
      let leave maybeStdinTextEncoding = do
            traverse_ (IO.hSetEncoding IO.stdin) maybeStdinTextEncoding
            IO.hSetNewlineMode IO.stdin IO.nativeNewlineMode
      E.bracket enter leave . const . action $ IO.stdin
    EventlogFile eventlogFile -> do
      writeLog logger INFO $
        "Reading eventlog from " <> T.pack eventlogFile
      IO.withBinaryFile eventlogFile IO.ReadMode $ \handle ->
        action handle
    EventlogSocketUnix eventlogSocketUnix -> do
      writeLog logger INFO $
        "Waiting to connect on " <> prettyEventlogSocketUnix eventlogSocketUnix
      E.bracket (connectRetry logger initialTimeoutS timeoutExponent eventlogSocketUnix) IO.hClose $ \handle ->
        action handle

{- |
Connect to an `EventlogSource` with retries and non-randomised exponential backoff.
-}
connectRetry ::
  -- | The logging action.
  Logger IO ->
  -- | The initial timeout in seconds for exponential backoff.
  Double ->
  -- | The timeout exponent for exponential backoff.
  Double ->
  -- | The eventlog socket.
  FilePath ->
  IO Handle
connectRetry logger initialTimeoutS timeoutExponent eventlogSocketUnix =
  connectLoop initialTimeoutS
 where
  waitFor :: Double -> IO ()
  waitFor timeoutS = threadDelay $ round $ timeoutS * 1e6

  connectLoop :: Double -> IO Handle
  connectLoop timeoutS = do
    let connect = do
          writeLog logger DEBUG $
            "Trying to connect on " <> prettyEventlogSocketUnix eventlogSocketUnix
          handle <- tryConnect eventlogSocketUnix
          writeLog logger DEBUG $
            "Connected on " <> prettyEventlogSocketUnix eventlogSocketUnix
          pure handle
    let cleanup (e :: E.IOException) = do
          writeLog logger DEBUG $
            "Failed to connect on "
              <> prettyEventlogSocketUnix eventlogSocketUnix
              <> ": "
              <> T.pack (displayException e)
          writeLog logger DEBUG $
            "Waiting "
              <> prettyTimeoutMcs timeoutS
              <> " to retry..."
          waitFor timeoutS
          connectLoop (timeoutS * timeoutExponent)
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
prettyTimeoutMcs timeoutS
  | timeoutS > 86400 = T.pack $ printf "%.2f days" (timeoutS / 86400)
  | timeoutS > 3600 = T.pack $ printf "%.2f hours" (timeoutS / 3600)
  | timeoutS > 60 = T.pack $ printf "%.2f minutes" (timeoutS / 60)
  | timeoutS > 1 = T.pack $ printf "%.2f seconds" timeoutS
  | timeoutS > 1e-3 = T.pack $ printf "%.2f milliseconds" (timeoutS / 1e-3)
  | timeoutS > 1e-6 = T.pack $ printf "%.2f microseconds" (timeoutS / 1e-6)
  | timeoutS > 1e-9 = T.pack $ printf "%.2f nanoseconds" (timeoutS / 1e-9)
  | otherwise = T.pack $ printf "%.2f seconds" timeoutS

{- |
Internal helper. Pretty-printer for eventlog sockets.
-}
prettyEventlogSocketUnix :: FilePath -> Text
prettyEventlogSocketUnix eventlogSocketUnix = "Unix socket " <> T.pack eventlogSocketUnix
