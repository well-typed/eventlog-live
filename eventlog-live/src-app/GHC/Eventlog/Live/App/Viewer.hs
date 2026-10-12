{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.App.Viewer (
  ViewerServerApi (..),
  startViewerServer,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Exception (SomeException, handle)
import Control.Monad (forever)
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import GHC.Eventlog.Live.Logger (Logger, logException, logInfo, logTrace)
import GHC.Eventlog.Live.Machine.Core (Tick (..))
import GHC.Eventlog.Live.Processor (ExportRequest)
import Network.WebSockets qualified as WS

newtype ViewerServerApi = ViewerServerApi
  { stop :: IO ()
  }

startViewerServer :: Logger IO -> Chan (Tick ExportRequest) -> IO ViewerServerApi
startViewerServer logger exportRequestChan = do
  logInfo logger $ "Starting viewer server"

  threadId <-
    forkIO $
      handle (logException @SomeException logger) $
        WS.runServer "127.0.0.1" 30180 $
          viewerApp logger exportRequestChan

  let stop :: IO ()
      stop = killThread threadId

  pure ViewerServerApi{..}

viewerApp ::
  Logger IO -> Chan (Tick ExportRequest) -> WS.ServerApp
viewerApp logger exportRequestChan pending = do
  conn <- WS.acceptRequest pending
  chan <- dupChan exportRequestChan

  let handleConnectionException :: WS.ConnectionException -> IO ()
      handleConnectionException = \case
        WS.CloseRequest{} ->
          logInfo logger $ "Viewer requested to close the connection."
        WS.ConnectionClosed{} ->
          logInfo logger $ "Viewer closed the connection."
        wsErr -> logException logger wsErr

  let viewer :: IO ()
      viewer =
        forever $
          readChan chan >>= \case
            Tick ->
              pure ()
            Item er -> do
              let erBytes = JSON.encode er
              let erByteSize = BSL.length erBytes
              logTrace logger $ "Export " <> T.show erByteSize <> " bytes to viewer."
              WS.sendTextData conn erBytes

  handle handleConnectionException viewer
