{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otelcol.Control (
  ControlServerPort (..),
  ControlServerApi (..),
  startControlServer,
) where

import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Otelcol.Options (ServiceName (..))
import GHC.Eventlog.Live.Source.Core (EventlogSourceHandle (..))

#ifdef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary qualified as B
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isLower, isUpper, toLower)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.List qualified as L
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Eventlog.Socket.Control qualified as C (requestHeapCensus, startHeapProfiling, stopHeapProfiling)
import GHC.Generics (Generic)
import Network.HTTP.Types.Header (hOrigin)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy qualified as SBSL
import Network.Wai (Middleware, Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Network.Wai.Middleware.RequestLogger (Destination (..), DetailedSettings (..), OutputFormat (..), RequestLoggerSettings (..), defaultDetailedSettings, defaultRequestLoggerSettings, mkRequestLogger)
import Servant (throwError)
import Servant.API (FormUrlEncoded, Get, JSON, PostAccepted, ReqBody, (:>), type (:<|>) (..))
import Servant.Server (Handler, Server, ServerError (..), serve)
import System.Log.FastLogger (fromLogStr)
import Web.FormUrlEncoded (Form, FormOptions (fieldLabelModifier), FromForm (..), defaultFormOptions, genericFromForm)
#endif

--------------------------------------------------------------------------------
-- Control App
--------------------------------------------------------------------------------

newtype ControlServerPort = ControlServerPort Int
  deriving (Eq, Show)

data ControlServerApi = ControlServerApi
  { notifyNewConnection :: ServiceName -> EventlogSourceHandle -> IO ()
  , notifyEndConnection :: ServiceName -> IO ()
  , stop :: IO ()
  }

startControlServer :: Logger IO -> ControlServerPort -> IO ControlServerApi
#ifdef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
startControlServer = startControlServerIfEnabled
#else
startControlServer = startControlServerIfDisabled
#endif

--------------------------------------------------------------------------------
-- Control App - Disabled
--------------------------------------------------------------------------------

#ifndef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
startControlServerIfDisabled :: Logger IO -> ControlServerPort -> IO ControlServerApi
startControlServerIfDisabled logger _controlServerPort = do
  writeLog logger WARN $
    "This binary was built without support for the control server."
  let notifyNewConnection :: ServiceName -> EventlogSourceHandle -> IO ()
      notifyNewConnection _serviceName _eventlogSourceHandle = pure ()
  let notifyEndConnection :: ServiceName -> IO ()
      notifyEndConnection _serviceName = pure ()
  let stop :: IO ()
      stop = pure ()
  pure ControlServerApi{..}
#endif

--------------------------------------------------------------------------------
-- Control App - Enabled
--------------------------------------------------------------------------------

#ifdef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
startControlServerIfEnabled :: Logger IO -> ControlServerPort -> IO ControlServerApi
startControlServerIfEnabled logger (ControlServerPort port) = do
  -- Create middleware that logs all incoming requests.
  requestLogger <- mkLoggerMiddleware logger

  -- Create variable for storing sockets.
  eventlogSourceHandleMap <- newTVarIO M.empty

  -- Start control server.
  controlServerThreadId <-
    forkIO $
      Warp.run port $
        requestLogger $
          -- TODO: Allow customizable CORS policy.
          cors corsResourcePolicy $
            serve (Proxy @(HealthApi :<|> ControlApi)) $
              controlServer logger eventlogSourceHandleMap

  -- When notified of a new connection, update the eventlogSourceHandleMap.
  let notifyNewConnection serviceName eventlogSourceHandle = do
        writeLog logger DEBUG $
          "New connection for service " <> serviceName.serviceName <> "."
        atomically $
          modifyTVar' eventlogSourceHandleMap (M.insert serviceName eventlogSourceHandle)

  -- When notified of the end of a connection, update the eventlogSourceHandleMap.
  let notifyEndConnection serviceName = do
        writeLog logger DEBUG $
          "End connection for service " <> serviceName.serviceName <> "."
        atomically $
          modifyTVar' eventlogSourceHandleMap (M.delete serviceName)

  -- When requested to stop, kill the control server thread.
  let stop = killThread controlServerThreadId

  pure ControlServerApi{..}

mkLoggerMiddleware :: Logger IO -> IO Middleware
mkLoggerMiddleware logger =
  mkRequestLogger $
    defaultRequestLoggerSettings
      { destination = Callback $ writeLog logger TRACE2 . TE.decodeUtf8Lenient . fromLogStr
      , outputFormat =
          DetailedWithSettings
            defaultDetailedSettings
              { useColors = False
              }
      }

corsResourcePolicy :: Request -> Maybe CorsResourcePolicy
corsResourcePolicy req = do
  origin <- findOrigin req
  pure
    CorsResourcePolicy
      { corsOrigins = Just ([origin], True)
      , corsMethods = ["GET", "POST"]
      , corsRequestHeaders = []
      , corsExposedHeaders = Nothing
      , corsMaxAge = Nothing -- TODO: Pick a timeout.
      , corsVaryOrigin = True
      , corsRequireOrigin = True
      , corsIgnoreFailures = False
      }

findOrigin :: Request -> Maybe ByteString
findOrigin = fmap snd . L.find ((hOrigin ==) . fst) . requestHeaders

--------------------------------------------------------------------------------
-- Control Server

controlServer :: Logger IO -> TVar (HashMap ServiceName EventlogSourceHandle) -> Server (HealthApi :<|> ControlApi)
controlServer logger eventlogSourceHandleMapVar =
  health :<|> eventlogSocket
 where
  health :: Handler ()
  health = do
    liftIO . writeLog logger DEBUG $
      "Received request on /health."

  eventlogSocket :: Server EventlogSocketApi
  eventlogSocket = startHeapProfiling :<|> stopHeapProfiling :<|> requestHeapCensus
   where
    startHeapProfiling :: StartHeapProfilingReq -> Handler ()
    startHeapProfiling req = do
      liftIO . writeLog logger DEBUG $
        "Received request on /control/eventlog-socket/start-heap-profiling for " <> req.serviceName <> "."
      -- Send the control command over the socket.
      withSocketFor (ServiceName req.serviceName) $ \socket -> do
        liftIO (SBSL.sendAll socket $ B.encode C.startHeapProfiling)

    stopHeapProfiling :: StopHeapProfilingReq -> Handler ()
    stopHeapProfiling req = do
      liftIO . writeLog logger DEBUG $
        "Received request on /control/eventlog-socket/stop-heap-profiling for " <> req.serviceName <> "."
      -- Send the control command over the socket.
      withSocketFor (ServiceName req.serviceName) $ \socket -> do
        liftIO (SBSL.sendAll socket $ B.encode C.stopHeapProfiling)

    requestHeapCensus :: RequestHeapCensusReq -> Handler ()
    requestHeapCensus req = do
      liftIO . writeLog logger DEBUG $
        "Received request on /control/eventlog-socket/request-heap-census for " <> req.serviceName <> "."
      -- Send the control command over the socket.
      withSocketFor (ServiceName req.serviceName) $ \socket -> do
        liftIO (SBSL.sendAll socket $ B.encode C.requestHeapCensus)

    withSocketFor :: ServiceName -> (Socket -> Handler ()) -> Handler ()
    withSocketFor serviceName action = do
      eventlogSourceHandleMap <- liftIO (readTVarIO eventlogSourceHandleMapVar)
      case M.lookup serviceName eventlogSourceHandleMap of
        -- If the service is not known, return a 404.
        Nothing ->
          throwError
            ServerError
              { errHTTPCode = 404
              , errReasonPhrase = "Not Found"
              , errBody = BSL.fromStrict . TE.encodeUtf8 $ "Could not find eventlog socket for service " <> serviceName.serviceName <> "."
              , errHeaders = []
              }
        -- If the service telemetry is streamed from stdin or a file, return a 400.
        Just EventlogSourceHandleStdin ->
          throwError
            ServerError
              { errHTTPCode = 400
              , errReasonPhrase = "Bad Request"
              , errBody = BSL.fromStrict . TE.encodeUtf8 $ "The telemetry for service " <> serviceName.serviceName <> " is streamed from stdin."
              , errHeaders = []
              }
        Just (EventlogSourceHandleFile _h) ->
          throwError
            ServerError
              { errHTTPCode = 400
              , errReasonPhrase = "Bad Request"
              , errBody = BSL.fromStrict . TE.encodeUtf8 $ "The telemetry for service " <> serviceName.serviceName <> " is streamed from a file."
              , errHeaders = []
              }
        -- If the service telemetry is streamed from a socket, continue.
        Just (EventlogSourceHandleSocketUnix s) -> action s

--------------------------------------------------------------------------------
-- Control API

type ControlApi =
  "control" :> EventlogSocketApi

type HealthApi =
  "health" :> Get '[JSON] ()

-- 2025-12-11:
-- Ideally, the /heap-profiling API would attach the semantics of /start and
-- /stop to a PUT and DELETE request on /heap-profiling with 204 No Content
-- responses. This would give us exactly the right caching semantics.
-- However, I have not been able to find any Grafana plugins that support
-- sending PUT and DELETE requests, and servant appears to have trouble adding
-- headers to 204 No Content responses, so I'm using POST requests on separate
-- /start and /stop endpoints.

type EventlogSocketApi =
  "eventlog-socket"
    :> ( "start-heap-profiling" :> StartHeapProfilingApi
          :<|> "stop-heap-profiling" :> StopHeapProfilingApi
          :<|> "request-heap-census" :> RequestHeapCensusApi
       )

type StartHeapProfilingApi =
  ReqBody '[FormUrlEncoded] StartHeapProfilingReq
    :> PostAccepted '[JSON] ()

type StopHeapProfilingApi =
  ReqBody '[FormUrlEncoded] StopHeapProfilingReq
    :> PostAccepted '[JSON] ()

type RequestHeapCensusApi =
  ReqBody '[FormUrlEncoded] RequestHeapCensusReq
    :> PostAccepted '[JSON] ()

--------------------------------------------------------------------------------
-- Health

data Health = Health
  deriving (Generic, Show)

--------------------------------------------------------------------------------
-- StartHeapProfilingReq

newtype StartHeapProfilingReq = StartHeapProfilingReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm StartHeapProfilingReq where
  fromForm :: Form -> Either Text StartHeapProfilingReq
  fromForm = genericFromForm myFormOptions

--------------------------------------------------------------------------------
-- StopHeapProfilingReq

newtype StopHeapProfilingReq = StopHeapProfilingReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm StopHeapProfilingReq where
  fromForm :: Form -> Either Text StopHeapProfilingReq
  fromForm = genericFromForm myFormOptions

--------------------------------------------------------------------------------
-- RequestHeapCensusReq

newtype RequestHeapCensusReq = RequestHeapCensusReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm RequestHeapCensusReq where
  fromForm :: Form -> Either Text RequestHeapCensusReq
  fromForm = genericFromForm myFormOptions

myFormOptions :: FormOptions
myFormOptions =
  defaultFormOptions
    { fieldLabelModifier = camelTo2 '-'
    }

--------------------------------------------------------------------------------
-- Internal helpers.

-- | Taken from aeson.
camelTo2 :: Char -> String -> String
camelTo2 c = map toLower . go2 . go1
 where
  go1 "" = ""
  go1 (x : u : l : xs) | isUpper u && isLower l = x : c : u : l : go1 xs
  go1 (x : xs) = x : go1 xs
  go2 "" = ""
  go2 (l : u : xs) | isLower l && isUpper u = l : c : u : go2 xs
  go2 (x : xs) = x : go2 xs
#endif
