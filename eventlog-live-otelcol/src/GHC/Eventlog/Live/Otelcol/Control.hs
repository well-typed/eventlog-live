{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otelcol.Control (
  ControlServerApi (..),
  startControlServer,
) where

import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Otelcol.Options (ControlOptions, ServiceName (..))
import GHC.Eventlog.Live.Source.Core (EventlogSourceHandle (..))

#ifdef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSON (..), Parser, Value, genericParseJSON)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isLower, isUpper, toLower)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Eventlog.Live.Otelcol.Options (ControlOptions (..), ControlPort (..), ControlCors(..), ControlCorsAllowOrigin(..))
import GHC.Eventlog.Socket.Control qualified as C (requestHeapCensus, startHeapProfiling, stopHeapProfiling)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy qualified as SBSL
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), Origin, cors)
import Network.Wai.Middleware.RequestLogger (Destination (..), DetailedSettings (..), OutputFormat (..), RequestLoggerSettings (..), defaultDetailedSettings, defaultRequestLoggerSettings, mkRequestLogger)
import Servant (PlainText, throwError)
import Servant.API (FormUrlEncoded, Get, Header, Headers, JSON, NoContent (..), PostAccepted, ReqBody, StdMethod (..), UVerb, Union, WithStatus (..), addHeader, (:>), type (:<|>) (..))
import Servant.Server (Handler, Server, ServerError (..), serve, respond)
import System.Log.FastLogger (fromLogStr)
import Web.FormUrlEncoded (Form, FormOptions, FromForm (..), genericFromForm)
import Web.FormUrlEncoded qualified as Form (FormOptions (fieldLabelModifier), defaultFormOptions)
#endif

--------------------------------------------------------------------------------
-- Control App
--------------------------------------------------------------------------------

data ControlServerApi = ControlServerApi
  { notifyNewConnection :: ServiceName -> EventlogSourceHandle -> IO ()
  , notifyEndConnection :: ServiceName -> IO ()
  , stop :: IO ()
  }

startControlServer :: Logger IO -> ControlOptions -> IO ControlServerApi
#ifdef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
startControlServer = startControlServerIfEnabled
#else
startControlServer = startControlServerIfDisabled
#endif

--------------------------------------------------------------------------------
-- Control App - Disabled
--------------------------------------------------------------------------------

#ifndef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
startControlServerIfDisabled :: Logger IO -> ControlOptions -> IO ControlServerApi
startControlServerIfDisabled logger _controlOptions = do
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
startControlServerIfEnabled :: Logger IO -> ControlOptions -> IO ControlServerApi
startControlServerIfEnabled logger ControlOptions{controlPort = ControlPort port, ..} = do
  -- Create middleware that logs all incoming requests.
  requestLogger <- mkLoggerMiddleware logger

  -- Create variable for storing sockets.
  eventlogSourceHandleMap <- newTVarIO M.empty

  -- Create CORS resource policy.
  --
  -- NOTE: The @wai-cors@ package lets the resource policy depend dynamically
  -- on the received request. However, it is difficult to expose this freedom
  -- via command-line arguments.
  --
  -- TODO: We might have to manually find the origin in the request, check it
  -- against the allow-list, and set the @corsOrigins@ field.
  let !corsResourcePolicy = mkCorsResourcePolicy controlCors

  -- Start control server.
  controlServerThreadId <-
    forkIO $
      Warp.run port $
        requestLogger $
          cors (const $ Just corsResourcePolicy) $
            serve (Proxy @(HealthApi :<|> ControlApi)) $
              controlServer logger eventlogSourceHandleMap (corsIgnoreFailures corsResourcePolicy)

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

mkCorsResourcePolicy :: ControlCors -> CorsResourcePolicy
mkCorsResourcePolicy ControlCors{..} =
  CorsResourcePolicy
    { corsOrigins = corsOrigins
      , corsMethods = ["GET", "POST"]
      , corsRequestHeaders = ["Content-Type", "Origin"]
      , corsExposedHeaders = Nothing
      , corsMaxAge = controlCorsMaxAgeS
      , corsVaryOrigin = corsVaryOrigin
      , corsRequireOrigin = controlCorsRequireOrigin
      , corsIgnoreFailures = controlCorsIgnoreFailures
      }
 where
  -- TODO: If we add authentication, this flag needs to be set.
  corsCredentials :: Bool
  corsCredentials = False

  -- The @wai-cors@ package represents wildcard as @Nothing@ and otherwise
  -- accepts a list of origins and a boolean that determines whether or not
  -- credentials are used to access the resource.
  corsOrigins :: Maybe ([Origin], Bool)
  corsOrigins =
    case controlCorsAllowOrigin of
      ControlCorsAllowOriginWildcard -> Nothing
      ControlCorsAllowOriginList origins -> Just (origins, corsCredentials)

  -- The @Vary: Origin@ header must be added if @Access-Control-Allow-Origin@
  -- is not set to wildcard and there are multiple origins.
  corsVaryOrigin :: Bool
  corsVaryOrigin =
    case controlCorsAllowOrigin of
      ControlCorsAllowOriginWildcard -> False
      ControlCorsAllowOriginList origins -> length origins >= 2

--------------------------------------------------------------------------------
-- Control Server

controlServer :: Logger IO -> TVar (HashMap ServiceName EventlogSourceHandle) -> Bool -> Server (HealthApi :<|> ControlApi)
controlServer logger eventlogSourceHandleMapVar corsIgnoreFailures =
  health :<|> eventlogSocket
 where
  health :: Handler ()
  health = do
    liftIO . writeLog logger DEBUG $
      "Received request on /health."

  eventlogSocket :: Server EventlogSocketApi
  eventlogSocket =
    (startHeapProfiling :<|> badCORSPreflight)
      :<|> (stopHeapProfiling :<|> badCORSPreflight)
      :<|> (requestHeapCensus :<|> badCORSPreflight)
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

    badCORSPreflight :: Handler (Union '[BadCORSPreflightAccept, BadCORSPreflightReject])
    badCORSPreflight
      | corsIgnoreFailures = do
          -- This accepts malformed CORS preflight requests.
          liftIO . writeLog logger DEBUG $ "Accepted malformed CORS preflight request."
          respond . WithStatus @204 $
            addHeader @"Access-Control-Allow-Origin" @String "*" $
              addHeader @"Access-Control-Allow-Methods" @String "GET, POST" $
                addHeader @"Access-Control-Allow-Headers" @String "*" $
                  NoContent
      | otherwise = do
          liftIO . writeLog logger DEBUG $ "Accepted malformed CORS preflight request."
          respond . WithStatus @400 $
            NoContent

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
    :> ( "start-heap-profiling" :> (StartHeapProfilingApi :<|> BadCORSPreflight)
          :<|> "stop-heap-profiling" :> (StopHeapProfilingApi :<|> BadCORSPreflight)
          :<|> "request-heap-census" :> (RequestHeapCensusApi :<|> BadCORSPreflight)
       )

type StartHeapProfilingApi =
  ReqBody '[FormUrlEncoded, JSON] StartHeapProfilingReq
    :> PostAccepted '[JSON] ()
    -- Verb 'POST 200 '[JSON] ()
    -- UVerb 'OPTIONS '[JSON] '[WithStatus 200 (), WithStatus 404 BadOops]

type StopHeapProfilingApi =
  ReqBody '[FormUrlEncoded, JSON] StopHeapProfilingReq
    :> PostAccepted '[JSON] ()

type RequestHeapCensusApi =
  ReqBody '[FormUrlEncoded, JSON] RequestHeapCensusReq
    :> PostAccepted '[JSON] ()

type BadCORSPreflight =
  UVerb 'OPTIONS '[PlainText] '[BadCORSPreflightAccept, BadCORSPreflightReject]

-- "/a/b/c"

type BadCORSPreflightAccept = WithStatus 204 (Headers
    '[ Header "Access-Control-Allow-Origin" String
     , Header "Access-Control-Allow-Methods" String
     , Header "Access-Control-Allow-Headers" String
     ]
    NoContent)
{- (Headers
    '[ Header "Access-Control-Allow-Origin" String
     , Header "Access-Control-Allow-Methods" String
     , Header "Access-Control-Allow-Headers" String
     ]
    NoContent)
-}

type BadCORSPreflightReject =  WithStatus 400 NoContent


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

instance FromJSON StartHeapProfilingReq where
  parseJSON :: Value -> Parser StartHeapProfilingReq
  parseJSON = genericParseJSON myJSONOptions

--------------------------------------------------------------------------------
-- StopHeapProfilingReq

newtype StopHeapProfilingReq = StopHeapProfilingReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm StopHeapProfilingReq where
  fromForm :: Form -> Either Text StopHeapProfilingReq
  fromForm = genericFromForm myFormOptions

instance FromJSON StopHeapProfilingReq where
  parseJSON :: Value -> Parser StopHeapProfilingReq
  parseJSON = genericParseJSON myJSONOptions

--------------------------------------------------------------------------------
-- RequestHeapCensusReq

newtype RequestHeapCensusReq = RequestHeapCensusReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm RequestHeapCensusReq where
  fromForm :: Form -> Either Text RequestHeapCensusReq
  fromForm = genericFromForm myFormOptions

instance FromJSON RequestHeapCensusReq where
  parseJSON :: Value -> Parser RequestHeapCensusReq
  parseJSON = genericParseJSON myJSONOptions

--------------------------------------------------------------------------------
-- Internal helpers.

-- | Generic options for `FromForm`.
myFormOptions :: FormOptions
myFormOptions =
  Form.defaultFormOptions
    { Form.fieldLabelModifier = camelTo2 '-'
    }

-- | Generic options for `FromJSON`.
myJSONOptions :: JSON.Options
myJSONOptions =
  JSON.defaultOptions
    { JSON.fieldLabelModifier = camelTo2 '-'
    }

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
