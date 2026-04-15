{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otelcol.Control (
  ControlOptions (..),
  ControlPort (..),
  ControlCors (..),
  ControlCorsAllowOrigin (..),
  ControlServerApi (..),
  controlOptionsParser,
  startControlServer,
) where

import Control.Applicative (asum)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace)
import Data.Maybe (isJust)
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Otelcol.Config (ServiceName)
import GHC.Eventlog.Live.Source.Core (EventlogSourceHandle (..))
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra.Feature (Feature (..))
import Options.Applicative.Extra.Feature qualified as OF
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

#ifdef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (Exception (..), catches)
import Control.Exception qualified as E (Handler (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSON (..), ToJSON (..), Parser, Value, genericParseJSON, genericToJSON)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isLower, isUpper, toLower)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (writeLog)
import GHC.Eventlog.Live.Otelcol.Config (ServiceName (..))
import GHC.Eventlog.Socket.Control qualified as C
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy qualified as SBSL
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), Origin, cors)
import Network.Wai.Middleware.RequestLogger (Destination (..), DetailedSettings (..), OutputFormat (..), RequestLoggerSettings (..), defaultDetailedSettings, defaultRequestLoggerSettings, mkRequestLogger)
import Servant (PlainText, throwError)
import Servant.API (Capture, FormUrlEncoded, Get, Header, Headers, JSON, NoContent (..), PostAccepted, ReqBody, StdMethod (..), UVerb, Union, WithStatus (..), addHeader, (:>), type (:<|>) (..))
import Servant.Server (Handler, Server, ServerError (..), serve, respond)
import System.Log.FastLogger (fromLogStr)
import Web.FormUrlEncoded (Form, FormOptions, FromForm (..), genericFromForm)
import Web.FormUrlEncoded qualified as Form (FormOptions (fieldLabelModifier), defaultFormOptions)
#else
import Control.Monad (when)
#endif

--------------------------------------------------------------------------------
-- Feature: control
--------------------------------------------------------------------------------

control :: Feature
control = Feature{flag = "control", isOn = isOn, info = "Cannot start control server."}
 where
  isOn :: Bool
#ifdef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL
  isOn = True
#else
  isOn = False
#endif

--------------------------------------------------------------------------------
-- Control App
--------------------------------------------------------------------------------

data ControlServerApi = ControlServerApi
  { notifyNewConnection :: ServiceName -> EventlogSourceHandle -> IO ()
  , notifyEndConnection :: ServiceName -> IO ()
  , stop :: IO ()
  }

noopControlServerApi :: ControlServerApi
noopControlServerApi =
  ControlServerApi
    { notifyNewConnection = \_serviceName _eventlogSourceHandle -> pure ()
    , notifyEndConnection = \_serviceName -> pure ()
    , stop = pure ()
    }

{- |
If the binary is built with -f+control and the control server is enabled from
the CLI, this starts the control server and returns the control server API.

If the binary is NOT built with -f+control, but the control server is enabled
from the CLI, this prints an error and exits the process.

If the control server is NOT enabled from the CLI, this returns a no-op API.
-}
startControlServer :: Logger IO -> ControlOptions -> IO ControlServerApi

--------------------------------------------------------------------------------
-- Control App - Disabled
--------------------------------------------------------------------------------
#ifndef EVENTLOG_LIVE_OTELCOL_FEATURE_CONTROL

startControlServer logger controlOptions = do
  when (shouldStart controlOptions) $
    OF.exitIfUnsupported control logger
  pure noopControlServerApi

--------------------------------------------------------------------------------
-- Control App - Enabled
--------------------------------------------------------------------------------
#else

startControlServer logger controlOptions
  | shouldStart controlOptions = do
    -- Determine the control port
    let port = maybe 30179 (.port) controlOptions.controlPort

    writeLog logger INFO $
      "Starting control server on " <> T.pack (show port)
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
    let !corsResourcePolicy = mkCorsResourcePolicy controlOptions.controlCors

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
  | otherwise =
    pure noopControlServerApi

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
      Just (ControlCorsAllowOriginList origins) -> Just (origins, corsCredentials)
      _otherwise -> Nothing

  -- The @Vary: Origin@ header must be added if @Access-Control-Allow-Origin@
  -- is not set to wildcard and there are multiple origins.
  corsVaryOrigin :: Bool
  corsVaryOrigin =
    case controlCorsAllowOrigin of
      Just (ControlCorsAllowOriginList origins) -> length origins >= 2
      _otherwise -> False

--------------------------------------------------------------------------------
-- Control Server

controlServer :: Logger IO -> TVar (HashMap ServiceName EventlogSourceHandle) -> Bool -> Server (HealthApi :<|> ControlApi)
controlServer logger eventlogSourceHandleMapVar corsIgnoreFailures =
  health :<|> controlApi
 where
  health :: Handler ()
  health = do
    liftIO . writeLog logger DEBUG $
      "Received request on /health."

  controlApi :: Server ControlApi
  controlApi = eventlogSocket :<|> customCommand
   where
    customCommand :: Server CustomCommandApi
    customCommand namespaceText commandId = callCustomCommand :<|> badCORSPreflight
     where
      callCustomCommand :: CustomCommandReq -> Handler (Union '[CustomCommandAccept, CustomCommandReject])
      callCustomCommand req = do
        liftIO . writeLog logger DEBUG $
          "Received request on /control/" <> namespaceText <> " with command ID " <> T.pack (show commandId) <> " for " <> req.serviceName <> "."
        -- Construct the user namespace.
        liftIO (eitherUserNamespace namespaceText) >>= \case
          -- If userNamespace throws an exception, then...
          Left customCommandError ->
            -- ...respond with a custom command error.
            respond . WithStatus @404 $
              customCommandError
          -- Otherwise, ...
          Right namespace -> do
            -- ...construct the user command...
            let command = C.userCommand namespace (C.CommandId commandId)
            -- ...send the user command...
            withSocketFor (ServiceName req.serviceName) $ \socket -> do
              liftIO (SBSL.sendAll socket $ B.encode command)
            -- ...respond with a success status.
            respond . WithStatus @204 $
              NoContent

    eventlogSocket :: Server EventlogSocketApi
    eventlogSocket =
        (startProfiling :<|> badCORSPreflight)
        :<|> (stopProfiling :<|> badCORSPreflight)
        :<|> (startHeapProfiling :<|> badCORSPreflight)
        :<|> (stopHeapProfiling :<|> badCORSPreflight)
        :<|> (requestHeapCensus :<|> badCORSPreflight)
     where
      startProfiling :: StartProfilingReq -> Handler ()
      startProfiling req = do
        liftIO . writeLog logger DEBUG $
          "Received request on /control/eventlog-socket/start-profiling for " <> req.serviceName <> "."
        -- Send the control command over the socket.
        withSocketFor (ServiceName req.serviceName) $ \socket -> do
          liftIO (SBSL.sendAll socket $ B.encode C.startProfiling)

      stopProfiling :: StopProfilingReq -> Handler ()
      stopProfiling req = do
        liftIO . writeLog logger DEBUG $
          "Received request on /control/eventlog-socket/stop-profiling for " <> req.serviceName <> "."
        -- Send the control command over the socket.
        withSocketFor (ServiceName req.serviceName) $ \socket -> do
          liftIO (SBSL.sendAll socket $ B.encode C.stopProfiling)

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
  "control" :> (EventlogSocketApi :<|> CustomCommandApi)

--------------------------------------------------------------------------------
-- Health API

type HealthApi =
  "health" :> Get '[JSON] ()

--------------------------------------------------------------------------------
-- Custom Command API

type CustomCommandApi =
  Capture "namespace" Text
    :> Capture "commandId" Word8
      :> ((ReqBody '[FormUrlEncoded, JSON] CustomCommandReq
        :> UVerb 'POST '[JSON] '[CustomCommandAccept, CustomCommandReject]) :<|> BadCORSPreflight)

type CustomCommandAccept = WithStatus 204 NoContent

type CustomCommandReject = WithStatus 404 CustomCommandError

newtype CustomCommandError = CustomCommandError
  { errorMessage :: String
  }
  deriving (Generic, Show)

eitherUserNamespace :: Text -> IO (Either CustomCommandError C.Namespace)
eitherUserNamespace namespace =
  (pure . Right . C.userNamespace $ namespace) `catches` handlers
 where
  handlers =
    [ E.Handler $ pure . Left . toCustomCommandError @C.NamespaceReservedError
    , E.Handler $ pure . Left . toCustomCommandError @C.NamespaceTooLongError
    ]

toCustomCommandError :: (Exception e) => e -> CustomCommandError
toCustomCommandError = CustomCommandError . displayException

instance ToJSON CustomCommandError where
  toJSON :: CustomCommandError -> Value
  toJSON = genericToJSON myJSONOptions

--------------------------------------------------------------------------------
-- CustomCommandReq

data CustomCommandReq = CustomCommandReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm CustomCommandReq where
  fromForm :: Form -> Either Text CustomCommandReq
  fromForm = genericFromForm myFormOptions

instance FromJSON CustomCommandReq where
  parseJSON :: Value -> Parser CustomCommandReq
  parseJSON = genericParseJSON myJSONOptions

--------------------------------------------------------------------------------
-- Eventlog Socket API

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
    :> ("start-profiling" :> (StartProfilingApi :<|> BadCORSPreflight)
          :<|> "stop-profiling" :> (StopProfilingApi :<|> BadCORSPreflight)
          :<|> "start-heap-profiling" :> (StartHeapProfilingApi :<|> BadCORSPreflight)
          :<|> "stop-heap-profiling" :> (StopHeapProfilingApi :<|> BadCORSPreflight)
          :<|> "request-heap-census" :> (RequestHeapCensusApi :<|> BadCORSPreflight)
       )

type StartProfilingApi =
  ReqBody '[FormUrlEncoded, JSON] StartProfilingReq
    :> PostAccepted '[JSON] ()

type StopProfilingApi =
  ReqBody '[FormUrlEncoded, JSON] StopProfilingReq
    :> PostAccepted '[JSON] ()

type StartHeapProfilingApi =
  ReqBody '[FormUrlEncoded, JSON] StartHeapProfilingReq
    :> PostAccepted '[JSON] ()

type StopHeapProfilingApi =
  ReqBody '[FormUrlEncoded, JSON] StopHeapProfilingReq
    :> PostAccepted '[JSON] ()

type RequestHeapCensusApi =
  ReqBody '[FormUrlEncoded, JSON] RequestHeapCensusReq
    :> PostAccepted '[JSON] ()

type BadCORSPreflight =
  UVerb 'OPTIONS '[PlainText] '[BadCORSPreflightAccept, BadCORSPreflightReject]

type BadCORSPreflightAccept = WithStatus 204 (Headers
    '[ Header "Access-Control-Allow-Origin" String
     , Header "Access-Control-Allow-Methods" String
     , Header "Access-Control-Allow-Headers" String
     ]
    NoContent)

type BadCORSPreflightReject =  WithStatus 400 NoContent

--------------------------------------------------------------------------------
-- StartProfilingReq

newtype StartProfilingReq = StartProfilingReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm StartProfilingReq where
  fromForm :: Form -> Either Text StartProfilingReq
  fromForm = genericFromForm myFormOptions

instance FromJSON StartProfilingReq where
  parseJSON :: Value -> Parser StartProfilingReq
  parseJSON = genericParseJSON myJSONOptions

--------------------------------------------------------------------------------
-- StopProfilingReq

newtype StopProfilingReq = StopProfilingReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm StopProfilingReq where
  fromForm :: Form -> Either Text StopProfilingReq
  fromForm = genericFromForm myFormOptions

instance FromJSON StopProfilingReq where
  parseJSON :: Value -> Parser StopProfilingReq
  parseJSON = genericParseJSON myJSONOptions

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

--------------------------------------------------------------------------------
-- Control Options
--------------------------------------------------------------------------------

data ControlOptions = ControlOptions
  { controlEnabled :: !Bool
  , controlPort :: !(Maybe ControlPort)
  , controlCors :: !ControlCors
  }

{- |
Internal helper.

If the user provides any of the control options, this implies @--control@.
-}
shouldStart :: ControlOptions -> Bool
shouldStart controlOptions =
  controlOptions.controlEnabled
    || isJust controlOptions.controlPort
    || isJust controlOptions.controlCors.controlCorsAllowOrigin
    || isJust controlOptions.controlCors.controlCorsMaxAgeS
    || controlOptions.controlCors.controlCorsRequireOrigin
    || controlOptions.controlCors.controlCorsIgnoreFailures

controlOptionsParser :: O.Parser ControlOptions
controlOptionsParser =
  OC.parserOptionGroup "Control Server Options" $
    ControlOptions
      <$> controlEnabledParser
      <*> controlPortParser
      <*> controlCorsParser

controlEnabledParser :: O.Parser Bool
controlEnabledParser =
  OF.onlyFor control (O.flag False True) mempty $
    O.long "control"
      <> OF.helpFor control "Start the control server."

newtype ControlPort = ControlPort {port :: Int}
  deriving (Eq, Show)

controlPortParser :: O.Parser (Maybe ControlPort)
controlPortParser =
  asum
    [ OF.onlyFor control (O.option (Just . ControlPort <$> O.auto)) (O.metavar "PORT") $
        O.long "control-port"
          <> OF.helpFor control "The port number for the control server."
    , pure Nothing
    ]

data ControlCors = ControlCors
  { controlCorsAllowOrigin :: !(Maybe ControlCorsAllowOrigin)
  , controlCorsMaxAgeS :: !(Maybe Int)
  , controlCorsRequireOrigin :: !Bool
  , controlCorsIgnoreFailures :: !Bool
  }

controlCorsParser :: O.Parser ControlCors
controlCorsParser =
  ControlCors
    <$> controlCorsAllowOriginParser
    <*> controlCorsMaxAgeSParser
    <*> controlCorsRequireOriginParser
    <*> controlCorsIgnoreFailuresParser

controlCorsMaxAgeSParser :: O.Parser (Maybe Int)
controlCorsMaxAgeSParser =
  asum
    [ OF.onlyFor control (O.option (Just <$> O.auto)) (O.metavar "SECONDS") $
        O.long "control-cors-max-age"
          <> OF.helpFor control "Set the maximum age of a cached CORS preflight request for the control server CORS policy."
    , pure Nothing
    ]

controlCorsRequireOriginParser :: O.Parser Bool
controlCorsRequireOriginParser =
  OF.onlyFor control (O.flag False True) mempty $
    O.long "control-cors-require-origin"
      <> OF.helpFor control "If enabled, the control server will not accept requests without an Origin header."

controlCorsIgnoreFailuresParser :: O.Parser Bool
controlCorsIgnoreFailuresParser =
  OF.onlyFor control (O.flag False True) mempty $
    O.long "control-cors-ignore-failure"
      <> OF.helpFor control "If enabled, the control server will accept malformed CORS preflight requests."

type ControlCorsOrigin = BSC.ByteString

data ControlCorsAllowOrigin
  = ControlCorsAllowOriginWildcard
  | ControlCorsAllowOriginList [ControlCorsOrigin]

controlCorsAllowOriginParser :: O.Parser (Maybe ControlCorsAllowOrigin)
controlCorsAllowOriginParser =
  asum
    [ OF.onlyFor control (O.option (Just <$> readPReader pControlCorsAllowOrigin)) (O.metavar "ORIGIN") $
        O.long "control-cors-allow-origin"
          <> OF.helpFor control "Set the allowed origins for the control server CORS policy."
    , pure Nothing
    ]

readPReader :: ReadP a -> O.ReadM a
readPReader readP = readSReader (P.readP_to_S readP)

readSReader :: ReadS a -> O.ReadM a
readSReader readS = O.maybeReader $ \str ->
  case readS str of
    [(a, "")] -> Just a
    _otherwise -> Nothing

pControlCorsAllowOrigin :: ReadP ControlCorsAllowOrigin
pControlCorsAllowOrigin =
  P.skipSpaces
    *> asum
      [ -- Wildcard
        ControlCorsAllowOriginWildcard <$ P.char '*' <* P.skipSpaces
      , -- List of origins
        ControlCorsAllowOriginList <$> P.sepBy1 pOrigin (P.char ',' <* P.skipSpaces)
      ]
 where
  -- TODO: The parser for origin could parse the syntax for origins:
  --
  -- Origin: null
  -- Origin: <scheme>://<hostname>
  -- Origin: <scheme>://<hostname>:<port>
  --
  pOrigin :: ReadP ControlCorsOrigin
  pOrigin = BSC.pack <$> P.munch1 (\c -> not (c == ',' || isSpace c)) <* P.skipSpaces
