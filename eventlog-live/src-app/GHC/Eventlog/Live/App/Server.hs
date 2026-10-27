{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.App.Server (
  ServerHandle (..),
  ControlHooks (..),
  startServer,
  ServerOptions (..),
  serverOptionsParser,
)
where

import Control.Applicative (asum)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isSpace)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Eventlog.Live.App.Server.Control (ControlApi, ControlHooks (..), control, newControlServer, newDisabledControlServer)
import GHC.Eventlog.Live.App.Server.Viewer (ViewerApi, viewerServer, viewerWsServerApp)
import GHC.Eventlog.Live.Logger (Logger, logDebug, logInfo, logTrace)
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Processor (ExportRequest)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), Origin, cors)
import Network.Wai.Middleware.RequestLogger qualified as RL
import Network.WebSockets (defaultConnectionOptions)
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra.Feature qualified as OF
import Servant.API (Get, JSON, (:<|>) (..), (:>))
import Servant.Server (Handler, serve)
import System.Log.FastLogger (fromLogStr)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

data ServerHandle = ServerHandle
  { controlHooks :: ControlHooks
  , stop :: IO ()
  }

startServer ::
  Logger IO ->
  ServerOptions ->
  Chan (Tick ExportRequest) ->
  IO ServerHandle
startServer logger ServerOptions{..} exportRequestChan = do
  -- Announce the server
  logInfo logger $ "Starting server on " <> T.show serverPort.port

  -- Create middleware that logs all incoming requests.
  requestLogger <- mkLoggerMiddleware logger

  -- Create CORS resource policy.
  --
  -- NOTE: The @wai-cors@ package lets the resource policy depend dynamically
  -- on the received request. However, it is difficult to expose this freedom
  -- via command-line arguments.
  --
  -- TODO: We might have to manually find the origin in the request, check it
  -- against the allow-list, and set the @corsOrigins@ field.
  let !corsResourcePolicy = mkCorsResourcePolicy serverCors

  -- Create the control server.
  (controlHooks, controlServer) <-
    (if controlEnabled then newControlServer else newDisabledControlServer)
      logger
      corsResourcePolicy.corsIgnoreFailures

  -- Create the viewer websocket server.
  let viewerWebsocketOr app
        | viewerEnabled =
            let server = viewerWsServerApp logger exportRequestChan
                options = defaultConnectionOptions
             in websocketsOr options server app
        | otherwise = app

  -- Start the server.
  serverThreadId <-
    forkIO $
      Warp.run serverPort.port $
        requestLogger $
          cors (const $ Just corsResourcePolicy) $
            viewerWebsocketOr $
              serve (Proxy @ServerApi) $
                controlServer :<|> health logger :<|> viewerServer

  -- Create the stop function.
  let stop = killThread serverThreadId

  pure ServerHandle{..}

--------------------------------------------------------------------------------
-- Server API
--------------------------------------------------------------------------------

type ServerApi =
  ("control" :> ControlApi)
    :<|> ("health" :> HealthApi)
    :<|> ViewerApi

--------------------------------------------------------------------------------
-- Health API
--------------------------------------------------------------------------------

type HealthApi =
  Get '[JSON] ()

health :: Logger IO -> Handler ()
health logger = do
  liftIO . logDebug logger $
    "Received request on /health."

--------------------------------------------------------------------------------
-- Logger Middleware
--------------------------------------------------------------------------------

mkLoggerMiddleware :: Logger IO -> IO Middleware
mkLoggerMiddleware logger =
  RL.mkRequestLogger $
    RL.defaultRequestLoggerSettings
      { RL.destination = RL.Callback $ logTrace logger . TE.decodeUtf8Lenient . fromLogStr
      , RL.outputFormat =
          RL.DetailedWithSettings
            RL.defaultDetailedSettings
              { RL.useColors = False
              }
      }

--------------------------------------------------------------------------------
-- Server Options
--------------------------------------------------------------------------------

data ServerOptions = ServerOptions
  { viewerEnabled :: !Bool
  , controlEnabled :: !Bool
  , serverPort :: !ServerPort
  , serverCors :: !ServerCors
  }

serverOptionsParser :: O.Parser ServerOptions
serverOptionsParser =
  OC.parserOptionGroup "Server options:" $
    ServerOptions
      <$> viewerEnabledParser
      <*> controlEnabledParser
      <*> serverPortParser
      <*> serverCorsParser

--------------------------------------------------------------------------------
-- Server Enabled

viewerEnabledParser :: O.Parser Bool
viewerEnabledParser =
  O.flag False True . mconcat $
    [ O.long "viewer"
    , O.help "Enable the viewer server."
    ]

--------------------------------------------------------------------------------
-- Server Enabled

controlEnabledParser :: O.Parser Bool
controlEnabledParser =
  OF.onlyFor control (O.flag False True) mempty $
    O.long "control"
      <> OF.helpFor control "Enable the control server."

--------------------------------------------------------------------------------
-- Server Port

newtype ServerPort = ServerPort {port :: Int}
  deriving (Eq, Show)

serverPortParser :: O.Parser ServerPort
serverPortParser =
  O.option (ServerPort <$> O.auto) . mconcat $
    [ O.metavar "PORT"
    , O.long "port"
    , O.help "The port number for the server."
    , O.value (ServerPort 30179)
    ]

--------------------------------------------------------------------------------
-- Server CORS

data ServerCors = ServerCors
  { serverCorsAllowOrigin :: !(Maybe ServerCorsAllowOrigin)
  , serverCorsMaxAgeS :: !(Maybe Int)
  , serverCorsRequireOrigin :: !Bool
  , serverCorsIgnoreFailures :: !Bool
  }

serverCorsParser :: O.Parser ServerCors
serverCorsParser =
  ServerCors
    <$> serverCorsAllowOriginParser
    <*> serverCorsMaxAgeSParser
    <*> serverCorsRequireOriginParser
    <*> serverCorsIgnoreFailuresParser

serverCorsMaxAgeSParser :: O.Parser (Maybe Int)
serverCorsMaxAgeSParser =
  O.optional . O.option O.auto . mconcat $
    [ O.metavar "SECONDS"
    , O.long "server-cors-max-age"
    , O.help "Set the maximum age of a cached CORS preflight request for the server CORS policy."
    ]

serverCorsRequireOriginParser :: O.Parser Bool
serverCorsRequireOriginParser =
  O.flag False True . mconcat $
    [ O.long "server-cors-require-origin"
    , O.help "If enabled, the server will not accept requests without an Origin header."
    ]

serverCorsIgnoreFailuresParser :: O.Parser Bool
serverCorsIgnoreFailuresParser =
  O.flag False True . mconcat $
    [ O.long "server-cors-ignore-failure"
    , O.help "If enabled, the control server will accept malformed CORS preflight requests."
    ]

type ServerCorsOrigin = BSC.ByteString

data ServerCorsAllowOrigin
  = ServerCorsAllowOriginWildcard
  | ServerCorsAllowOriginList [ServerCorsOrigin]

serverCorsAllowOriginParser :: O.Parser (Maybe ServerCorsAllowOrigin)
serverCorsAllowOriginParser =
  O.optional . O.option (readPReader pServerCorsAllowOrigin) . mconcat $
    [ O.long "server-cors-allow-origin"
    , O.metavar "ORIGIN"
    , O.help "Set the allowed origins for the control server CORS policy."
    ]

readPReader :: ReadP a -> O.ReadM a
readPReader readP = readSReader (P.readP_to_S readP)

readSReader :: ReadS a -> O.ReadM a
readSReader readS = O.maybeReader $ \str ->
  case readS str of
    [(a, "")] -> Just a
    _otherwise -> Nothing

pServerCorsAllowOrigin :: ReadP ServerCorsAllowOrigin
pServerCorsAllowOrigin =
  P.skipSpaces
    *> asum
      [ -- Wildcard
        ServerCorsAllowOriginWildcard <$ P.char '*' <* P.skipSpaces
      , -- List of origins
        ServerCorsAllowOriginList <$> P.sepBy1 pOrigin (P.char ',' <* P.skipSpaces)
      ]
 where
  -- TODO: The parser for origin could parse the syntax for origins:
  --
  -- Origin: null
  -- Origin: <scheme>://<hostname>
  -- Origin: <scheme>://<hostname>:<port>
  --
  pOrigin :: ReadP ServerCorsOrigin
  pOrigin = BSC.pack <$> P.munch1 (\c -> not (c == ',' || isSpace c)) <* P.skipSpaces

mkCorsResourcePolicy :: ServerCors -> CorsResourcePolicy
mkCorsResourcePolicy ServerCors{..} =
  CorsResourcePolicy
    { corsOrigins = corsOrigins
    , corsMethods = ["GET", "POST"]
    , corsRequestHeaders = ["Content-Type", "Origin"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = serverCorsMaxAgeS
    , corsVaryOrigin = corsVaryOrigin
    , corsRequireOrigin = serverCorsRequireOrigin
    , corsIgnoreFailures = serverCorsIgnoreFailures
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
    case serverCorsAllowOrigin of
      Just (ServerCorsAllowOriginList origins) -> Just (origins, corsCredentials)
      _otherwise -> Nothing

  -- The @Vary: Origin@ header must be added if @Access-Control-Allow-Origin@
  -- is not set to wildcard and there are multiple origins.
  corsVaryOrigin :: Bool
  corsVaryOrigin =
    case serverCorsAllowOrigin of
      Just (ServerCorsAllowOriginList origins) -> length origins >= 2
      _otherwise -> False
