{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.App.Server.Control (
  ControlHooks (..),
  ControlApi,
  newControlServer,
  newDisabledControlServer,
  control,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, genericParseJSON, genericToJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isLower, isUpper, toLower)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Eventlog.Live.App.Environment (ServiceName (..))
import GHC.Eventlog.Live.Logger (Logger, logDebug)
import GHC.Eventlog.Live.Source.Core (EventlogSourceHandle (..))
import GHC.Generics (Generic)
import Options.Applicative.Extra.Feature (Feature (..))
import Servant (PlainText, throwError)
import Servant.API (Capture, FormUrlEncoded, Header, Headers, JSON, NoContent (..), ReqBody, StdMethod (..), UVerb, Union, WithStatus (..), addHeader, (:>), type (:<|>) (..))
import Servant.Server (Handler, Server, ServerError (..), respond)
import Web.FormUrlEncoded (Form, FormOptions, FromForm (..), genericFromForm)
import Web.FormUrlEncoded qualified as Form (FormOptions (fieldLabelModifier), defaultFormOptions)

#ifdef EVENTLOG_LIVE_FEATURE_CONTROL
import Control.Exception (Exception (..), catches)
import Control.Exception qualified as E (Handler (..))
import Data.Binary qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Word (Word8)
import GHC.Eventlog.Socket.Control qualified as C
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy qualified as SBSL
import Text.Read (readMaybe)
#endif

--------------------------------------------------------------------------------
-- Feature: control
--------------------------------------------------------------------------------

control :: Feature
control = Feature{flag = "control", isOn = isOn, info = "Cannot start control server."}
 where
  isOn :: Bool
#ifdef EVENTLOG_LIVE_FEATURE_CONTROL
  isOn = True
#else
  isOn = False
#endif

--------------------------------------------------------------------------------
-- Control App
--------------------------------------------------------------------------------

data ControlHooks = ControlHooks
  { notifyNewConnection :: ServiceName -> EventlogSourceHandle -> IO ()
  , notifyEndConnection :: ServiceName -> IO ()
  }

newControlServer :: Logger IO -> Bool -> IO (ControlHooks, Server ControlApi)
#ifdef EVENTLOG_LIVE_FEATURE_CONTROL
newControlServer = newEnabledControlServer
#else
newControlServer = newDisabledControlServer
#endif

--------------------------------------------------------------------------------
-- Control App - Disabled
--------------------------------------------------------------------------------

newDisabledControlServer :: Logger IO -> Bool -> IO (ControlHooks, Server ControlApi)
newDisabledControlServer logger corsIgnoreFailures = do
  pure (api, server)
 where
  api :: ControlHooks
  api =
    ControlHooks
      { notifyNewConnection = \_serviceName _eventlogSourceHandle -> pure ()
      , notifyEndConnection = \_serviceName -> pure ()
      }

  server :: Server ControlApi
  server _namespaceText _commandText = handleCommand :<|> badCORSPreflight
   where
    badCORSPreflight :: Handler (Union '[BadCORSPreflightAccept, BadCORSPreflightReject])
    badCORSPreflight = badCORSPreflightHandler logger corsIgnoreFailures

    handleCommand :: CommandReq -> Handler (Union '[CommandAccept, CommandReject])
    handleCommand _req = disabled

    disabled :: Handler a
    disabled =
      throwError
        ServerError
          { errHTTPCode = 400
          , errReasonPhrase = "Bad Request"
          , errBody = BSL.fromStrict . TE.encodeUtf8 $ "The control server is disabled."
          , errHeaders = []
          }

--------------------------------------------------------------------------------
-- Control App - Enabled
--------------------------------------------------------------------------------
#ifdef EVENTLOG_LIVE_FEATURE_CONTROL

newEnabledControlServer :: Logger IO -> Bool -> IO (ControlHooks, Server ControlApi)
newEnabledControlServer logger corsIgnoreFailures = do
  -- Create variable for storing sockets.
  connectionMapRef <- newIORef @(HashMap ServiceName EventlogSourceHandle) M.empty
  pure (api connectionMapRef, server connectionMapRef)
 where
  api :: IORef (HashMap ServiceName EventlogSourceHandle) -> ControlHooks
  api connectionMapRef = ControlHooks{..}
   where
    -- When notified of a new connection, update the connectionMapRef.
    notifyNewConnection serviceName eventlogSourceHandle = do
      logDebug logger $
        "New connection for service " <> serviceName.serviceName <> "."
      atomicModifyIORef' connectionMapRef $ \connectionMap ->
        let !connectionMap' = M.insert serviceName eventlogSourceHandle connectionMap
         in (connectionMap', ())

    -- When notified of the end of a connection, update the connectionMapRef.
    notifyEndConnection serviceName = do
      logDebug logger $
        "End connection for service " <> serviceName.serviceName <> "."
      atomicModifyIORef' connectionMapRef $ \connectionMap ->
        let !connectionMap' = M.delete serviceName connectionMap
         in (connectionMap', ())

  server :: IORef (HashMap ServiceName EventlogSourceHandle) -> Server ControlApi
  server connectionMapRef namespaceText commandText = handleCommand :<|> badCORSPreflight
   where
    badCORSPreflight :: Handler (Union '[BadCORSPreflightAccept, BadCORSPreflightReject])
    badCORSPreflight = badCORSPreflightHandler logger corsIgnoreFailures

    handleCommand :: CommandReq -> Handler (Union '[CommandAccept, CommandReject])
    handleCommand req
        | -- Handle the builtin eventlog-socket commands by name.
          namespaceText == "eventlog-socket"
        , Just command <- eventlogSocketCommandId commandText = do
            withSocketFor (ServiceName req.serviceName) $ \socket ->
              liftIO (SBSL.sendAll socket $ B.encode command)
            respond $ WithStatus @204 NoContent
        -- Handle custom commands.
        | Just commandId <- readMaybe @Word8 (T.unpack commandText) = do
            -- Construct the user namespace.
            liftIO (eitherUserNamespace namespaceText) >>= \case
              -- If userNamespace throws an exception, then...
              Left customCommandError ->
                -- ...respond with a custom command error.
                respond $ WithStatus @404 customCommandError
              -- Otherwise, ...
              Right namespace -> do
                -- ...construct the user command...
                let command = C.userCommand namespace (C.CommandId commandId)
                -- ...send the user command...
                withSocketFor (ServiceName req.serviceName) $ \socket -> do
                  liftIO (SBSL.sendAll socket $ B.encode command)
                -- ...respond with a success status.
                respond $ WithStatus @204 NoContent
        | otherwise = do
          let errorMessage="Unknown command "<>T.unpack commandText
          respond $ WithStatus @404 CustomCommandError{..}

    withSocketFor :: ServiceName -> (Socket -> Handler ()) -> Handler ()
    withSocketFor serviceName action = do
      connectionMap <- liftIO (readIORef connectionMapRef)
      case M.lookup serviceName connectionMap of
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
        Just (EventlogSourceHandleSocket s) -> action s


eventlogSocketCommandId :: Text -> Maybe C.Command
eventlogSocketCommandId commandText
  | commandText == "start-profiling" = Just C.startProfiling
  | commandText == "stop-profiling" = Just C.stopProfiling
  | commandText == "start-heap-profiling" = Just C.startHeapProfiling
  | commandText == "stop-heap-profiling" = Just C.stopHeapProfiling
  | commandText == "request-heap-census" = Just C.requestHeapCensus
  | otherwise = Nothing

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

-- EVENTLOG_LIVE_FEATURE_CONTROL
#endif
--------------------------------------------------------------------------------
-- Control API
--------------------------------------------------------------------------------

type ControlApi =
  Capture "namespace" Text
    :> Capture "commandId" Text
    :> ( ( ReqBody '[FormUrlEncoded, JSON] CommandReq
             :> UVerb 'POST '[JSON] '[CommandAccept, CommandReject]
         )
           :<|> BadCORSPreflight
       )

type CommandAccept = WithStatus 204 NoContent

type CommandReject = WithStatus 404 CustomCommandError

newtype CustomCommandError = CustomCommandError
  { errorMessage :: String
  }
  deriving (Generic, Show)

instance ToJSON CustomCommandError where
  toJSON :: CustomCommandError -> Value
  toJSON = genericToJSON myJSONOptions

--------------------------------------------------------------------------------
-- CommandReq

newtype CommandReq = CommandReq
  { serviceName :: Text
  }
  deriving (Generic, Show)

instance FromForm CommandReq where
  fromForm :: Form -> Either Text CommandReq
  fromForm = genericFromForm myFormOptions

instance FromJSON CommandReq where
  parseJSON :: Value -> Parser CommandReq
  parseJSON = genericParseJSON myJSONOptions

--------------------------------------------------------------------------------
-- Bad CORS preflight requests

type BadCORSPreflight =
  UVerb 'OPTIONS '[PlainText] '[BadCORSPreflightAccept, BadCORSPreflightReject]

type BadCORSPreflightAccept =
  WithStatus
    204
    ( Headers
        '[ Header "Access-Control-Allow-Origin" String
         , Header "Access-Control-Allow-Methods" String
         , Header "Access-Control-Allow-Headers" String
         ]
        NoContent
    )

type BadCORSPreflightReject = WithStatus 400 NoContent

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

-- | Handler for bad CORS preflight requests.
badCORSPreflightHandler :: Logger IO -> Bool -> Handler (Union '[BadCORSPreflightAccept, BadCORSPreflightReject])
badCORSPreflightHandler logger corsIgnoreFailures
  | corsIgnoreFailures = do
      -- This accepts malformed CORS preflight requests.
      liftIO . logDebug logger $ "Accepted malformed CORS preflight request."
      respond . WithStatus @204 $
        addHeader @"Access-Control-Allow-Origin" @String "*" $
          addHeader @"Access-Control-Allow-Methods" @String "GET, POST" $
            addHeader @"Access-Control-Allow-Headers" @String "*" $
              NoContent
  | otherwise = do
      liftIO . logDebug logger $ "Accepted malformed CORS preflight request."
      respond . WithStatus @400 $
        NoContent

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
