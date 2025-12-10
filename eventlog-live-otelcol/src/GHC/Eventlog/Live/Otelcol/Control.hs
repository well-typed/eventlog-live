{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otelcol.Control (
  runControlApp,
) where

import Data.ByteString (ByteString)
import Data.Char (isLower, isUpper, toLower)
import Data.List qualified as L
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Generics (Generic)
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai (Request (..))
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Network.Wai.Middleware.RequestLogger (Destination (..), DetailedSettings (..), OutputFormat (..), RequestLoggerSettings (..), defaultDetailedSettings, defaultRequestLoggerSettings, mkRequestLogger)
import Servant.API (FormUrlEncoded, Get, JSON, PostAccepted, ReqBody, (:>), type (:<|>) (..))
import Servant.Server (Handler, Server, serve)
import System.Log.FastLogger (fromLogStr)
import Web.FormUrlEncoded (Form, FormOptions (fieldLabelModifier), FromForm (..), defaultFormOptions, genericFromForm)

--------------------------------------------------------------------------------
-- Control App
--------------------------------------------------------------------------------

runControlApp :: Logger IO -> Warp.Port -> IO ()
runControlApp logger port = do
  requestLogger <- mkRequestLoggerForLogger
  Warp.run port $
    requestLogger $
      -- TODO: Allow customizable CORS policy.
      cors corsResourcePolicy $
        serve (Proxy @ControlApi) $
          controlServer logger
 where
  mkRequestLoggerForLogger =
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
--------------------------------------------------------------------------------

controlServer :: Logger IO -> Server ControlApi
controlServer _logger =
  health :<|> heapProfiling
 where
  health :: Handler ()
  health = pure ()

  heapProfiling :: Server HeapProfilingApi
  heapProfiling = start :<|> stop :<|> census
   where
    start :: HeapProfilingStart -> Handler ()
    start _req = pure ()

    stop :: HeapProfilingStop -> Handler ()
    stop _req = pure ()

    census :: HeapProfilingCensus -> Handler ()
    census _req = pure ()

--------------------------------------------------------------------------------
-- Control API
--------------------------------------------------------------------------------

type ControlApi =
  "control" :> (HealthApi :<|> HeapProfilingApi)

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

type HeapProfilingApi =
  "heap-profiling"
    :> ( "start" :> HeapProfilingStartApi
          :<|> "stop" :> HeapProfilingStopApi
          :<|> "census" :> HeapProfilingCensusApi
       )

type HeapProfilingStartApi =
  ReqBody '[FormUrlEncoded] HeapProfilingStart
    :> PostAccepted '[JSON] ()

type HeapProfilingStopApi =
  ReqBody '[FormUrlEncoded] HeapProfilingStop
    :> PostAccepted '[JSON] ()

type HeapProfilingCensusApi =
  ReqBody '[FormUrlEncoded] HeapProfilingCensus
    :> PostAccepted '[JSON] ()

--------------------------------------------------------------------------------
-- Health
--------------------------------------------------------------------------------

data Health = Health
  deriving (Generic)

--------------------------------------------------------------------------------
-- HeapProfilingStart
--------------------------------------------------------------------------------

newtype HeapProfilingStart = HeapProfilingStart
  { serviceName :: Text
  }
  deriving (Generic)

instance FromForm HeapProfilingStart where
  fromForm :: Form -> Either Text HeapProfilingStart
  fromForm = genericFromForm myFormOptions

--------------------------------------------------------------------------------
-- HeapProfilingStop
--------------------------------------------------------------------------------

newtype HeapProfilingStop = HeapProfilingStop
  { serviceName :: Text
  }
  deriving (Generic)

instance FromForm HeapProfilingStop where
  fromForm :: Form -> Either Text HeapProfilingStop
  fromForm = genericFromForm myFormOptions

--------------------------------------------------------------------------------
-- HeapProfilingCensus
--------------------------------------------------------------------------------

newtype HeapProfilingCensus = HeapProfilingCensus
  { serviceName :: Text
  }
  deriving (Generic)

instance FromForm HeapProfilingCensus where
  fromForm :: Form -> Either Text HeapProfilingCensus
  fromForm = genericFromForm myFormOptions

myFormOptions :: FormOptions
myFormOptions =
  defaultFormOptions
    { fieldLabelModifier = camelTo2 '-'
    }

--------------------------------------------------------------------------------
-- Internal helpers.
--------------------------------------------------------------------------------

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
