{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otelcol.Exporter.Core (
  OpenTelemetryExporter (..),
  HttpProtobufExporter,
  HttpProtobufError (..),
  validateOpenTelemetryCollectorOptions,
  withOpenTelemetryExporter,
  sendHttpProtobuf,
) where

import Control.Exception (Exception (..), throwIO)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.ProtoLens.Encoding qualified as Proto
import Data.ProtoLens.Message (Message (defMessage))
import GHC.Eventlog.Live.Otelcol.Options (
  HttpHeader (..),
  HttpProtobufOptions (..),
  OpenTelemetryCollectorOptions (..),
  OpenTelemetryCollectorProtocol (..),
 )
import Network.GRPC.Client qualified as G
import Network.GRPC.Common qualified as G
import Network.HTTP.Client qualified as H
import Network.HTTP.Client.TLS qualified as H
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Text.Printf (printf)

data OpenTelemetryExporter
  = OpenTelemetryExporter'Grpc G.Connection
  | OpenTelemetryExporter'Http HttpProtobufExporter

data HttpProtobufExporter = HttpProtobufExporter
  { manager :: H.Manager
  , baseUrl :: String
  , headers :: HTTP.RequestHeaders
  }

data HttpProtobufError
  = HttpProtobufStatusError
      { statusCode :: Int
      , statusMessage :: ByteString
      , responseBody :: ByteString
      }
  | HttpProtobufDecodeError
      { errorMessage :: String
      }
  deriving (Show)

instance Exception HttpProtobufError where
  displayException :: HttpProtobufError -> String
  displayException = \case
    HttpProtobufStatusError{..} ->
      printf
        "Error: OpenTelemetry Collector HTTP/Protobuf endpoint returned status %d %s with body: %s"
        statusCode
        (BSC.unpack statusMessage)
        (BSC.unpack responseBody)
    HttpProtobufDecodeError{..} ->
      "Error: Could not decode OpenTelemetry Collector HTTP/Protobuf response: " <> errorMessage

withOpenTelemetryExporter :: OpenTelemetryCollectorOptions -> (OpenTelemetryExporter -> IO a) -> IO a
withOpenTelemetryExporter options action =
  case toExporterOptions options of
    Left err -> ioError (userError err)
    Right (ExporterOptions'Grpc server) ->
      G.withConnection G.def server $ \conn ->
        action (OpenTelemetryExporter'Grpc conn)
    Right (ExporterOptions'HttpProtobuf httpProtobufOptions) -> do
      manager <- H.newManager H.tlsManagerSettings
      action . OpenTelemetryExporter'Http $ makeHttpProtobufExporter manager httpProtobufOptions

validateOpenTelemetryCollectorOptions :: OpenTelemetryCollectorOptions -> Either String ()
validateOpenTelemetryCollectorOptions = void . toExporterOptions

data ExporterOptions
  = ExporterOptions'Grpc G.Server
  | ExporterOptions'HttpProtobuf HttpProtobufOptions

toExporterOptions :: OpenTelemetryCollectorOptions -> Either String ExporterOptions
toExporterOptions OpenTelemetryCollectorOptions{..} =
  case openTelemetryCollectorProtocol of
    OpenTelemetryCollectorProtocol'Grpc -> do
      case otelcolHeaders of
        [] -> Right ()
        _ : _ -> Left "--otelcol-header is only supported with --otelcol-protocol=http-protobuf."
      Right . ExporterOptions'Grpc $
        makeServer
          (G.Address otelcolHost (maybe 4317 fromIntegral maybeOtelcolPort) maybeOtelcolAuthority)
          otelcolSsl
          (makeServerValidation maybeOtelcolCertificateStore)
          (fromMaybe G.SslKeyLogNone maybeOtelcolSslKeyLog)
    OpenTelemetryCollectorProtocol'HttpProtobuf -> do
      forM_ maybeOtelcolAuthority $ \_ ->
        Left "--otelcol-authority is only supported with --otelcol-protocol=grpc."
      forM_ maybeOtelcolCertificateStore $ \_ ->
        Left "--otelcol-certificate-store is only supported with --otelcol-protocol=grpc."
      forM_ maybeOtelcolSslKeyLog $ \_ ->
        Left "--otelcol-ssl-key-log and --otelcol-ssl-key-log-from-env are only supported with --otelcol-protocol=grpc."
      pure . ExporterOptions'HttpProtobuf $
        HttpProtobufOptions
          { httpProtobufScheme = if otelcolSsl then "https" else "http"
          , httpProtobufHost = otelcolHost
          , httpProtobufPort = fromMaybe 4318 maybeOtelcolPort
          , httpProtobufHeaders = otelcolHeaders
          }
 where
  makeServer :: G.Address -> Bool -> G.ServerValidation -> G.SslKeyLog -> G.Server
  makeServer address ssl serverValidation sslKeyLog
    | ssl = G.ServerSecure serverValidation sslKeyLog address
    | otherwise = G.ServerInsecure address

  makeServerValidation :: Maybe FilePath -> G.ServerValidation
  makeServerValidation =
    G.ValidateServer . maybe G.certStoreFromSystem G.certStoreFromPath

makeHttpProtobufExporter :: H.Manager -> HttpProtobufOptions -> HttpProtobufExporter
makeHttpProtobufExporter manager HttpProtobufOptions{..} =
  HttpProtobufExporter
    { manager = manager
    , baseUrl = httpProtobufScheme <> "://" <> httpProtobufHost <> ":" <> show httpProtobufPort
    , headers =
        map
          ( \HttpHeader{..} ->
              ( CI.mk (BSC.pack httpHeaderName)
              , BSC.pack httpHeaderValue
              )
          )
          httpProtobufHeaders
    }

sendHttpProtobuf :: (Message req, Message resp) => HttpProtobufExporter -> String -> req -> IO resp
sendHttpProtobuf HttpProtobufExporter{..} path req = do
  baseRequest <- H.parseRequest (baseUrl <> path)
  let request =
        baseRequest
          { H.method = "POST"
          , H.requestBody = H.RequestBodyBS (Proto.encodeMessage req)
          , H.checkResponse = \_ _ -> pure ()
          , H.requestHeaders =
              [ (HTTP.hContentType, "application/x-protobuf")
              , (HTTP.hAccept, "application/x-protobuf")
              ]
                <> headers
          }
  response <- H.httpLbs request manager
  let status = H.responseStatus response
  let body = BSL.toStrict (H.responseBody response)
  if HTTP.statusIsSuccessful status
    then decodeResponseBody body
    else
      throwIO
        HttpProtobufStatusError
          { statusCode = HTTP.statusCode status
          , statusMessage = HTTP.statusMessage status
          , responseBody = body
          }

decodeResponseBody :: (Message msg) => ByteString -> IO msg
decodeResponseBody body
  | BS.null body = pure defMessage
  | otherwise =
      case Proto.decodeMessage body of
        Left errorMessage -> throwIO HttpProtobufDecodeError{..}
        Right msg -> pure msg
