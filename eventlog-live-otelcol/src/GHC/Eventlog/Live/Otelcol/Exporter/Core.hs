{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otelcol.Exporter.Core (
  OpenTelemetryExporter (..),
  validateOpenTelemetryCollectorOptions,
  withOpenTelemetryExporter,
  export,

  -- * Export via gRPC
  CanExportViaGrpc,

  -- * Export via HTTP/Protobuf
  CanExportViaHttpProtobuf (..),
  HttpProtobufError (..),
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
import Data.ProtoLens.Service.Types (HasMethodImpl (..))
import GHC.Eventlog.Live.Otelcol.Options (HttpHeader (..), HttpProtobufOptions (..), OpenTelemetryCollectorOptions (..), OpenTelemetryCollectorProtocol (..))
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf, StreamingType (..))
import Network.GRPC.Common.Protobuf qualified as G
import Network.GRPC.Common.StreamType qualified as G
import Network.HTTP.Client qualified as H
import Network.HTTP.Client.TLS qualified as H
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- OTLP Exporter
--------------------------------------------------------------------------------

data OpenTelemetryExporter
  = OpenTelemetryExporter'Grpc GrpcExporter
  | OpenTelemetryExporter'HttpProtobuf HttpProtobufExporter

{- |
Construct an t`OpenTelemetryExporter` from t`OpenTelemetryCollectorOptions`.
-}
withOpenTelemetryExporter :: OpenTelemetryCollectorOptions -> (OpenTelemetryExporter -> IO a) -> IO a
withOpenTelemetryExporter options action =
  case toExporterOptions options of
    Left err ->
      ioError (userError err)
    Right (ExporterOptions'Grpc server) ->
      G.withConnection G.def server $ \conn ->
        action (OpenTelemetryExporter'Grpc (GrpcExporter conn))
    Right (ExporterOptions'HttpProtobuf httpProtobufOptions) -> do
      manager <- H.newManager H.tlsManagerSettings
      action (OpenTelemetryExporter'HttpProtobuf $ makeHttpProtobufExporter manager httpProtobufOptions)

{- |
Internal helper.

This is the result of successfully interpreting t`OpenTelemetryCollectorOptions` as either gRPC or HTTP/Protobuf options.
-}
data ExporterOptions
  = ExporterOptions'Grpc G.Server
  | ExporterOptions'HttpProtobuf HttpProtobufOptions

{- |
Check that the t`OpenTelemetryCollectorOptions` options are consistent.
-}
validateOpenTelemetryCollectorOptions :: OpenTelemetryCollectorOptions -> Either String ()
validateOpenTelemetryCollectorOptions = void . toExporterOptions

{- |
Internal helper.

Interpret t`OpenTelemetryCollectorOptions` as either gRPC or HTTP/Protobuf options.
-}
toExporterOptions :: OpenTelemetryCollectorOptions -> Either String ExporterOptions
toExporterOptions OpenTelemetryCollectorOptions{..} =
  case openTelemetryCollectorProtocol of
    OpenTelemetryCollectorProtocol'Grpc -> do
      case otelcolHeaders of
        [] -> Right ()
        _ : _ -> Left "--otelcol-header is only supported with --otelcol-protocol=http/protobuf."
      Right . ExporterOptions'Grpc $
        makeGrpcServer
          (G.Address otelcolHost (maybe 4317 fromIntegral maybeOtelcolPort) maybeOtelcolAuthority)
          otelcolSsl
          (makeGrpcServerValidation maybeOtelcolCertificateStore)
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
  makeGrpcServer :: G.Address -> Bool -> G.ServerValidation -> G.SslKeyLog -> G.Server
  makeGrpcServer address ssl serverValidation sslKeyLog
    | ssl = G.ServerSecure serverValidation sslKeyLog address
    | otherwise = G.ServerInsecure address

  makeGrpcServerValidation :: Maybe FilePath -> G.ServerValidation
  makeGrpcServerValidation =
    G.ValidateServer . maybe G.certStoreFromSystem G.certStoreFromPath

export ::
  forall serv meth.
  ( CanExportViaGrpc serv meth
  , CanExportViaHttpProtobuf serv meth
  ) =>
  -- | The HTTP/Protobuf exporter.
  OpenTelemetryExporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
export = \case
  OpenTelemetryExporter'Grpc grpcExporter ->
    sendGrpc @serv @meth grpcExporter
  OpenTelemetryExporter'HttpProtobuf httpProtobufExporter ->
    sendHttpProtobuf @serv @meth httpProtobufExporter

--------------------------------------------------------------------------------
-- OTLP gRPC Exporter
--------------------------------------------------------------------------------

newtype GrpcExporter = GrpcExporter
  { connection :: G.Connection
  }

type CanExportViaGrpc serv meth =
  ( G.SupportsClientRpc (Protobuf serv meth)
  , G.SupportsStreamingType (Protobuf serv meth) 'NonStreaming
  , G.RequestMetadata (Protobuf serv meth) ~ G.NoMetadata
  )

sendGrpc ::
  forall serv meth.
  (CanExportViaGrpc serv meth) =>
  GrpcExporter ->
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
sendGrpc grpcExporter input =
  G.getProto <$> G.nonStreaming grpcExporter.connection (G.rpc @(G.Protobuf serv meth)) (G.Proto input)

--------------------------------------------------------------------------------
-- OTLP HTTP/Protobuf Exporter
--------------------------------------------------------------------------------

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

{- |
Internal helper.
-}
makeHttpProtobufExporter :: H.Manager -> HttpProtobufOptions -> HttpProtobufExporter
makeHttpProtobufExporter manager HttpProtobufOptions{..} =
  HttpProtobufExporter
    { manager = manager
    , baseUrl = httpProtobufScheme <> "://" <> httpProtobufHost <> ":" <> show httpProtobufPort
    , headers = makeRequestHeaders httpProtobufHeaders
    }

{- |
Internal helper.

Convert a list of t`HttpHeader` headers to `HTTP.RequestHeaders`.
-}
makeRequestHeaders :: [HttpHeader] -> HTTP.RequestHeaders
makeRequestHeaders httpHeaders =
  [ (CI.mk (BSC.pack httpHeaderName), BSC.pack httpHeaderValue)
  | HttpHeader{..} <- httpHeaders
  ]

class
  ( Message (MethodInput serv meth)
  , Message (MethodOutput serv meth)
  ) =>
  CanExportViaHttpProtobuf serv meth
  where
  apiPath :: String

{- |
Send a Protobuf message over an HTTP connection.
-}
sendHttpProtobuf ::
  forall serv meth.
  (CanExportViaHttpProtobuf serv meth) =>
  -- | The HTTP/Protobuf exporter.
  HttpProtobufExporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
sendHttpProtobuf HttpProtobufExporter{..} req = do
  baseRequest <- H.parseRequest (baseUrl <> apiPath @serv @meth)
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

{- |
Internal helper.

Decode the HTTP response body into a Protobuf message.
-}
decodeResponseBody :: (Message msg) => ByteString -> IO msg
decodeResponseBody body
  | BS.null body = pure defMessage
  | otherwise =
      case Proto.decodeMessage body of
        Left errorMessage -> throwIO HttpProtobufDecodeError{..}
        Right msg -> pure msg
