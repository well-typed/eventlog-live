{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otelcol.Exporter.Core (
  OtlpExporter (..),
  parseOtlpExporterOptions,
  withOtlpExporter,
  export,

  -- * Export via gRPC
  CanExportViaGrpc,

  -- * Export via HTTP/Protobuf
  CanExportViaHttpProtobuf (..),
  HttpError (..),
) where

import Control.Exception (Exception (..), throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.ProtoLens.Encoding qualified as Proto
import Data.ProtoLens.Message (Message (defMessage))
import Data.ProtoLens.Service.Types (HasMethodImpl (..))
import GHC.Eventlog.Live.Otelcol.Options (OtlpExporterOptions (..), OtlpProtocol (..))
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
import Network.URL (URL (url_path))
import Network.URL qualified as URL

--------------------------------------------------------------------------------
-- OTLP Exporter
--------------------------------------------------------------------------------

data OtlpExporter
  = OtlpExporterGrpc !OtlpGrpcExporter
  | OtlpExporterHttpProtobuf !OtlpHttpProtobufExporter

{- |
Construct an t`OtlpExporter` from t`OtlpExporterOptions`.
-}
withOtlpExporter :: OtlpExporterOptions OtlpEndpoint -> (OtlpExporter -> IO a) -> IO a
withOtlpExporter options action =
  case options of
    OtlpExporterOptions{otlpEndpoint = Left otlpEndpointGrpc, ..} -> do
      let options' = OtlpExporterOptions{otlpEndpoint = otlpEndpointGrpc, ..}
      withOtlpGrpcExporter options' $ action . OtlpExporterGrpc
    OtlpExporterOptions{otlpEndpoint = Right otlpEndpointHttp, ..} -> do
      let options' = OtlpExporterOptions{otlpEndpoint = otlpEndpointHttp, ..}
      withOtlpHttpProtobufExporter options' $ action . OtlpExporterHttpProtobuf

{- |
The options for an OTLP endpoint.
-}
type OtlpEndpoint = Either OtlpEndpointGrpc OtlpEndpointHttp

{- |
Parse the OTLP endpoint from a t`String` to an t`OtlpEndpoint`.
-}
parseOtlpExporterOptions :: OtlpExporterOptions String -> Either String (OtlpExporterOptions OtlpEndpoint)
parseOtlpExporterOptions OtlpExporterOptions{..} = do
  otlpEndpoint' <- parseOtlpEndpoint otlpProtocol otlpEndpoint
  let !options' = OtlpExporterOptions{otlpEndpoint = otlpEndpoint', ..}
  case otlpProtocol of
    OtlpProtocolGrpc
      | Just headers <- otlpHttpHeaders
      , not (null headers) -> do
          let showHeaders = L.intercalate "," . map (\(name, value) -> name <> "=" <> value)
          Left $ "The grpc protocol does not support additional HTTP headers, found " <> showHeaders headers
    OtlpProtocolHttpProtobuf
      | Just _sslKeyLog <- otlpGrpcSslKeyLog ->
          Left $ "The http/protobuf protocol does not support the SSL key log."
    OtlpProtocolHttpProtobuf
      | Just certificateStore <- otlpGrpcCertificateStore ->
          Left $ "The http/protobuf protocol does not support the certificate store, found " <> certificateStore
    _otherwise -> pure options'

{- |
Parse an OTLP endpoint string as an t`OtlpEndpoint` depending on the t`OtlpProtocol`.
-}
parseOtlpEndpoint :: OtlpProtocol -> String -> Either String OtlpEndpoint
parseOtlpEndpoint = go True
 where
  go :: Bool -> OtlpProtocol -> String -> Either String OtlpEndpoint
  go retry otlpProtocol url =
    case URL.importURL url of
      Just URL.URL{url_type = URL.Absolute URL.Host{..}, ..} ->
        case otlpProtocol of
          OtlpProtocolGrpc
            | not (null url_path) ->
                Left $ "The grpc protocol does not support an URL path, found: " <> URL.encString False URL.ok_path url_path
            | not (null url_params) ->
                Left $ "The grpc protocol does not support URL parameters, found: " <> URL.exportParams url_params
            | URL.HTTP secure <- protocol ->
                Right . Left $
                  OtlpEndpointGrpc
                    { otlpGrpcHost = host
                    , otlpGrpcPort = fromMaybe 4317 port
                    , otlpGrpcSecure = secure
                    }
            | otherwise ->
                Left $ "The grpc protocol does not support " <> exportProt protocol
          OtlpProtocolHttpProtobuf
            | URL.HTTP secure <- protocol ->
                Right . Right $
                  OtlpEndpointHttp
                    { otlpHttpHost = host
                    , otlpHttpPort = fromMaybe 4318 port
                    , otlpHttpSecure = secure
                    , otlpHttpPath = url_path
                    , otlpHttpParams = url_params
                    }
            | otherwise ->
                Left $ "The http/protobuf protocol does not support " <> exportProt protocol
      Just URL.URL{url_type = URL.PathRelative{}}
        | retry ->
            go False otlpProtocol ("http://" <> url)
      Just URL.URL{} ->
        Left $ "Endpoint must be absolute URL, found " <> url
      Nothing ->
        Left $ "Could not parse url " <> url

  exportProt :: URL.Protocol -> String
  exportProt prot = case prot of
    URL.HTTP True -> "https"
    URL.HTTP False -> "http"
    URL.FTP True -> "ftps"
    URL.FTP False -> "ftp"
    URL.RawProt s -> s

export ::
  forall serv meth.
  ( CanExportViaGrpc serv meth
  , CanExportViaHttpProtobuf serv meth
  ) =>
  -- | The HTTP/Protobuf exporter.
  OtlpExporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
export = \case
  OtlpExporterGrpc exporter -> exportGrpc @serv @meth exporter
  OtlpExporterHttpProtobuf exporter -> exportHttpProtobuf @serv @meth exporter

--------------------------------------------------------------------------------
-- OTLP gRPC Exporter
--------------------------------------------------------------------------------

{- |
An opaque OTLP gRPC exporter.
-}
newtype OtlpGrpcExporter = OtlpGrpcExporter
  { connection :: G.Connection
  }

{- |
The options for an OTLP gRPC endpoint.
-}
data OtlpEndpointGrpc = OtlpEndpointGrpc
  { otlpGrpcHost :: !String
  , otlpGrpcPort :: !Integer
  , otlpGrpcSecure :: !Bool
  }

type CanExportViaGrpc serv meth =
  ( G.SupportsClientRpc (Protobuf serv meth)
  , G.SupportsStreamingType (Protobuf serv meth) 'NonStreaming
  , G.RequestMetadata (Protobuf serv meth) ~ G.NoMetadata
  )

withOtlpGrpcExporter :: OtlpExporterOptions OtlpEndpointGrpc -> (OtlpGrpcExporter -> IO a) -> IO a
withOtlpGrpcExporter OtlpExporterOptions{otlpEndpoint = OtlpEndpointGrpc{..}, ..} action =
  G.withConnection G.def server $ \connection -> action OtlpGrpcExporter{..}
 where
  server :: G.Server
  server
    | otlpGrpcSecure = G.ServerSecure serverValidation sslKeyLog address
    | otherwise = G.ServerInsecure address

  address = G.Address otlpGrpcHost (fromIntegral otlpGrpcPort) Nothing
  sslKeyLog = fromMaybe G.SslKeyLogNone otlpGrpcSslKeyLog
  serverValidation = G.ValidateServer $ maybe G.certStoreFromSystem G.certStoreFromPath otlpGrpcCertificateStore

exportGrpc ::
  forall serv meth.
  (CanExportViaGrpc serv meth) =>
  OtlpGrpcExporter ->
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
exportGrpc grpcExporter input =
  G.getProto <$> G.nonStreaming grpcExporter.connection (G.rpc @(G.Protobuf serv meth)) (G.Proto input)

--------------------------------------------------------------------------------
-- OTLP HTTP/Protobuf Exporter
--------------------------------------------------------------------------------

{- |
The options for an OTLP HTTP/Protobuf endpoint.
-}
data OtlpEndpointHttp = OtlpEndpointHttp
  { otlpHttpHost :: !String
  , otlpHttpPort :: !Integer
  , otlpHttpSecure :: !Bool
  , otlpHttpPath :: !String
  , otlpHttpParams :: ![(String, String)]
  }
  deriving (Show)

data OtlpHttpProtobufExporter = OtlpHttpProtobufExporter
  { manager :: H.Manager
  , baseUrl :: String
  , headers :: HTTP.RequestHeaders
  }

data HttpError
  = HttpStatusError
      { statusCode :: Int
      , statusMessage :: ByteString
      , responseBody :: ByteString
      }
  | HttpDecodeError
      { errorMessage :: String
      }
  deriving (Show)

instance Exception HttpError where
  displayException :: HttpError -> String
  displayException = \case
    HttpStatusError{..} ->
      "OpenTelemetry Collector HTTP/Protobuf endpoint returned status "
        <> show statusCode
        <> " "
        <> BSC.unpack statusMessage
        <> " with body: "
        <> BSC.unpack responseBody
    HttpDecodeError{..} ->
      "Could not decode OpenTelemetry Collector HTTP/Protobuf response: "
        <> errorMessage

{- |
Internal helper.

Run an action with an t`OtlpHttpProtobufExporter`.
-}
withOtlpHttpProtobufExporter :: OtlpExporterOptions OtlpEndpointHttp -> (OtlpHttpProtobufExporter -> IO a) -> IO a
withOtlpHttpProtobufExporter OtlpExporterOptions{otlpEndpoint = OtlpEndpointHttp{..}, ..} action = do
  -- Create an HTTP manager.
  manager <- H.newManager H.tlsManagerSettings
  -- Create the HTTP baseUrl.
  let baseUrlType = URL.Absolute URL.Host{protocol = URL.HTTP otlpHttpSecure, host = otlpHttpHost, port = Just otlpHttpPort}
  let baseUrl = URL.URL{url_type = baseUrlType, url_path = otlpHttpPath, url_params = otlpHttpParams}
  -- Create the HTTP headers.
  let headers = [(CI.mk (BSC.pack name), BSC.pack value) | (name, value) <- fromMaybe [] otlpHttpHeaders]
  -- Create the HTTP/Protobuf exporter.
  let exporter = OtlpHttpProtobufExporter{manager = manager, baseUrl = URL.exportURL baseUrl, headers = headers}
  -- Run the action.
  action exporter

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
exportHttpProtobuf ::
  forall serv meth.
  (CanExportViaHttpProtobuf serv meth) =>
  -- | The HTTP/Protobuf exporter.
  OtlpHttpProtobufExporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
exportHttpProtobuf OtlpHttpProtobufExporter{..} req = do
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
        HttpStatusError
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
        Left errorMessage -> throwIO HttpDecodeError{..}
        Right msg -> pure msg
