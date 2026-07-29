{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otlp.Exporter.Core (
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
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.Maybe (fromMaybe)
import Data.ProtoLens.Encoding qualified as Proto
import Data.ProtoLens.Message (Message (defMessage))
import Data.ProtoLens.Service.Types (HasMethodImpl (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Otlp.Options (OtlpExporterOptions (..), OtlpProtocol (..))
import GHC.IsList qualified as IsList
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
import Network.URI qualified as URI
import OpenTelemetry.Baggage (encodeBaggageHeader)
import OpenTelemetry.Baggage qualified as Baggage
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- OTLP Exporter
--------------------------------------------------------------------------------

data OtlpExporter
  = OtlpExporterGrpc !OtlpGrpcExporter
  | OtlpExporterHttpProtobuf !OtlpHttpProtobufExporter

{- |
Construct an t`OtlpExporter` from t`OtlpExporterOptions`.
-}
withOtlpExporter ::
  Logger IO ->
  OtlpExporterOptions OtlpEndpoint ->
  (OtlpExporter -> IO a) ->
  IO a
withOtlpExporter logger options action =
  case options of
    OtlpExporterOptions{otlpEndpoint = Left otlpGrpcEndpoint, ..} -> do
      let options' = OtlpExporterOptions{otlpEndpoint = otlpGrpcEndpoint, ..}
      withOtlpGrpcExporter logger options' $ action . OtlpExporterGrpc
    OtlpExporterOptions{otlpEndpoint = Right otlpHttpEndpoint, ..} -> do
      let options' = OtlpExporterOptions{otlpEndpoint = otlpHttpEndpoint, ..}
      withOtlpHttpProtobufExporter logger options' $ action . OtlpExporterHttpProtobuf

{- |
The options for an OTLP endpoint.
-}
type OtlpEndpoint = Either OtlpGrpcEndpoint OtlpHttpEndpoint

{- |
Parse the OTLP endpoint from a t`String` to an t`OtlpEndpoint`.
-}
parseOtlpExporterOptions :: OtlpExporterOptions (Maybe String) -> Either String (OtlpExporterOptions OtlpEndpoint)
parseOtlpExporterOptions OtlpExporterOptions{..} = do
  otlpEndpoint' <- parseOtlpEndpoint otlpProtocol otlpEndpoint
  let !options' = OtlpExporterOptions{otlpEndpoint = otlpEndpoint', ..}
  case otlpProtocol of
    OtlpProtocolGrpc
      | Just headers <- otlpHttpHeaders
      , headers /= Baggage.empty -> do
          let showHeaders = T.unpack . TE.decodeUtf8 . encodeBaggageHeader
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
parseOtlpEndpoint :: OtlpProtocol -> Maybe String -> Either String OtlpEndpoint
parseOtlpEndpoint = go True
 where
  go :: Bool -> OtlpProtocol -> Maybe String -> Either String OtlpEndpoint
  go _retry otlpProtocol Nothing =
    case otlpProtocol of
      OtlpProtocolGrpc ->
        pure $ Left OtlpGrpcEndpoint{host = "localhost", port = 4317, secure = False}
      OtlpProtocolHttpProtobuf ->
        pure $ Right OtlpHttpEndpoint{baseUrl = "http://localhost:4318"}
  go retry otlpProtocol (Just url) =
    case URI.parseURI url of
      Just URI.URI{..} ->
        case otlpProtocol of
          OtlpProtocolGrpc
            | uriScheme `elem` ["http:", "https:"]
            , null uriPath
            , null uriQuery
            , null uriFragment -> do
                let !host = maybe "localhost" (.uriRegName) uriAuthority
                let !port = fromMaybe 4317 $ uriPortNumber uriAuthority
                let !secure = uriScheme == "https:"
                pure $ Left OtlpGrpcEndpoint{..}
            | otherwise ->
                Left $ "The gRPC protocol only supports HTTP and HTTPS and does not support an URI path, query, or fragment, found: " <> url
          OtlpProtocolHttpProtobuf
            | uriScheme `elem` ["http:", "https:"]
            , null uriQuery
            , null uriFragment -> do
                let !uriAuth = (fromMaybe URI.nullURIAuth uriAuthority){URI.uriRegName = maybe "localhost" (.uriRegName) uriAuthority}
                let !baseURI = URI.nullURI{URI.uriScheme = uriScheme, URI.uriAuthority = Just uriAuth, URI.uriPath = uriPath}
                pure $ Right OtlpHttpEndpoint{baseUrl = show baseURI}
            | otherwise ->
                Left $ "The HTTP/Protobuf protocol only supports HTTP and HTTPS and does not support an URI query or fragment, found: " <> url
      Nothing
        | retry ->
            go False otlpProtocol (Just $ "http://" <> url)
        | otherwise ->
            Left $ "Could not parse url " <> url

{- |
Internal helper.

Extract a t`Word16` port number from a `URI.URIAuth`.
-}
uriPortNumber :: Maybe URI.URIAuth -> Maybe Word16
uriPortNumber = readMaybe @Word16 <=< fmap (dropColon . (.uriPort))
 where
  dropColon :: String -> String
  dropColon = \case (':' : str) -> str; str -> str

{- |
Export telemetry data to the t`OtlpExporter`.
-}
export ::
  forall serv meth.
  ( CanExportViaGrpc serv meth
  , CanExportViaHttpProtobuf serv meth
  ) =>
  Logger IO ->
  -- | The HTTP/Protobuf exporter.
  OtlpExporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
export logger = \case
  OtlpExporterGrpc exporter -> exportGrpc @serv @meth logger exporter
  OtlpExporterHttpProtobuf exporter -> exportHttpProtobuf @serv @meth logger exporter

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
data OtlpGrpcEndpoint = OtlpGrpcEndpoint
  { host :: !String
  , port :: !Word16
  , secure :: !Bool
  }

type CanExportViaGrpc serv meth =
  ( G.SupportsClientRpc (Protobuf serv meth)
  , G.SupportsStreamingType (Protobuf serv meth) 'NonStreaming
  , G.RequestMetadata (Protobuf serv meth) ~ G.NoMetadata
  )

withOtlpGrpcExporter ::
  Logger IO ->
  OtlpExporterOptions OtlpGrpcEndpoint ->
  (OtlpGrpcExporter -> IO a) ->
  IO a
withOtlpGrpcExporter _logger OtlpExporterOptions{otlpEndpoint = OtlpGrpcEndpoint{..}, ..} action =
  G.withConnection G.def server $ \connection -> action OtlpGrpcExporter{..}
 where
  server :: G.Server
  server
    | secure = G.ServerSecure serverValidation sslKeyLog address
    | otherwise = G.ServerInsecure address

  address = G.Address host (fromIntegral port) Nothing
  sslKeyLog = fromMaybe G.SslKeyLogNone otlpGrpcSslKeyLog
  serverValidation = G.ValidateServer $ maybe G.certStoreFromSystem G.certStoreFromPath otlpGrpcCertificateStore

exportGrpc ::
  forall serv meth.
  (CanExportViaGrpc serv meth) =>
  Logger IO ->
  OtlpGrpcExporter ->
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
exportGrpc _logger grpcExporter input =
  G.getProto <$> G.nonStreaming grpcExporter.connection (G.rpc @(G.Protobuf serv meth)) (G.Proto input)

--------------------------------------------------------------------------------
-- OTLP HTTP/Protobuf Exporter
--------------------------------------------------------------------------------

{- |
The options for an OTLP HTTP/Protobuf endpoint.
-}
newtype OtlpHttpEndpoint = OtlpHttpEndpoint
  { baseUrl :: String
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
withOtlpHttpProtobufExporter ::
  Logger IO ->
  OtlpExporterOptions OtlpHttpEndpoint ->
  (OtlpHttpProtobufExporter -> IO a) ->
  IO a
withOtlpHttpProtobufExporter logger OtlpExporterOptions{otlpEndpoint = OtlpHttpEndpoint{..}, ..} action = do
  -- Create an HTTP manager.
  manager <- H.newManager H.tlsManagerSettings
  -- Create the HTTP headers.
  writeLog logger TRACE . T.pack $ "HTTP/Protobuf Exporter - Headers: " <> show otlpHttpHeaders
  let headers =
        [ (CI.mk (Baggage.tokenValue token), TE.encodeUtf8 value)
        | (token, Baggage.Element value _properties) <-
            IsList.toList (maybe mempty Baggage.values otlpHttpHeaders)
        ]
  -- Run the action.
  action OtlpHttpProtobufExporter{..}

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
  Logger IO ->
  -- | The HTTP/Protobuf exporter.
  OtlpHttpProtobufExporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
exportHttpProtobuf logger OtlpHttpProtobufExporter{..} req = do
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
  writeLog logger TRACE . T.pack $ "HTTP/Protobuf Exporter - HTTP Request:  " <> show request
  response <- H.httpLbs request manager
  writeLog logger TRACE . T.pack $ "HTTP/Protobuf Exporter - HTTP Response:  " <> show response
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
