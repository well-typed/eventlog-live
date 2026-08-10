{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Otlp.Exporter.Core (
  -- * Exporter
  Exporter (..),
  withExporter,
  withExporters,
  export,

  -- ** Export via gRPC
  CanExportToConsole,

  -- ** Export via gRPC
  CanExportToOltpViaGrpc,

  -- ** Export via HTTP/Protobuf
  CanExportToOltpViaHttpProtobuf (..),
  HttpError (..),
) where

import Codec.Compression.GZip qualified as GZip
import Control.Exception (Exception (..), throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive qualified as CI
import Data.Maybe (fromMaybe)
import Data.ProtoLens.Encoding qualified as Proto
import Data.ProtoLens.Message (Message (defMessage))
import Data.ProtoLens.Service.Types (HasMethodImpl (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Otlp.Environment (Compression (..))
import GHC.Eventlog.Live.Otlp.Environment qualified as E (Endpoint (..), ExporterOptions (..), OtlpExporterOptions (..), PerSignal (..), Protocol (..), Timeout (..), defaultPortFor)
import GHC.IsList qualified as IsList
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Compression qualified as G
import Network.GRPC.Common.Protobuf (Protobuf, StreamingType (..))
import Network.GRPC.Common.Protobuf qualified as G
import Network.GRPC.Common.StreamType qualified as G
import Network.HTTP.Client qualified as H
import Network.HTTP.Client.TLS qualified as H
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import OpenTelemetry.Baggage qualified as Baggage

--------------------------------------------------------------------------------
-- OTLP Exporter
--------------------------------------------------------------------------------

data Exporter
  = Exporter'Console
  | Exporter'OtlpGrpc !OtlpGrpcExporter
  | Exporter'OtlpHttpProtobuf !OtlpHttpProtobufExporter

{- |
Construct one shared t`OtlpExporter` or one t`OtlpExporter` per signal.
-}
withExporters ::
  Logger IO ->
  E.PerSignal (Maybe E.ExporterOptions) ->
  (E.PerSignal (Maybe Exporter) -> IO a) ->
  IO a
withExporters logger (E.Shared maybeOptions) action =
  withMaybeExporter logger maybeOptions $ action . E.Shared
withExporters logger E.PerSignal{..} action =
  withMaybeExporter logger forTRACES $ \exporterForTRACES ->
    withMaybeExporter logger forMETRICS $ \exporterForMETRICS ->
      withMaybeExporter logger forLOGS $ \exporterForLOGS ->
        withMaybeExporter logger forPROFILES $ \exporterForPROFILES ->
          action $ E.PerSignal exporterForTRACES exporterForMETRICS exporterForLOGS exporterForPROFILES

{- |
Construct a @Maybe t`Exporter`@ from @Maybe t`OtlpExporterOptions`@.
-}
withMaybeExporter ::
  Logger IO ->
  Maybe E.ExporterOptions ->
  (Maybe Exporter -> IO a) ->
  IO a
withMaybeExporter logger maybeOptions action =
  case maybeOptions of
    Nothing ->
      action Nothing
    Just options ->
      withExporter logger options $ action . Just

{- |
Construct an t`Exporter` from t`ExporterOptions`.
-}
withExporter ::
  Logger IO ->
  E.ExporterOptions ->
  (Exporter -> IO a) ->
  IO a
withExporter logger options action =
  case options of
    E.ExporterOptions'Console ->
      action Exporter'Console
    E.ExporterOptions'Otlp otlpExporterOptions ->
      case otlpExporterOptions.protocol of
        E.Grpc ->
          withOtlpGrpcExporter logger otlpExporterOptions $ action . Exporter'OtlpGrpc
        E.HttpProtobuf ->
          withOtlpHttpProtobufExporter logger otlpExporterOptions $ action . Exporter'OtlpHttpProtobuf

{- |
Export telemetry data to the t`OtlpExporter`.
-}
export ::
  forall serv meth.
  ( CanExportToConsole serv meth
  , CanExportToOltpViaGrpc serv meth
  , CanExportToOltpViaHttpProtobuf serv meth
  ) =>
  Logger IO ->
  -- | The HTTP/Protobuf exporter.
  Exporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
export logger = \case
  Exporter'Console -> \req -> do
    TIO.putStrLn (displayExportRequest @serv @meth req)
    pure (makeExportResponse @serv @meth req)
  Exporter'OtlpGrpc exporter ->
    exportGrpc @serv @meth logger exporter
  Exporter'OtlpHttpProtobuf exporter ->
    exportHttpProtobuf @serv @meth logger exporter

--------------------------------------------------------------------------------
-- Console Exporter
--------------------------------------------------------------------------------

class CanExportToConsole serv meth where
  {- |
  Display an export request message.
  -}
  displayExportRequest :: MethodInput serv meth -> Text
  default displayExportRequest :: (Show (MethodInput serv meth)) => MethodInput serv meth -> Text
  displayExportRequest = T.pack . show
  {-# INLINE displayExportRequest #-}

  {- |
  Construct an export response.
  -}
  makeExportResponse :: MethodInput serv meth -> MethodOutput serv meth
  default makeExportResponse :: (Message (MethodOutput serv meth)) => MethodInput serv meth -> MethodOutput serv meth
  makeExportResponse = const defMessage
  {-# INLINE makeExportResponse #-}

--------------------------------------------------------------------------------
-- OTLP gRPC Exporter
--------------------------------------------------------------------------------

{- |
An opaque OTLP gRPC exporter.
-}
newtype OtlpGrpcExporter = OtlpGrpcExporter
  { connection :: G.Connection
  }

type CanExportToOltpViaGrpc serv meth =
  ( G.SupportsClientRpc (Protobuf serv meth)
  , G.SupportsStreamingType (Protobuf serv meth) 'NonStreaming
  , G.RequestMetadata (Protobuf serv meth) ~ G.NoMetadata
  )

withOtlpGrpcExporter ::
  Logger IO ->
  E.OtlpExporterOptions ->
  (OtlpGrpcExporter -> IO a) ->
  IO a
withOtlpGrpcExporter logger options action = do
  writeLog logger DEBUG . T.pack $
    "OTLP gRPC Exporter - Endpoint: " <> show options.endpoint
  let !connParams =
        G.def
          { -- The compression negotiation strategy:
            G.connCompression =
              case options.maybeCompression of
                Nothing -> G.none
                Just GZip -> G.only G.gzip
          , -- The default timeout:
            G.connDefaultTimeout =
              if options.timeout.milliseconds == 0
                then Nothing
                else Just (G.Timeout G.Millisecond (G.TimeoutValue options.timeout.milliseconds))
          }
  G.withConnection connParams server $ \connection ->
    action OtlpGrpcExporter{..}
 where
  server :: G.Server
  server
    | options.endpoint.secure = G.ServerSecure serverValidation G.SslKeyLogNone address
    | otherwise = G.ServerInsecure address
   where
    port = fromIntegral $ fromMaybe (E.defaultPortFor options.protocol) options.endpoint.port
    address = G.Address options.endpoint.host port Nothing
    serverValidation = G.ValidateServer $ maybe G.certStoreFromSystem G.certStoreFromPath options.maybeCertificate

exportGrpc ::
  forall serv meth.
  (CanExportToOltpViaGrpc serv meth) =>
  Logger IO ->
  OtlpGrpcExporter ->
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
exportGrpc _logger grpcExporter input =
  let callParams :: G.CallParams (G.Protobuf serv meth)
      callParams = G.def
   in G.getProto <$> G.nonStreaming grpcExporter.connection (G.rpcWith callParams) (G.Proto input)

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
  , maybeCompression :: Maybe Compression
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
      "OTLP HTTP/Protobuf Exporter - HTTP Response: "
        <> show statusCode
        <> " "
        <> BSC.unpack statusMessage
        <> " with body: "
        <> BSC.unpack responseBody
    HttpDecodeError{..} ->
      "OTLP HTTP/Protobuf Exporter - Malformed HTTP Response: "
        <> errorMessage

{- |
Internal helper.

Run an action with an t`OtlpHttpProtobufExporter`.
-}
withOtlpHttpProtobufExporter ::
  Logger IO ->
  E.OtlpExporterOptions ->
  (OtlpHttpProtobufExporter -> IO a) ->
  IO a
withOtlpHttpProtobufExporter logger options action = do
  writeLog logger DEBUG . T.pack $
    "OTLP HTTP/Protobuf Exporter - Endpoint: " <> show options.endpoint
  -- Create HTTP manager settings.
  let responseTimeout
        | options.timeout.microseconds == 0 =
            H.responseTimeoutNone
        | options.timeout.microseconds <= fromIntegral (maxBound @Int) =
            H.responseTimeoutMicro (fromIntegral options.timeout.microseconds)
        | otherwise =
            -- NOTE: Oh no, this truncates the response timeout to 292,271 years!
            H.responseTimeoutMicro maxBound
  -- Create an HTTP manager.
  --
  -- NOTE: newTlsManagerWith respects proxy environment variables
  manager <-
    H.newTlsManagerWith H.tlsManagerSettings{H.managerResponseTimeout = responseTimeout}
  -- Create the HTTP headers.
  writeLog logger TRACE . T.pack $
    "OTLP HTTP/Protobuf Exporter - Headers: " <> show options.maybeHeaders
  let headers =
        [ (CI.mk (Baggage.tokenValue token), TE.encodeUtf8 value)
        | (token, Baggage.Element value _properties) <-
            IsList.toList (maybe mempty Baggage.values options.maybeHeaders)
        ]
  -- Run the action.
  action
    OtlpHttpProtobufExporter
      { baseUrl = show options.endpoint
      , maybeCompression = options.maybeCompression
      , ..
      }

class
  ( Message (MethodInput serv meth)
  , Message (MethodOutput serv meth)
  ) =>
  CanExportToOltpViaHttpProtobuf serv meth
  where
  apiPath :: String

{- |
Send a Protobuf message over an HTTP connection.
-}
exportHttpProtobuf ::
  forall serv meth.
  (CanExportToOltpViaHttpProtobuf serv meth) =>
  Logger IO ->
  -- | The HTTP/Protobuf exporter.
  OtlpHttpProtobufExporter ->
  -- | The request message.
  MethodInput serv meth ->
  IO (MethodOutput serv meth)
exportHttpProtobuf logger OtlpHttpProtobufExporter{..} req = do
  baseRequest <- H.parseRequest (baseUrl <> apiPath @serv @meth)
  let (compressionHeaders, compress) = httpCompression maybeCompression
  let !requestBody = H.RequestBodyBS (compress (Proto.encodeMessage req))
  let request =
        baseRequest
          { H.method = "POST"
          , H.requestBody = requestBody
          , H.checkResponse = \_ _ -> pure ()
          , H.requestHeaders =
              [ (HTTP.hContentType, "application/x-protobuf")
              , (HTTP.hAccept, "application/x-protobuf")
              ]
                <> headers -- user-provided
                <> compressionHeaders
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

{- |
Internal helper.

Determine HTTP compression headers and algorithms.
-}
httpCompression :: Maybe Compression -> ([HTTP.Header], ByteString -> ByteString)
httpCompression = \case
  Nothing -> ([], id)
  Just GZip -> ([(HTTP.hContentEncoding, "gzip")], BSL.toStrict . GZip.compress . BSL.fromStrict)
