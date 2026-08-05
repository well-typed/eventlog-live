{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module GHC.Eventlog.Live.Otlp.Environment (
  -- * OpenTelemetry SDK Options
  OpenTelemetrySdkOptions (..),
  lookupOpenTelemetrySdkOptions,
  Severity (..),
  lookupLogLevel,
  ServiceName (..),
  ResourceAttributes (..),

  -- * OpenTelemetry Exporter Options
  ExporterOptions (..),

  -- ** OpenTelemetry Signals and Per-Signal Options
  PerSignal (..),
  Signal (..),
  forSignal,

  -- ** OpenTelemetry OTLP Exporter Options
  OtlpExporterOptions (..),
  Protocol (..),
  Endpoint (..),
  Compression (..),
  Timeout (..),
  defaultEndpointFor,
  defaultPortFor,
) where

import Control.Monad (join, unless)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.CaseInsensitive qualified as CI
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Hashable (Hashable)
import Data.List qualified as L
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Traversable (for)
import Data.Word (Word16)
import GHC.Eventlog.Live.Data.Severity (Severity (..), fromSeverityString)
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.IsList qualified as IsList
import GHC.Records (HasField (..))
import Network.URI (URI (..), URIAuth (..))
import Network.URI qualified as URI
import OpenTelemetry.Baggage (Baggage, decodeBaggageHeader)
import OpenTelemetry.Baggage qualified as Baggage
import System.Environment (lookupEnv)
import Text.Printf (printf)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- OpenTelemetry SDK Options
--------------------------------------------------------------------------------

{- |
The OpenTelemetry SDK configuration options.

Read these values from the environment using `lookupOpenTelemetrySdkOptions`.
This function writes to a `Logger` when it encounters configuration errors.
Hence, it does not read @OTEL_LOG_LEVEL@, which is needed to construct a logger.

See: https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables
-}
data OpenTelemetrySdkOptions = OpenTelemetrySdkOptions
  { maybeResourceAttributes :: !(Maybe ResourceAttributes)
  , exporterOptions :: !(PerSignal (Maybe ExporterOptions))
  }

lookupOpenTelemetrySdkOptions ::
  Logger IO ->
  ExceptT String IO OpenTelemetrySdkOptions
lookupOpenTelemetrySdkOptions logger = do
  maybeResourceAttributes <- lookupResourceAttributes logger
  exporterOptions <- lookupExporterOptions logger
  pure OpenTelemetrySdkOptions{..}

--------------------------------------------------------------------------------
-- OpenTelemetry Log Level

{- |
Lookup the OpenTelemetry Log Level from the environment.
-}
lookupLogLevel ::
  ExceptT String IO Severity
lookupLogLevel = do
  -- Lookup OTEL_LOG_LEVEL
  let otelLogLevel = "OTEL_LOG_LEVEL"
  fmap (fromMaybe INFO)
    <$> traverse (readSeverity otelLogLevel)
    =<< lift (lookupEnv otelLogLevel)

--------------------------------------------------------------------------------
-- OpenTelemetry Resource Attributes

{- |
OpenTelemetry Service Name.
-}
newtype ServiceName = ServiceName {serviceName :: Text}
  deriving newtype (Eq, Hashable, Show)

{- |
OpenTelemetry Resource Attributes.
-}
newtype ResourceAttributes = ResourceAttributes
  { resourceAttributeBaggage :: Baggage
  }

{- |
Internal helper.

The @service.name@ token.
-}
serviceNameToken :: Baggage.Token
serviceNameToken = [Baggage.token|service.name|]

instance HasField "serviceName" ResourceAttributes (Maybe ServiceName) where
  getField :: ResourceAttributes -> Maybe ServiceName
  getField ResourceAttributes{..} =
    ServiceName <$> Baggage.getValue serviceNameToken resourceAttributeBaggage

instance HasField "attributes" ResourceAttributes [(Text, Text)] where
  getField :: ResourceAttributes -> [(Text, Text)]
  getField ResourceAttributes{..} =
    [ (key, value)
    | (token, Baggage.Element value _properties) <-
        IsList.toList (Baggage.values resourceAttributeBaggage)
    , let key = TE.decodeUtf8 (Baggage.tokenValue token)
    ]

{- |
Lookup the OpenTelemetry Resource Attributes from the environment.

This function reads both @OTEL_RESOURCE_ATTRIBUTES@ and @OTEL_SERVICE_NAME@.
-}
lookupResourceAttributes ::
  Logger IO ->
  ExceptT String IO (Maybe ResourceAttributes)
lookupResourceAttributes logger = do
  -- Lookup OTEL_SERVICE_NAME
  let otelServiceName = "OTEL_SERVICE_NAME"
  maybeServiceName <-
    fmap (singletonBaggage serviceNameToken . T.pack)
      <$> lift (lookupEnv otelServiceName)

  -- Lookup OTEL_RESOURCE_ATTRIBUTES
  let otelResourceAttributes = "OTEL_RESOURCE_ATTRIBUTES"
  maybeResourceAttributeBaggage <-
    traverse (readBaggage logger otelResourceAttributes)
      =<< lift (lookupEnv otelResourceAttributes)

  pure $ fmap ResourceAttributes (maybeServiceName <> maybeResourceAttributeBaggage)

{- |
Internal helper.

Convert a `Text` value to singleton `Baggage`.
-}
singletonBaggage :: Baggage.Token -> Text -> Baggage
singletonBaggage token value = Baggage.insert token (Baggage.element value) Baggage.empty

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Options
--------------------------------------------------------------------------------

{- |
Supported exporters.
-}
data ExporterType = Otlp
  deriving (Eq, Show)

{- |
Lookup the OpenTelemetry Exporter Type for a signal from the environment.

See: https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables/#exporter-selection
-}
lookupExporterType ::
  Logger IO ->
  Signal ->
  ExceptT String IO (Maybe ExporterType)
lookupExporterType logger signal = do
  let otelExporter = "OTEL_" <> show signal <> "_EXPORTER"
  lift (lookupEnv otelExporter) >>= \case
    Nothing ->
      pure (Just Otlp)
    Just otelExporterType ->
      readExporterType logger otelExporter otelExporterType

{- |
Exporter options for each exporter type.
-}
newtype ExporterOptions = ExporterOptions'Otlp OtlpExporterOptions
  deriving (Eq, Show)

{- |
Lookup the OpenTelemetry Exporter options from the environment.

See: https://opentelemetry.io/docs/specs/otel/protocol/exporter
-}
lookupExporterOptions ::
  Logger IO ->
  ExceptT String IO (PerSignal (Maybe ExporterOptions))
lookupExporterOptions logger = do
  !tracesExporterType <- lookupExporterType logger TRACES
  !tracesExporterOptions <-
    for tracesExporterType $ \Otlp ->
      ExporterOptions'Otlp <$> lookupOtlpExporterOptions logger (Just TRACES)

  !metricsExporterType <- lookupExporterType logger METRICS
  !metricsExporterOptions <-
    for metricsExporterType $ \Otlp ->
      ExporterOptions'Otlp <$> lookupOtlpExporterOptions logger (Just METRICS)

  !logsExporterType <- lookupExporterType logger LOGS
  !logsExporterOptions <-
    for logsExporterType $ \Otlp ->
      ExporterOptions'Otlp <$> lookupOtlpExporterOptions logger (Just LOGS)

  !profilesExporterType <- lookupExporterType logger PROFILES
  !profilesExporterOptions <-
    for profilesExporterType $ \Otlp ->
      ExporterOptions'Otlp <$> lookupOtlpExporterOptions logger (Just PROFILES)

  let exporterOptions =
        [tracesExporterOptions, metricsExporterOptions, logsExporterOptions, profilesExporterOptions]
  pure $
    if allSame exporterOptions
      then
        Shared tracesExporterOptions
      else
        PerSignal
          { forTRACES = tracesExporterOptions
          , forMETRICS = metricsExporterOptions
          , forLOGS = logsExporterOptions
          , forPROFILES = profilesExporterOptions
          }

--------------------------------------------------------------------------------
-- OpenTelemetry OTLP Exporter Options

{- |
OTLP protocol.
-}
data Protocol = Grpc | HttpProtobuf
  deriving (Eq, Show)

instance Default Protocol where
  def :: Protocol
  def = HttpProtobuf

{- |
OTLP endpoint.
-}
data Endpoint = Endpoint
  { host :: !String
  , port :: !(Maybe Word16)
  , path :: !String
  , secure :: !Bool
  }
  deriving (Eq)

toURI :: Endpoint -> URI
toURI endpoint =
  URI.rectify $
    URI.nullURI
      { uriScheme = if endpoint.secure then "https" else "http"
      , uriAuthority = Just URI.nullURIAuth{uriRegName = endpoint.host, uriPort = maybe "" show endpoint.port}
      , uriPath = endpoint.path
      }

instance Show Endpoint where
  showsPrec :: Int -> Endpoint -> ShowS
  showsPrec p = showsPrec p . toURI

{- |
The default endpoint for each protocol.

See: https://opentelemetry.io/docs/specs/otel/protocol/exporter/#configuration-options
-}
defaultEndpointFor :: Protocol -> Endpoint
defaultEndpointFor protocol =
  Endpoint{host = "localhost", port = Just $ defaultPortFor protocol, path = "", secure = False}

{- |
The default port for each protocol.
-}
defaultPortFor :: Protocol -> Word16
defaultPortFor = \case Grpc -> 4317; HttpProtobuf -> 4318

{- |
OTLP compression.
-}
data Compression = GZip
  deriving (Eq, Show)

{- |
OTLP timeout.

The value is specified in milliseconds.
The value @0@ should be interpreted as "no timeout".
-}
newtype Timeout = Timeout {milliseconds :: Word}
  deriving (Eq, Show)

instance HasField "microseconds" Timeout Word where
  getField :: Timeout -> Word
  getField timeout = 1_000 * timeout.milliseconds

instance Default Timeout where
  def :: Timeout
  def = Timeout 10_000

{- |
OpenTelemetry OTLP Exporter options.
-}
data OtlpExporterOptions = OtlpExporterOptions
  { protocol :: !Protocol
  , endpoint :: !Endpoint
  , maybeCertificate :: !(Maybe String)
  , maybeHeaders :: !(Maybe Baggage)
  , maybeCompression :: !(Maybe Compression)
  , timeout :: !Timeout
  }
  deriving (Eq, Show)

{- |
Lookup the OpenTelemetry OTLP Exporter options from the environment.

See: https://opentelemetry.io/docs/specs/otel/protocol/exporter
-}
lookupOtlpExporterOptions ::
  Logger IO ->
  Maybe Signal ->
  ExceptT String IO OtlpExporterOptions
lookupOtlpExporterOptions logger signal = do
  maybeProtocol <- lookupOtlpExporterOption logger signal PROTOCOL readProtocol
  let !protocol = fromMaybe HttpProtobuf maybeProtocol
  -- The INSECURE option should be used to infer http/https, but whenever
  -- http/https is specified in the endpoint, this should take precedence.
  maybeInsecure <- lookupOtlpExporterOption logger signal INSECURE readBoolean
  maybeEndpoint <- lookupOtlpExporterOption logger signal ENDPOINT (readEndpoint maybeInsecure)
  let !endpoint = fromMaybe (defaultEndpointFor protocol) maybeEndpoint
  maybeCertificate <- lookupOtlpExporterOption logger signal CERTIFICATE readString
  maybeHeaders <- lookupOtlpExporterOption logger signal HEADERS readBaggage
  maybeCompression <- join <$> lookupOtlpExporterOption logger signal COMPRESSION readCompression
  maybeTimeout <- lookupOtlpExporterOption logger signal TIMEOUT readTimeout
  let timeout = fromMaybe def maybeTimeout
  pure OtlpExporterOptions{..}

{- |
Internal helper.

The exporter options supported by OTLP.
-}
data OtlpExporterOption
  = PROTOCOL
  | ENDPOINT
  | INSECURE
  | CERTIFICATE
  | HEADERS
  | COMPRESSION
  | TIMEOUT
  deriving (Show)

{- |
Internal helper.

Render an exporter option name as a string.

>>> exporterOptionName Nothing        ENDPOINT == "OTEL_EXPORTER_OTLP_ENDPOINT"
>>> exporterOptionName (Just TRACES)  ENDPOINT == "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"
>>> exporterOptionName (Just METRICS) ENDPOINT == "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT"
>>> exporterOptionName (Just LOGS)    ENDPOINT == "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT"
-}
exporterOptionName :: Maybe Signal -> OtlpExporterOption -> String
exporterOptionName signal option =
  L.intercalate "_" . catMaybes $
    [Just "OTEL_EXPORTER_OTLP", show <$> signal, Just (show option)]

{- |
Internal helper.

Look up an exporter option, cascading from signal-specific for generic options.
-}
lookupOtlpExporterOption ::
  Logger IO ->
  Maybe Signal ->
  OtlpExporterOption ->
  (Logger IO -> String -> String -> ExceptT e IO a) ->
  ExceptT e IO (Maybe a)
lookupOtlpExporterOption logger maybeSignal option parser =
  -- Look up the exporter option for the specified signal, if any.
  case maybeSignal of
    Nothing ->
      lookupOtlpExporterOptionOnlyFor Nothing
    Just signal ->
      lookupOtlpExporterOptionOnlyFor (Just signal)
        >>= maybe (lookupOtlpExporterOptionOnlyFor Nothing) (pure . Just)
 where
  -- Look up the exporter option /only/ for the specified signal.
  lookupOtlpExporterOptionOnlyFor maybeSignal' =
    let optionName = exporterOptionName maybeSignal' option
     in lift (lookupEnv optionName) >>= traverse (parser logger optionName)

{- |
Internal helper.

Parse a protocol.
-}
readProtocol :: (Monad m) => Logger m -> String -> String -> ExceptT String m Protocol
readProtocol _logger optionName protocol
  | CI.mk protocol == "grpc" = pure Grpc
  | CI.mk protocol == "http/protobuf" = pure HttpProtobuf
  | CI.mk protocol == "http/json" =
      throwE $
        "Environment variable " <> optionName <> " specifies unsupported protocol 'http/json'."
  | otherwise =
      throwE $
        "Environment variable " <> optionName <> " specifies unknown protocol '" <> protocol <> "'."

{- |
Internal helper.

Parse a boolean.

See: https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables/#boolean
-}
readBoolean :: (Monad m) => Logger m -> String -> String -> ExceptT String m Bool
readBoolean logger optionName boolean
  | CI.mk boolean == "true" = pure True
  | CI.mk boolean == "false" = pure False
  | otherwise = do
      lift . writeLog logger WARN . T.pack $
        "Environment variable " <> optionName <> " has non-boolean value '" <> boolean <> "'. Use 'true' or 'false'."
      pure False

{- |
Internal helper.

Parse a string.
-}
readString :: (Monad m) => Logger m -> String -> String -> ExceptT String m String
readString _logger _optionName = pure

{- |
Internal helper.

Show an URI.
-}
showURI :: URI -> String
showURI URI{..} =
  printf
    "URI {uriScheme = \"%s\", uriAuthority = %s, uriPath = \"%s\", uriQuery = \"%s\", uriFragment = \"%s\"}"
    uriScheme
    (show uriAuthority)
    uriPath
    uriQuery
    uriFragment

{- |
Internal helper.

Parse an endpoint.

@`readEndpoint` maybeInsecure@ uses the value of @maybeInsecure@, to determine
whether or not to infer the URI scheme as http or https, if unspecified.
-}
readEndpoint :: (Monad m) => Maybe Bool -> Logger m -> String -> String -> ExceptT String m Endpoint
readEndpoint maybeInsecure logger optionName = go True
 where
  go retry endpoint = do
    let maybeURI = URI.parseAbsoluteURI endpoint
    for_ maybeURI $ \uri ->
      lift . writeLog logger TRACE . T.pack $
        "Environment variable " <> optionName <> " specifies URI: " <> showURI uri
    case maybeURI of
      Nothing
        | retry -> do
            if maybeInsecure == Just False
              then go False ("https://" <> endpoint)
              else go False ("http://" <> endpoint)
        | otherwise ->
            throwE $ "Environment variable " <> optionName <> " specifies malformed URI '" <> endpoint <> "'."
      Just URI{uriAuthority = Nothing} -> do
        throwE $ "Environment variable " <> optionName <> " specifies URI without autority '" <> endpoint <> "'."
      Just URI{uriScheme}
        | uriScheme `notElem` ["http:", "https:"] ->
            throwE $ "Environment variable " <> optionName <> " specifies URI with unsupported scheme '" <> endpoint <> "'. Use 'http' or 'https'."
      Just URI{uriAuthority = Just URIAuth{..}, ..} -> do
        unless (null uriUserInfo) $
          lift . writeLog logger WARN . T.pack $
            "Environment variable " <> optionName <> " specifies URI with user info '" <> uriUserInfo <> "'."
        unless (null uriQuery) $
          lift . writeLog logger WARN . T.pack $
            "Environment variable " <> optionName <> " specifies URI with query '" <> uriQuery <> "'."
        unless (null uriFragment) $
          lift . writeLog logger WARN . T.pack $
            "Environment variable " <> optionName <> " specifies URI with fragment '" <> uriFragment <> "'."
        pure
          Endpoint
            { host = uriRegName
            , port = readMaybe @Word16 (dropColon uriPort)
            , path = uriPath
            , secure = uriScheme == "https:"
            }

  dropColon :: String -> String
  dropColon = \case (':' : str) -> str; str -> str

{- |
Internal helper.

Parse headers in the baggage format.
-}
readBaggage :: (Monad m) => Logger m -> String -> String -> ExceptT String m Baggage
readBaggage _logger optionName baggage =
  either onErr pure . decodeBaggageHeader . TE.encodeUtf8 . T.pack $ baggage
 where
  onErr e =
    throwE $ printf "Environment variable %s specifies malformed baggage '%s': %s" optionName baggage e

{- |
Internal helper.

Parse compression.
-}
readCompression :: (Monad m) => Logger m -> String -> String -> ExceptT String m (Maybe Compression)
readCompression _logger optionName compression
  | CI.mk compression == "none" = pure Nothing
  | CI.mk compression == "gzip" = pure (Just GZip)
  | otherwise =
      throwE $
        "Environment variable " <> optionName <> " specifies unknown compression '" <> compression <> "'. Use 'none' or 'gzip'."

{- |
Internal helper.

Parse a timeout.
-}
readTimeout :: (Monad m) => Logger m -> String -> String -> ExceptT String m Timeout
readTimeout logger optionName timeout
  | Just milliseconds <- readMaybe @Word timeout =
      pure (Timeout milliseconds)
  | otherwise = do
      lift . writeLog logger WARN . T.pack $
        "Environment variable " <> optionName <> " specifies malformed timeout '" <> timeout <> "'."
      pure def

{- |
Internal helper.

Parse a log level.

See: https://opentelemetry.io/docs/specs/otel/logs/data-model/#field-severitytext
-}
readSeverity :: (Monad m) => String -> String -> ExceptT String m Severity
readSeverity optionName logLevel
  | Just severity <- fromSeverityString logLevel = pure severity
  | otherwise =
      throwE $
        "Environment variable " <> optionName <> " specifies malformed log level '" <> logLevel <> "'."

{- |
Internal helper.

Parse an exporter type.
-}
readExporterType :: (Monad m) => Logger m -> String -> String -> ExceptT String m (Maybe ExporterType)
readExporterType _logger optionName exporterType
  | CI.mk exporterType == "none" = pure Nothing
  | CI.mk exporterType == "otlp" = pure (Just Otlp)
  | CI.mk exporterType `elem` ["zipkin", "prometheus", "console", "logging"] =
      throwE $
        "Environment variable " <> optionName <> " specifies unsupported exporter '" <> exporterType <> "'."
  | otherwise =
      throwE $
        "Environment variable " <> optionName <> " specifies unknown exporter '" <> exporterType <> "'."

{- |
Internal helper.

Check if all elements are the same.
-}
allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

--------------------------------------------------------------------------------
-- OpenTelemetry Signals and Options per Signal

{- |
The signals supported by OTLP.
-}
data Signal
  = TRACES
  | METRICS
  | LOGS
  | PROFILES
  deriving (Show, Enum, Bounded)

{- |
A collection of values for per signal.
-}
data PerSignal a
  = Shared !a
  | PerSignal
      { forTRACES :: !a
      , forMETRICS :: !a
      , forLOGS :: !a
      , forPROFILES :: !a
      }
  deriving stock (Functor, Foldable, Traversable)

{- |
Get the element for a specific signal.
-}
forSignal :: PerSignal a -> Signal -> a
forSignal = \case
  Shared a -> const a
  PerSignal{..} -> \case
    TRACES -> forTRACES
    METRICS -> forMETRICS
    LOGS -> forLOGS
    PROFILES -> forPROFILES
