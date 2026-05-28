module GHC.Eventlog.Live.Otelcol.Options (
  Options (..),
  MyDebugOptions (..),
  ServiceName (..),
  OpenTelemetryCollectorOptions (..),
  OpenTelemetryCollectorProtocol (..),
  HttpProtobufOptions (..),
  HttpHeader (..),
  options,
) where

import Data.Default (Default (..))
import Data.Text qualified as T
import Data.Version (showVersion)
import GHC.Debug.Stub.Compat (MyGhcDebugSocket, maybeMyGhcDebugSocketParser)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Otelcol.Config (ServiceName (..))
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Default.Raw (defaultConfigJSONSchemaString, defaultConfigString)
import GHC.Eventlog.Live.Otelcol.Config.Types (Config)
import GHC.Eventlog.Live.Otelcol.Control (ControlOptions, controlOptionsParser)
import GHC.Eventlog.Live.Source.Core (EventlogSourceOptions (..))
import GHC.Eventlog.Socket.Compat (MyEventlogSocket (..), maybeMyEventlogSocketParser)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Network.GRPC.Common qualified as G
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra qualified as OE
import Paths_eventlog_live_otelcol qualified as EventlogLive

options :: O.ParserInfo Options
options =
  O.info
    ( optionsParser
        O.<**> defaultsPrinter
        O.<**> debugDefaultsPrinter
        O.<**> configJSONSchemaPrinter
        O.<**> OE.helperWith (O.long "help" <> O.help "Show this help text.")
        O.<**> OC.simpleVersioner (showVersion EventlogLive.version)
    )
    O.idm

data Options = Options
  { eventlogSourceOptions :: EventlogSourceOptions
  , eventlogSocketTimeoutS :: Double
  , eventlogSocketTimeoutExponent :: Double
  , eventlogFlushIntervalS :: Double
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , maybeServiceName :: Maybe ServiceName
  , maybeIpeDBPath :: Maybe FilePath
  , maybeCCDBPath :: Maybe FilePath
  , severityThreshold :: Severity
  , stats :: Bool
  , maybeConfigFile :: Maybe FilePath
  , openTelemetryCollectorOptions :: OpenTelemetryCollectorOptions
  , controlOptions :: ControlOptions
  , myDebugOptions :: MyDebugOptions
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSourceOptionsParser
    <*> eventlogSocketTimeoutSParser
    <*> eventlogSocketTimeoutExponentParser
    <*> eventlogFlushIntervalSParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> O.optional serviceNameParser
    <*> O.optional ipeDBPathParser
    <*> O.optional ccDBPathParser
    <*> verbosityParser
    <*> statsParser
    <*> O.optional configFileParser
    <*> openTelemetryCollectorOptionsParser
    <*> controlOptionsParser
    <*> myDebugOptionsParser

--------------------------------------------------------------------------------
-- Configuration

configFileParser :: O.Parser FilePath
configFileParser =
  O.strOption
    ( O.long "config"
        <> O.metavar "FILE"
        <> O.help "The path to a detailed configuration file."
    )

defaultsPrinter :: O.Parser (a -> a)
defaultsPrinter =
  O.infoOption defaultConfigString . mconcat $
    [ O.long "print-defaults"
    , O.help "Print default configuration options."
    ]

configJSONSchemaPrinter :: O.Parser (a -> a)
configJSONSchemaPrinter =
  O.infoOption defaultConfigJSONSchemaString . mconcat $
    [ O.long "print-config-json-schema"
    , O.help "Print JSON Schema for configuration format."
    ]

debugDefaultsPrinter :: O.Parser (a -> a)
debugDefaultsPrinter =
  O.infoOption defaultConfigDebugString . mconcat $
    [ O.long "print-defaults-debug"
    , O.help "Print default configuration options using the parsed representation."
    , O.internal
    ]
 where
  defaultConfigDebugString =
    T.unpack . C.prettyConfig $ (def :: Config)

--------------------------------------------------------------------------------
-- Service Name

serviceNameParser :: O.Parser ServiceName
serviceNameParser =
  ServiceName
    <$> O.strOption
      ( O.long "service-name"
          <> O.metavar "STRING"
          <> O.help "The name of the profiled service."
      )

--------------------------------------------------------------------------------
-- InfoProv Tables

ipeDBPathParser :: O.Parser FilePath
ipeDBPathParser =
  O.strOption
    ( O.long "ipedb"
        <> O.metavar "FILE"
        <> O.help "The path to an IPE database."
    )

--------------------------------------------------------------------------------
-- CostCentre Tables

ccDBPathParser :: O.Parser FilePath
ccDBPathParser =
  O.strOption
    ( O.long "ccdb"
        <> O.metavar "FILE"
        <> O.help "The path a cost-centre database."
    )

--------------------------------------------------------------------------------
-- OpenTelemetry Collector configuration

data OpenTelemetryCollectorOptions = OpenTelemetryCollectorOptions
  { openTelemetryCollectorProtocol :: OpenTelemetryCollectorProtocol
  , otelcolHost :: String
  , maybeOtelcolPort :: Maybe Int
  , maybeOtelcolAuthority :: Maybe String
  , otelcolSsl :: Bool
  , maybeOtelcolCertificateStore :: Maybe FilePath
  , maybeOtelcolSslKeyLog :: Maybe G.SslKeyLog
  , otelcolHeaders :: [HttpHeader]
  }

data OpenTelemetryCollectorProtocol
  = OpenTelemetryCollectorProtocol'Grpc
  | OpenTelemetryCollectorProtocol'HttpProtobuf
  deriving (Eq, Show)

data HttpProtobufOptions = HttpProtobufOptions
  { httpProtobufScheme :: String
  , httpProtobufHost :: String
  , httpProtobufPort :: Int
  , httpProtobufHeaders :: [HttpHeader]
  }
  deriving (Show)

data HttpHeader = HttpHeader
  { httpHeaderName :: String
  , httpHeaderValue :: String
  }
  deriving (Show)

openTelemetryCollectorOptionsParser :: O.Parser OpenTelemetryCollectorOptions
openTelemetryCollectorOptionsParser =
  OC.parserOptionGroup "OpenTelemetry Collector Server Options" $
    OpenTelemetryCollectorOptions
      <$> otelcolProtocolParser
      <*> otelcolHostParser
      <*> O.optional otelcolPortParser
      <*> O.optional otelcolAuthorityParser
      <*> O.switch (O.long "otelcol-ssl" <> O.help "Use SSL.")
      <*> O.optional otelcolCertificateStoreParser
      <*> O.optional otelcolSslKeyLogParser
      <*> O.many otelcolHeaderParser

otelcolProtocolParser :: O.Parser OpenTelemetryCollectorProtocol
otelcolProtocolParser =
  O.option
    ( O.eitherReader $ \case
        "grpc" -> Right OpenTelemetryCollectorProtocol'Grpc
        "http-protobuf" -> Right OpenTelemetryCollectorProtocol'HttpProtobuf
        protocol -> Left $ "Unknown OpenTelemetry Collector protocol: " <> protocol
    )
    ( O.long "otelcol-protocol"
        <> O.metavar "grpc|http-protobuf"
        <> O.help "OpenTelemetry Collector protocol."
        <> O.value OpenTelemetryCollectorProtocol'Grpc
        <> O.showDefaultWith (const "grpc")
    )

otelcolHostParser :: O.Parser String
otelcolHostParser =
  O.strOption
    ( O.long "otelcol-host"
        <> O.metavar "HOST"
        <> O.help "Otelcol server hostname."
    )

otelcolPortParser :: O.Parser Int
otelcolPortParser =
  O.option
    O.auto
    ( O.long "otelcol-port"
        <> O.metavar "PORT"
        <> O.help "Otelcol server TCP port. Defaults to 4317 for gRPC and 4318 for HTTP/protobuf."
    )

otelcolAuthorityParser :: O.Parser String
otelcolAuthorityParser =
  O.strOption
    ( O.long "otelcol-authority"
        <> O.metavar "HOST"
        <> O.help "Otelcol server authority."
    )

otelcolCertificateStoreParser :: O.Parser FilePath
otelcolCertificateStoreParser =
  O.strOption
    ( O.long "otelcol-certificate-store"
        <> O.metavar "FILE"
        <> O.help "Store for certificate validation."
    )

otelcolHeaderParser :: O.Parser HttpHeader
otelcolHeaderParser =
  O.option
    ( O.eitherReader $ \header ->
        case break (== '=') header of
          ("", _) -> Left "Header name must not be empty."
          (_, "") -> Left "Header must have the form NAME=VALUE."
          (httpHeaderName, _ : httpHeaderValue) -> Right HttpHeader{..}
    )
    ( O.long "otelcol-header"
        <> O.metavar "NAME=VALUE"
        <> O.help "Add an HTTP header for OTLP HTTP/protobuf. May be repeated."
    )

otelcolSslKeyLogParser :: O.Parser G.SslKeyLog
otelcolSslKeyLogParser =
  O.asum
    [ G.SslKeyLogPath
        <$> O.strOption
          ( O.long "otelcol-ssl-key-log"
              <> O.metavar "FILE"
              <> O.help "Use file to log SSL keys."
          )
    , O.flag'
        G.SslKeyLogFromEnv
        ( O.long "otelcol-ssl-key-log-from-env"
            <> O.help "Use SSLKEYLOGFILE to log SSL keys."
        )
    ]

--------------------------------------------------------------------------------
-- Debug Options

data MyDebugOptions = MyDebugOptions
  { maybeMyEventlogSocket :: Maybe MyEventlogSocket
  , maybeMyGhcDebugSocket :: Maybe MyGhcDebugSocket
  }

myDebugOptionsParser :: O.Parser MyDebugOptions
myDebugOptionsParser =
  OC.parserOptionGroup "Debug Options" $
    MyDebugOptions
      <$> maybeMyEventlogSocketParser
      <*> maybeMyGhcDebugSocketParser
