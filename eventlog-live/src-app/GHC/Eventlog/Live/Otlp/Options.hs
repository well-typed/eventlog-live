module GHC.Eventlog.Live.Otlp.Options (
  Options (..),
  MyDebugOptions (..),
  ServiceName (..),
  OtlpExporterOptions (..),
  OtlpProtocol (..),
  options,
) where

import Data.Char (toLower)
import Data.Default (Default (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Version (showVersion)
import GHC.Debug.Stub.Compat (MyGhcDebugSocket, maybeMyGhcDebugSocketParser)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Otlp.Config (ServiceName (..))
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Config.Default.Raw (defaultConfigJSONSchemaString, defaultConfigString)
import GHC.Eventlog.Live.Otlp.Config.Types (Config)
import GHC.Eventlog.Live.Otlp.Control (ControlOptions, controlOptionsParser)
import GHC.Eventlog.Live.Source.Core (EventlogSourceOptions (..))
import GHC.Eventlog.Socket.Compat (MyEventlogSocket (..), maybeMyEventlogSocketParser)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Network.GRPC.Common qualified as G
import OpenTelemetry.Baggage (Baggage, decodeBaggageHeader)
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra qualified as OE
import Options.Applicative.Help.Pretty qualified as OP
import Paths_eventlog_live qualified as EventlogLive

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
  , otlpExporterOptions :: OtlpExporterOptions String
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
    <*> otlpExporterOptionsParser
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

data OtlpExporterOptions a = OtlpExporterOptions
  { otlpProtocol :: !OtlpProtocol
  , otlpEndpoint :: !a
  , otlpGrpcCertificateStore :: !(Maybe FilePath)
  , otlpGrpcSslKeyLog :: !(Maybe G.SslKeyLog)
  , otlpHttpHeaders :: !(Maybe Baggage)
  }
  deriving stock (Functor, Foldable, Traversable)

otlpExporterOptionsParser :: O.Parser (OtlpExporterOptions String)
otlpExporterOptionsParser =
  OC.parserOptionGroup "OTLP Exporter Options" $
    OtlpExporterOptions
      <$> otlpProtocolParser
      <*> otlpEndpointParser
      <*> O.optional otlpGrpcCertificateStoreParser
      <*> O.optional otlpGrpcSslKeyLogParser
      <*> O.optional otlpHttpHeadersParser

data OtlpProtocol
  = OtlpProtocolGrpc
  | OtlpProtocolHttpProtobuf
  deriving (Show)

otlpProtocolParser :: O.Parser OtlpProtocol
otlpProtocolParser =
  O.option (O.maybeReader readOtlpProtocol) . mconcat $
    [ O.long "otlp-protocol"
    , O.helpDoc . Just . OP.vcat . fmap OP.pretty $
        [ "The OTLP transport protocol to be used for all telemetry data (gRPC, HTTP/Protobuf)."
        , "Default value: gRPC"
        ]
    , O.value OtlpProtocolGrpc
    ]
 where
  readOtlpProtocol :: String -> Maybe OtlpProtocol
  readOtlpProtocol protocol =
    case map toLower protocol of
      "grpc" -> Just OtlpProtocolGrpc
      "http/protobuf" -> Just OtlpProtocolHttpProtobuf
      _ -> Nothing

otlpEndpointParser :: O.Parser String
otlpEndpointParser =
  O.strOption . mconcat $
    [ O.long "otlp-endpoint"
    , O.helpDoc . Just . OP.vcat . fmap OP.pretty $
        [ "The OTLP endpoint URL for all telemetry data, with an optionally-specified port number."
        , "Default value:"
        , "  gRPC: http://localhost:4317"
        , "  HTTP: http://localhost:4318"
        , "Example:"
        , "  gRPC: https://my-api-endpoint:443"
        , "  HTTP: http://my-api-endpoint/"
        ]
    ]

otlpGrpcCertificateStoreParser :: O.Parser FilePath
otlpGrpcCertificateStoreParser =
  O.strOption
    ( O.long "otlp-grpc-certificate-store"
        <> O.metavar "FILE"
        <> O.help "Store for certificate validation."
    )

otlpGrpcSslKeyLogParser :: O.Parser G.SslKeyLog
otlpGrpcSslKeyLogParser =
  O.asum
    [ G.SslKeyLogPath
        <$> O.strOption
          ( O.long "otlp-grpc-ssl-key-log"
              <> O.metavar "FILE"
              <> O.help "Use file to log SSL keys."
          )
    , O.flag'
        G.SslKeyLogFromEnv
        ( O.long "otlp-grpc-ssl-key-log-from-env"
            <> O.help "Use SSLKEYLOGFILE to log SSL keys."
        )
    ]

otlpHttpHeadersParser :: O.Parser Baggage
otlpHttpHeadersParser =
  O.option (O.eitherReader readHeaders) . mconcat $
    [ O.long "otlp-http-headers"
    , O.help "A list of headers to apply to all outgoing data."
    ]

readHeaders :: String -> Either String Baggage
readHeaders = decodeBaggageHeader . TE.encodeUtf8 . T.pack

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
