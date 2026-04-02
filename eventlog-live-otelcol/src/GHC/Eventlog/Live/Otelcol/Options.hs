module GHC.Eventlog.Live.Otelcol.Options (
  Options (..),
  MyDebugOptions (..),
  ServiceName (..),
  OpenTelemetryCollectorOptions (..),
  options,
) where

import Control.Applicative (asum)
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
import Network.GRPC.Client qualified as G
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
-- OpenTelemetry Collector configuration

newtype OpenTelemetryCollectorOptions = OpenTelemetryCollectorOptions
  { openTelemetryCollectorServer :: G.Server
  }

openTelemetryCollectorOptionsParser :: O.Parser OpenTelemetryCollectorOptions
openTelemetryCollectorOptionsParser =
  OC.parserOptionGroup "OpenTelemetry Collector Server Options" $
    OpenTelemetryCollectorOptions
      <$> otelcolServerParser

otelcolServerParser :: O.Parser G.Server
otelcolServerParser =
  makeServer
    <$> otelcolAddressParser
    <*> O.switch (O.long "otelcol-ssl" <> O.help "Use SSL.")
    <*> otelcolServerValidationParser
    <*> otelcolSslKeyLogParser
 where
  makeServer :: G.Address -> Bool -> G.ServerValidation -> G.SslKeyLog -> G.Server
  makeServer address ssl serverValidation sslKeyLog
    | ssl = G.ServerSecure serverValidation sslKeyLog address
    | otherwise = G.ServerInsecure address

otelcolAddressParser :: O.Parser G.Address
otelcolAddressParser =
  G.Address
    <$> O.strOption
      ( O.long "otelcol-host"
          <> O.metavar "HOST"
          <> O.help "Otelcol server hostname."
      )
    <*> O.option
      O.auto
      ( O.long "otelcol-port"
          <> O.metavar "PORT"
          <> O.help "Otelcol server TCP port."
          <> O.value 4317
      )
    <*> O.optional
      ( O.strOption
          ( O.long "otelcol-authority"
              <> O.metavar "HOST"
              <> O.help "Otelcol server authority."
          )
      )

otelcolServerValidationParser :: O.Parser G.ServerValidation
otelcolServerValidationParser =
  asum
    [ G.ValidateServer <$> otelcolCertificateStoreSpecParser
    , pure G.NoServerValidation
    ]
 where
  otelcolCertificateStoreSpecParser :: O.Parser G.CertificateStoreSpec
  otelcolCertificateStoreSpecParser =
    makeCertificateStoreSpec
      <$> O.optional
        ( O.strOption
            ( O.long "otelcol-certificate-store"
                <> O.metavar "FILE"
                <> O.help "Store for certificate validation."
            )
        )
   where
    makeCertificateStoreSpec :: Maybe FilePath -> G.CertificateStoreSpec
    makeCertificateStoreSpec = maybe G.certStoreFromSystem G.certStoreFromPath

otelcolSslKeyLogParser :: O.Parser G.SslKeyLog
otelcolSslKeyLogParser =
  asum
    [ G.SslKeyLogPath
        <$> O.strOption
          ( O.long "otelcol-ssl-key-log"
              <> O.metavar "FILE"
              <> O.help "Use file to log SSL keys."
          )
    , O.flag
        G.SslKeyLogNone
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
