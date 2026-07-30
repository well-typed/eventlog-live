module GHC.Eventlog.Live.Otlp.Options (
  Options (..),
  MyDebugOptions (..),
  options,
) where

import Data.Default (Default (..))
import Data.Text qualified as T
import Data.Version (showVersion)
import GHC.Debug.Stub.Compat (MyGhcDebugSocket, maybeMyGhcDebugSocketParser)
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Config.Default.Raw (defaultConfigJSONSchemaString, defaultConfigString)
import GHC.Eventlog.Live.Otlp.Config.Types (Config)
import GHC.Eventlog.Live.Otlp.Control (ControlOptions, controlOptionsParser)
import GHC.Eventlog.Live.Source.Core (EventlogSourceOptions (..))
import GHC.Eventlog.Socket.Compat (MyEventlogSocket (..), maybeMyEventlogSocketParser)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra qualified as OE
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
  , maybeIpeDBPath :: Maybe FilePath
  , maybeCCDBPath :: Maybe FilePath
  , stats :: Bool
  , maybeConfigFile :: Maybe FilePath
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
    <*> O.optional ipeDBPathParser
    <*> O.optional ccDBPathParser
    <*> statsParser
    <*> O.optional configFileParser
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
