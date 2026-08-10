module GHC.Eventlog.Live.Otlp.Options (
  Options (..),
  MyDebugOptions (..),
  options,
) where

import Data.Char (isSpace)
import Data.Default (Default (..))
import Data.List qualified as L
import Data.Text qualified as T
import Data.Version (showVersion)
import GHC.Debug.Stub.Compat (MyGhcDebugSocket, maybeMyGhcDebugSocketParser)
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Config.Default.Raw (defaultConfigJSONSchemaString, defaultConfigString)
import GHC.Eventlog.Live.Otlp.Config.Types (Config)
import GHC.Eventlog.Live.Otlp.Control (ControlOptions, controlOptionsParser)
import GHC.Eventlog.Live.Otlp.Options.Raw (footerString, headerString, progDescString)
import GHC.Eventlog.Live.Source.Core (EventlogSourceOptions (..))
import GHC.Eventlog.Socket.Compat (MyEventlogSocket (..), maybeMyEventlogSocketParser)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Options.Applicative qualified as O
import Options.Applicative.Compat qualified as OC
import Options.Applicative.Extra qualified as OE
import Options.Applicative.Help.Chunk qualified as OHC
import Options.Applicative.Help.Pretty qualified as OHP
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
    ( O.headerDoc (helpDoc headerString)
        <> O.progDescDoc (helpDoc progDescString)
        <> O.footerDoc (helpDoc footerString)
    )

{- |
Internal helper.

Render a text as an `OHP.Doc`.
-}
helpDoc :: String -> Maybe OHP.Doc
helpDoc doc
  | OHC.isEmpty (OHC.vcatChunks docChunks) = Nothing
  | otherwise = OHC.unChunk $ vcatChunks docChunks
 where
  docChunks :: [OHC.Chunk OHP.Doc]
  docChunks = helpDocLine <$> lines doc

  -- NOTE: Variant of vcatChunks that uses hardline
  vcatChunks :: [OHC.Chunk OHP.Doc] -> OHC.Chunk OHP.Doc
  vcatChunks = mconcat . L.intersperse (OHC.Chunk $ Just OHP.hardline)

{- |
Internal helper.

Internal accumulated state for `helpDocLine`.
-}
data LineAcc
  = Space {count :: !Int, chunk :: !(OHC.Chunk OHP.Doc)}
  | Token {token :: !String, chunk :: !(OHC.Chunk OHP.Doc)}

{- |
Internal helper.

Render a line as an `OHP.Doc`.
-}
helpDocLine :: String -> OHC.Chunk OHP.Doc
helpDocLine =
  asDoc True . L.foldl' trans empty . reverse
 where
  empty :: LineAcc
  empty = Space 0 mempty

  trans :: LineAcc -> Char -> LineAcc
  trans la@Space{..} c
    | isSpace c = Space{count = count + 1, ..}
    | count >= 2 = Token{token = [c], chunk = asDoc False la}
    | otherwise = Token{token = [c], ..}
  trans la@Token{..} c
    | isSpace c = Space{count = 1, chunk = asDoc False la}
    | otherwise = Token{token = c : token, ..}

  asDoc :: Bool -> LineAcc -> OHC.Chunk OHP.Doc
  asDoc isFinal = \case
    Space{..} ->
      OHP.indent (if isFinal then count else count - 1) <$> chunk
    Token{..} ->
      OHC.stringChunk token OHC.<</>> chunk

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
  OC.parserOptionGroup "Debug options:" $
    MyDebugOptions
      <$> maybeMyEventlogSocketParser
      <*> maybeMyGhcDebugSocketParser
