module Main (main) where

import Data.Machine (ProcessT)
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..))
import GHC.Eventlog.Live
import GHC.Eventlog.Live.Machines (batchByTick)
import GHC.Eventlog.Live.Options
import Options.Applicative qualified as O
import System.Clock (TimeSpec)
import System.Clock qualified as Clock

main :: IO ()
main = do
  Options {..} <- O.execParser options
  putStrLn "Hello, Wen!"

--------------------------------------------------------------------------------
-- Machines
--------------------------------------------------------------------------------

getStartTime :: ProcessT m Event (a, Maybe Clock.TimeSpec)
getStartTime = undefined

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

options :: O.ParserInfo Options
options = O.info (optionsParser O.<**> O.helper) O.idm

data Options = Options
  { eventlogSocket :: EventlogSocket
  , batchInterval :: Int
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , openTelemetryCollectorOptions :: OpenTelemetryCollectorOptions
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSocketParser
    <*> batchIntervalParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> openTelemetryCollectorOptionsParser

--------------------------------------------------------------------------------
-- OpenTelemetry Collector Configuration

data OpenTelemetryCollectorOptions = OpenTelemetryCollectorOptions

openTelemetryCollectorOptionsParser :: O.Parser OpenTelemetryCollectorOptions
openTelemetryCollectorOptionsParser = pure OpenTelemetryCollectorOptions
