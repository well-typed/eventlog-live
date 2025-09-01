module GHC.Eventlog.Live.Options (
  eventlogSocketParser,
  heapProfBreakdownParser,
  eventlogLogFileParser,
  batchIntervalParser,
  verbosityParser,
) where

import Data.Char (toLower)
import GHC.Eventlog.Live (EventlogSocket (..))
import GHC.Eventlog.Live.Machines (Verbosity, heapProfBreakdownEitherReader, verbosityError, verbosityQuiet, verbosityWarning)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Options.Applicative qualified as O
import Text.Read (readEither)

--------------------------------------------------------------------------------
-- Eventlog Socket

eventlogSocketParser :: O.Parser EventlogSocket
eventlogSocketParser = socketUnixParser
 where
  socketUnixParser =
    EventlogSocketUnix
      <$> O.strOption
        ( O.long "eventlog-socket"
            <> O.metavar "SOCKET"
            <> O.help "Eventlog Unix socket."
        )

--------------------------------------------------------------------------------
-- Heap Profile Breakdown

heapProfBreakdownParser :: O.Parser HeapProfBreakdown
heapProfBreakdownParser =
  O.option
    (O.eitherReader heapProfBreakdownEitherReader)
    ( O.short 'h'
        <> O.metavar "[Tcmdyrbi]"
        <> O.help "Heap profile breakdown."
    )

--------------------------------------------------------------------------------
-- Eventlog Log File

eventlogLogFileParser :: O.Parser FilePath
eventlogLogFileParser =
  O.strOption
    ( O.long "eventlog-log-file"
        <> O.metavar "FILE"
        <> O.help "Use file to log binary eventlog data."
    )

--------------------------------------------------------------------------------
-- Batch Interval

batchIntervalParser :: O.Parser Int
batchIntervalParser =
  O.option
    O.auto
    ( O.long "batch-interval"
        <> O.metavar "NUM"
        <> O.help "Batch interval in microseconds."
        <> O.value defaultBatchIntervalMs
    )

defaultBatchIntervalMs :: Int
defaultBatchIntervalMs = 1_000

--------------------------------------------------------------------------------
-- Verbosity

verbosityParser :: O.Parser Verbosity
verbosityParser =
  O.option
    (O.eitherReader readEitherVerbosity)
    ( O.short 'v'
        <> O.long "verbosity"
        <> O.metavar "[N|quiet|error|warning]"
        <> O.help "The verbosity threshold for logging."
        <> O.value verbosityWarning
    )

readEitherVerbosity :: String -> Either String Verbosity
readEitherVerbosity rawVerbosity =
  -- try to parse the verbosity as a number...
  case readEither @Word rawVerbosity of
    -- if the verbosity string is a number, map it to a verbosity...
    Right verbosityThreshold
      | verbosityThreshold <= 0 -> Right verbosityQuiet
      | verbosityThreshold == 1 -> Right verbosityError
      | otherwise -> Right verbosityWarning
    -- otherwise, match it against the literal names of the levels...
    Left _parseError -> case toLower <$> rawVerbosity of
      "quiet" -> Right verbosityQuiet
      "error" -> Right verbosityError
      "warning" -> Right verbosityWarning
      _otherwise -> Left $ "Could not parse verbosity '" <> rawVerbosity <> "'."
