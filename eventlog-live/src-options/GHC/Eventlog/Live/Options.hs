{- |
Module      : GHC.Eventlog.Live.Options
Description : Command-line option parsers for eventlog machines.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Options (
  EventlogSocket (..),
  eventlogSocketParser,
  eventlogSocketTimeoutParser,
  eventlogSocketTimeoutExponentParser,
  heapProfBreakdownParser,
  eventlogLogFileParser,
  batchIntervalParser,
  verbosityParser,
) where

import Data.Char (toLower)
import GHC.Eventlog.Live.Machines (heapProfBreakdownEitherReader)
import GHC.Eventlog.Live.Verbosity (Verbosity, verbosityDebug, verbosityError, verbosityInfo, verbosityQuiet, verbosityWarning)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Options.Applicative qualified as O
import Text.Read (readEither)

--------------------------------------------------------------------------------
-- Eventlog Socket

{- |
The type of eventlog sockets.
-}
newtype EventlogSocket
  = EventlogSocketUnix FilePath

{- |
Parser for the eventlog socket.
-}
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

{- |
Parser for the intial timeout for exponential backoff.
-}
eventlogSocketTimeoutParser :: O.Parser Double
eventlogSocketTimeoutParser =
  O.option
    O.auto
    ( O.long "eventlog-socket-timeout"
        <> O.metavar "NUM"
        <> O.help "Eventlog socket connection retry timeout in microseconds."
        <> O.value 1
    )

{- |
Parser for the exponent for exponential backoff.
-}
eventlogSocketTimeoutExponentParser :: O.Parser Double
eventlogSocketTimeoutExponentParser =
  O.option
    O.auto
    ( O.long "eventlog-socket-exponent"
        <> O.metavar "NUM"
        <> O.help "Eventlog socket connection retry timeout exponent."
        <> O.value 1
    )

--------------------------------------------------------------------------------
-- Heap Profile Breakdown

{- |
Parser for the heap profile breakdown.
-}
heapProfBreakdownParser :: O.Parser HeapProfBreakdown
heapProfBreakdownParser =
  O.option
    (O.eitherReader heapProfBreakdownEitherReader)
    ( O.short 'h'
        <> O.metavar "Tcmdyrbi"
        <> O.help "Heap profile breakdown."
    )

--------------------------------------------------------------------------------
-- Eventlog Log File

{- |
Parser for the eventlog log file.
-}
eventlogLogFileParser :: O.Parser FilePath
eventlogLogFileParser =
  O.strOption
    ( O.long "eventlog-log-file"
        <> O.metavar "FILE"
        <> O.help "Use file to log binary eventlog data."
    )

--------------------------------------------------------------------------------
-- Batch Interval

{- |
Parser for the batch interval.
-}
batchIntervalParser :: O.Parser Int
batchIntervalParser =
  O.option
    O.auto
    ( O.long "batch-interval"
        <> O.metavar "NUM"
        <> O.help "Batch interval in microseconds."
        <> O.value defaultBatchIntervalMs
    )

{- |
Internal helper.
The default batch interval in milliseconds.
-}
defaultBatchIntervalMs :: Int
defaultBatchIntervalMs = 1_000

--------------------------------------------------------------------------------
-- Verbosity

{- |
Parser for verbosities.
The default verbosity is `verbosityWarning`.
-}
verbosityParser :: O.Parser Verbosity
verbosityParser =
  O.option
    (O.eitherReader readEitherVerbosity)
    ( O.short 'v'
        <> O.long "verbosity"
        <> O.metavar "quiet|error|warning|info|debug|0-4"
        <> O.help "The verbosity threshold for logging."
        <> O.value verbosityWarning
    )

{- |
Internal helper.
Parser for verbosities by number or name.
Case insensitive.
-}
readEitherVerbosity :: String -> Either String Verbosity
readEitherVerbosity rawVerbosity =
  -- try to parse the verbosity as a number...
  case readEither @Word rawVerbosity of
    -- if the verbosity string is a number, map it to a verbosity...
    Right verbosityThreshold
      | verbosityThreshold <= 0 -> Right verbosityQuiet
      | verbosityThreshold == 1 -> Right verbosityError
      | verbosityThreshold == 2 -> Right verbosityWarning
      | verbosityThreshold == 3 -> Right verbosityInfo
      | otherwise -> Right verbosityDebug
    -- otherwise, match it against the literal names of the levels...
    Left _parseError -> case toLower <$> rawVerbosity of
      "quiet" -> Right verbosityQuiet
      "error" -> Right verbosityError
      "warning" -> Right verbosityWarning
      "info" -> Right verbosityInfo
      "debug" -> Right verbosityDebug
      _otherwise -> Left $ "Could not parse verbosity '" <> rawVerbosity <> "'."
