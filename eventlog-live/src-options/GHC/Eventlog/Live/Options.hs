module GHC.Eventlog.Live.Options (
  eventlogSocketParser,
  heapProfBreakdownParser,
  warnMissingHeapProfBreakdown,
  eventlogLogFileParser,
  batchIntervalParser,
) where

import GHC.Eventlog.Live (EventlogSocket (..))
import GHC.RTS.Events (HeapProfBreakdown (..))
import Options.Applicative qualified as O
import System.IO qualified as IO

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
            <> O.help "Eventlog source Unix socket."
        )

--------------------------------------------------------------------------------
-- Heap Profile Breakdown

heapProfBreakdownParser :: O.Parser HeapProfBreakdown
heapProfBreakdownParser =
  O.option
    (O.eitherReader heapProfBreakdownEitherReader)
    ( O.short 'h'
        <> O.metavar "HEAP_PROFILE_BREAKDOWN"
        <> O.help "Heap profile breakdown (Tcmdyrbi)"
    )

heapProfBreakdownEitherReader :: String -> Either String HeapProfBreakdown
heapProfBreakdownEitherReader = \case
  "T" -> Right HeapProfBreakdownClosureType
  "c" -> Right HeapProfBreakdownCostCentre
  "m" -> Right HeapProfBreakdownModule
  "d" -> Right HeapProfBreakdownClosureDescr
  "y" -> Right HeapProfBreakdownTypeDescr
  "e" -> Left "Unsupported heap profile breakdown by era."
  "r" -> Right HeapProfBreakdownRetainer
  "b" -> Right HeapProfBreakdownBiography
  "i" -> Right HeapProfBreakdownInfoTable
  str -> Left $ "Unsupported heap profile breakdown -h" <> str

warnMissingHeapProfBreakdown :: IO ()
warnMissingHeapProfBreakdown =
  IO.hPutStrLn IO.stderr $
    "Warning: Cannot infer heap profile breakdown.\n\
    \         If your binary was compiled with a GHC version prior to 9.14,\n\
    \         you must also pass the heap profile type to this executable.\n\
    \         See: https://gitlab.haskell.org/ghc/ghc/-/commit/76d392a"

--------------------------------------------------------------------------------
-- Eventlog Log File

eventlogLogFileParser :: O.Parser FilePath
eventlogLogFileParser =
  O.strOption
    ( O.long "eventlog-log-file"
        <> O.metavar "FILE"
        <> O.help "If provided, the binary eventlog data is logged to this file."
    )

--------------------------------------------------------------------------------
-- Batch Interval

batchIntervalParser :: O.Parser Int
batchIntervalParser =
  O.option
    O.auto
    ( O.long "batch-interval"
        <> O.metavar "BATCH_INTERVAL"
        <> O.help "Batch interval in microseconds."
        <> O.value defaultBatchIntervalMs
    )

defaultBatchIntervalMs :: Int
defaultBatchIntervalMs = 1_000
