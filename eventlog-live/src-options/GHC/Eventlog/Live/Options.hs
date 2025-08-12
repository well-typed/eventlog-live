module GHC.Eventlog.Live.Options (
  eventlogSocketParser,
  heapProfBreakdownParser,
  eventlogLogFileParser,
  batchIntervalParser,
) where

import GHC.Eventlog.Live (EventlogSocket (..))
import GHC.Eventlog.Live.Machines (heapProfBreakdownEitherReader)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Options.Applicative qualified as O

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
