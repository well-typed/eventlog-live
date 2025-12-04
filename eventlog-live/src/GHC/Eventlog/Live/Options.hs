{- |
Module      : GHC.Eventlog.Live.Options
Description : Command-line option parsers for eventlog machines.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Options (
  EventlogSource (..),
  eventlogSourceParser,
  eventlogSocketTimeoutSParser,
  eventlogSocketTimeoutExponentParser,
  heapProfBreakdownParser,
  eventlogLogFileParser,
  eventlogFlushIntervalSParser,
  verbosityParser,
  statsParser,
) where

import Control.Applicative (asum)
import GHC.Eventlog.Live.Data.Severity (Severity (..), fromSeverityString)
import GHC.Eventlog.Live.Machine.Analysis.Heap (heapProfBreakdownEitherReader)
import GHC.RTS.Events (HeapProfBreakdown (..))
import Options.Applicative qualified as O
import Options.Applicative.Help.Pretty qualified as OP

--------------------------------------------------------------------------------
-- Eventlog Source

{- |
The type of eventlog sockets.
-}
data EventlogSource
  = EventlogStdin
  | EventlogFile FilePath
  | EventlogSocketUnix FilePath

{- |
Parser for the eventlog socket.
-}
eventlogSourceParser :: O.Parser EventlogSource
eventlogSourceParser =
  asum
    [ stdinParser
    , fileParser
    , socketUnixParser
    ]
 where
  stdinParser =
    EventlogStdin
      <$ O.flag'
        ()
        ( O.long "eventlog-stdin"
            <> O.help "Read the eventlog from stdin."
        )
  fileParser =
    EventlogFile
      <$> O.strOption
        ( O.long "eventlog-file"
            <> O.metavar "FILE"
            <> O.help "Read the eventlog from a file."
        )
  socketUnixParser =
    EventlogSocketUnix
      <$> O.strOption
        ( O.long "eventlog-socket"
            <> O.metavar "SOCKET"
            <> O.help "Read the eventlog from a Unix socket."
        )

{- |
Parser for the intial timeout for exponential backoff.
-}
eventlogSocketTimeoutSParser :: O.Parser Double
eventlogSocketTimeoutSParser =
  O.option
    O.auto
    ( O.long "eventlog-socket-timeout"
        <> O.metavar "SECONDS"
        <> O.help "Eventlog socket connection retry timeout in seconds."
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
        <> O.metavar "NUMBER"
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
  O.option (O.eitherReader heapProfBreakdownEitherReader) . mconcat $
    [ O.short 'h'
    , O.metavar "Tcmdyrbi"
    , O.helpDoc . Just . OP.vcat . fmap OP.pretty $
        [ "Heap profile breakdown."
        , "Should match the option passed to the application."
        ]
    ]

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
Parser for the eventlog flush interval.
-}
eventlogFlushIntervalSParser :: O.Parser Double
eventlogFlushIntervalSParser =
  O.option O.auto . mconcat $
    [ O.long "eventlog-flush-interval"
    , O.metavar "SECONDS"
    , O.helpDoc . Just . OP.vcat . fmap OP.pretty $
        [ "Eventlog flush interval in seconds."
        , "Should match the option passed to the application."
        ]
    , O.value defaultEventlogFlushIntervalS
    ]

{- |
Internal helper.
The default interval in which the eventlog is flushed in seconds.
-}
defaultEventlogFlushIntervalS :: Double
defaultEventlogFlushIntervalS = 1

--------------------------------------------------------------------------------
-- Verbosity

{- |
Parser for `Severity`
The default severity is `WARN`.
-}
verbosityParser :: O.Parser Severity
verbosityParser =
  O.option
    (O.maybeReader fromSeverityString)
    ( O.short 'v'
        <> O.long "verbosity"
        <> O.metavar "fatal|error|warning|info|debug|trace"
        <> O.help "The severity threshold for logging."
        <> O.value WARN
    )

--------------------------------------------------------------------------------
-- Statistics

statsParser :: O.Parser Bool
statsParser =
  O.flag False True $
    ( O.short 's'
        <> O.long "stats"
        <> O.help "Display runtime statistics."
    )
