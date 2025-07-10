{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Machine.Plan (await, yield)
import Data.Machine.Process (ProcessT, (~>))
import Data.Machine.Runner (runT_)
import Data.Machine.Type (MachineT, repeatedly)
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Eventlog.Machines (decodeEventsTick, dropTick, sourceHandleWait)
import GHC.RTS.Events (Event)
import qualified Network.Socket as S
import qualified Options.Applicative as O
import System.IO (Handle)
import qualified System.IO as IO
import qualified Text.Regex.TDFA as RE

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  Options{..} <- O.execParser optionsInfo
  let maybePattern = RE.makeRegex <$> eventlogPattern
  let EventlogSourceOptions{..} = eventlogSourceOptions
  eventlogHandle <- connect eventlogSourceSocket
  let eventlogSource = sourceHandleWait eventlogSourceTimeoutMcs eventlogSourceChunkSizeBytes eventlogHandle
  runT_ (eventlogSource ~> decodeEventsTick ~> dropTick ~> printSink maybePattern)

-- | Filter entries and print them to standard output.
printSink :: (Show a) => Maybe RE.Regex -> ProcessT IO a Void
printSink maybePattern = repeatedly go
  where
    go =
      show <$> await >>= \str ->
        when (maybe True (`RE.matchTest` str) maybePattern) $
          liftIO . putStrLn $ str

-- | Connect to eventlog socket.
connect :: EventlogSocket -> IO Handle
connect = \case
  EventlogSocketUnix socketName -> do
    socket <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
    S.connect socket (S.SockAddrUnix socketName)
    handle <- S.socketToHandle socket IO.ReadMode
    IO.hSetBuffering handle IO.NoBuffering
    pure handle

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

optionsInfo :: O.ParserInfo Options
optionsInfo = O.info (optionsParser O.<**> O.helper) O.idm

data Options = Options
  { eventlogSourceOptions :: EventlogSourceOptions
  , eventlogPattern :: Maybe ByteString
  }
  deriving (Show)

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSourceOptionsParser
    <*> O.optional
    ( O.strOption
      ( O.short 'P'
          <> O.long "pattern"
          <> O.help "Regular expression to filter events"
          <> O.metavar "PATTERN"
      )
    )

data EventlogSourceOptions = EventlogSourceOptions
  { eventlogSourceSocket :: EventlogSocket
  -- ^ Eventlog source socket.
  , eventlogSourceTimeoutMcs :: Int
  -- ^ Eventlog source timeout in microseconds.
  , eventlogSourceChunkSizeBytes :: Int
  -- ^ Eventlog source chunk size in bytes.
  , eventlogSourceIntervalMcs :: Word64
  -- ^ Eventlog source flush interval.
  }
  deriving (Show)

eventlogSourceOptionsParser :: O.Parser EventlogSourceOptions
eventlogSourceOptionsParser =
  EventlogSourceOptions
    <$> eventlogSocketParser
    <*> O.option
      O.auto
      ( O.long "timeout"
          <> O.help "Eventlog source timeout in microseconds"
          <> O.value 1_000_000
      )
    <*> O.option
      O.auto
      ( O.long "chuck-size"
          <> O.help "Eventlog source chuck size in bytes"
          <> O.value 4096
      )
    <*> O.option
      O.auto
      ( O.long "interval"
          <> O.help "Eventlog source interval in microseconds"
          <> O.value 4096
      )

newtype EventlogSocket
  = EventlogSocketUnix FilePath
  deriving (Show)

eventlogSocketParser :: O.Parser EventlogSocket
eventlogSocketParser = socketUnixParser
 where
  socketUnixParser =
    EventlogSocketUnix
      <$> O.strOption
        ( O.short 'U'
            <> O.long "unix"
            <> O.metavar "SOCKET"
            <> O.help "Eventlog source Unix socket."
        )
