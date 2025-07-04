{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Machine.Plan (await, yield)
import Data.Machine.Process (ProcessT, (~>))
import Data.Machine.Runner (runT_)
import Data.Machine.Type (MachineT, repeatedly)
import Data.Time (NominalDiffTime)
import Data.Void (Void)
import Data.Word (Word64)
import qualified Database.InfluxDB.Line as InfluxDB (Line (..))
import GHC.Eventlog.Machines (decodeEventsMaybe, sourceHandleWait)
import GHC.RTS.Events (Event (..), Timestamp)
import qualified Network.Socket as S
import qualified Options.Applicative as O
import System.IO (Handle)
import qualified System.IO as IO

main :: IO ()
main = do
    Options{..} <- O.execParser optionsInfo
    eventlogSource <- mkSourceEvents eventlogSourceOptions
    runT_ (eventlogSource ~> dumper)

dumper :: ProcessT IO Event ()
dumper = repeatedly $ await >>= liftIO . print

--------------------------------------------------------------------------------
-- Writing to InfluxDB
--------------------------------------------------------------------------------

type Line = () -- InfluxDB.Line Timestamp

mkSinkInfluxDB :: ProcessT IO Event Line
mkSinkInfluxDB = repeatedly go
    where
        go = do
            Event{..} <- await
            case evSpec of
                _ ->


--------------------------------------------------------------------------------
-- Reading from the Eventlog Socket
--------------------------------------------------------------------------------

mkSourceEvents :: EventlogSourceOptions -> IO (MachineT IO k Event)
mkSourceEvents EventlogSourceOptions{..} = do
    eventlogHandle <- connect eventlogSourceSocket
    pure $
        sourceHandleWait eventlogHandle eventlogSourceTimeoutMcs eventlogSourceChunkSizeBytes
            ~> decodeEventsMaybe
            ~> fromMaybe

fromMaybe :: ProcessT IO (Maybe a) a
fromMaybe = repeatedly go
  where
    go = await >>= maybe go yield

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

newtype Options = Options
    { eventlogSourceOptions :: EventlogSourceOptions
    }
    deriving (Show)

optionsParser :: O.Parser Options
optionsParser =
    Options
        <$> eventlogSourceOptionsParser

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
