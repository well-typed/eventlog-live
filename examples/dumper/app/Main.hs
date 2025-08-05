{-# LANGUAGE LambdaCase #-}
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
import GHC.Eventlog.Live
import GHC.Eventlog.Live.Machines (decodeEventsTick, dropTick, sourceHandleWait)
import GHC.Eventlog.Live.Options
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
  Options{..} <- O.execParser options
  runWithEventlogSocket
    batchInterval
    Nothing
    eventlogSocket
    Nothing
    $ printSink (RE.makeRegex <$> eventlogPattern)

-- | Filter entries and print them to standard output.
printSink :: (Show a) => Maybe RE.Regex -> ProcessT IO a Void
printSink maybePattern = repeatedly go
 where
  go =
    await >>= \ev -> do
      let evStr = show ev
      when (maybe True (`RE.matchTest` evStr) maybePattern) . liftIO $
        putStrLn evStr

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

options :: O.ParserInfo Options
options = O.info (optionsParser O.<**> O.helper) O.idm

data Options = Options
  { eventlogSocket :: EventlogSocket
  , batchInterval :: Int
  , eventlogPattern :: Maybe ByteString
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSocketParser
    <*> batchIntervalParser
    <*> O.optional
      ( O.strOption
          ( O.short 'P'
              <> O.long "pattern"
              <> O.help "Regular expression to filter events"
              <> O.metavar "PATTERN"
          )
      )
