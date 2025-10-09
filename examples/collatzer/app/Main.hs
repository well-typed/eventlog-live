{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (optional, (<**>), (<|>))
import Control.Concurrent.STM (STM, atomically, readTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Word (Word64)
import System.Console.Concurrent (outputConcurrent)
import System.Console.Regions (
  RegionLayout (Linear),
  displayConsoleRegions,
  finishConsoleRegion,
  setConsoleRegion,
  withConsoleRegion,
 )
import System.IO (Handle)
import Text.Printf (PrintfArg, PrintfType, printf)

import qualified Data.ByteString as BS
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.Socket as Sock
import qualified Options.Applicative as O
import qualified System.IO as IO
import qualified System.Metrics as EKG
import qualified System.Remote.Monitoring

import GHC.Eventlog.Counters (Counters (..), ThreadState (..), count, newCounters)
import GHC.Eventlog.Counters.EKG (registerCounters)
import GHC.Eventlog.Live.Machine.Core (Tick (..), dropTick, sortByBatchTick)
import GHC.Eventlog.Live.Machine.Decoder (decodeEventBatch)
import GHC.Eventlog.Live.Machine.Sink (fileSinkBatch)
import GHC.Eventlog.Live.Machine.Source (defaultChunkSizeBytes, sourceHandleBatch)
import GHC.RTS.Events (Event (evTime))

import Data.Machine (MachineT, ProcessT, await, repeatedly, runT_, (~>))
import Data.Machine.Fanout (fanout)

main :: IO ()
main = displayConsoleRegions $ do
  opts <-
    O.execParser $
      O.info (optsP <**> O.helper) $
        O.fullDesc
          <> O.header "collatzer - demo web app"

  hdl <- case optPathType opts of
    PathTypeSocket -> connectToEventlogSocket (optEventLogPath opts)
    PathTypeFile -> openEventlogFile (optEventLogPath opts)

  let input :: MachineT IO k (Tick BS.ByteString)
      input = sourceHandleBatch 1_000 defaultChunkSizeBytes hdl

  let events :: ProcessT IO (Tick BS.ByteString) Event
      events =
        decodeEventBatch
          ~> sortByBatchTick evTime
          ~> dropTick

  counters <- atomically newCounters

  for_ (optEkgPort opts) $ \port -> do
    ekg <- EKG.newStore
    registerCounters counters ekg
    System.Remote.Monitoring.forkServerWith ekg "localhost" port

  let count' :: ProcessT IO Event a
      count' = repeatedly $ do
        ev <- await
        liftIO $ atomically $ count counters ev

  withDisplay counters $ do
    putStrLn "Starting..."
    case optToFile opts of
      Nothing -> runT_ $ input ~> events ~> count'
      Just fp -> IO.withFile fp IO.WriteMode $ \whdl ->
        runT_ $ input ~> fanout [events ~> count', fileSinkBatch whdl]

-------------------------------------------------------------------------------
-- connecting & opening
-------------------------------------------------------------------------------

connectToEventlogSocket :: FilePath -> IO Handle
connectToEventlogSocket socketName = do
  s <- Sock.socket Sock.AF_UNIX Sock.Stream Sock.defaultProtocol
  Sock.connect s (Sock.SockAddrUnix socketName)
  h <- Sock.socketToHandle s IO.ReadMode
  IO.hSetBuffering h IO.NoBuffering
  return h

openEventlogFile :: FilePath -> IO Handle
openEventlogFile path = do
  hdl <- IO.openFile path IO.ReadMode
  IO.hSetBinaryMode hdl True
  return hdl

-------------------------------------------------------------------------------
-- output-concurrent + printf
-------------------------------------------------------------------------------

class PrintfConcurrent t where
  printfConcurrent' :: (forall r. (PrintfType r) => r) -> t

instance (a ~ ()) => PrintfConcurrent (IO a) where
  printfConcurrent' s = outputConcurrent (s :: String)

instance (PrintfArg a, PrintfConcurrent r) => PrintfConcurrent (a -> r) where
  printfConcurrent' f a = printfConcurrent (f a)

printfConcurrent :: (PrintfConcurrent t) => String -> t
printfConcurrent s = printfConcurrent' (printf s)

-------------------------------------------------------------------------------
-- HUD
-------------------------------------------------------------------------------

withDisplay :: Counters -> IO a -> IO a
withDisplay Counters{..} action0 =
  with textTime $
    with textCaps $
      with textGc $
        with textThread $
          with
            textHeap
            action0
 where
  textTime = do
    t <- readTVar cntTime
    n <- readTVar cntEvents
    return $ printf "Time: %s; Events: %s" (show t) (show n)

  textCaps = do
    c <- readTVar cntCaps
    return $ printf "Seen capabilities: %s" (show (IS.toList c))

  textGc = do
    maxGcTime <- readTVar cntMaxGCTime
    gcCount0 <- readTVar cntGcCount0
    gcCountS <- readTVar cntGcCountS
    return $ printf "Max GC time: %.06f sec; GCs seen: %d + %d" (fromIntegral maxGcTime / 1e9 :: Double) gcCount0 gcCountS

  textThread = do
    threads <- readTVar cntThreads

    let countThreads :: Int -> Int -> Int -> Int -> [ThreadState] -> (Int, Int, Int, Int)
        countThreads !n m p r [] = (n, m, p, r)
        countThreads !n m p r (ThreadRunning : xs) = countThreads (n + 1) m p r xs
        countThreads !n m p r (ThreadQueued : xs) = countThreads n (m + 1) p r xs
        countThreads !n m p r (ThreadStopped : xs) = countThreads n m (p + 1) r xs
        countThreads !n m p r (ThreadUnknown : xs) = countThreads n m p (r + 1) xs

    let (running, queued, stopped, unknown) = countThreads 0 0 0 0 $ Map.elems threads

    return $ printf "Running threads: %d; queued: %d; stopped: %d; unknown state: %d" running queued stopped unknown
  textHeap = do
    s <- readTVar cntHeapSize
    l <- readTVar cntHeapLive
    a <- readTVar cntHeapAlloc

    return $ printf "Heap: size %s; live %s; alloc %s" (humaniseW64 s) (humaniseW64 l) (humaniseW64 a)

  with :: STM String -> IO a -> IO a
  with regionText action = do
    withConsoleRegion Linear $ \reg -> do
      setConsoleRegion reg (fmap T.pack regionText)
      res <- action
      text <- atomically regionText
      finishConsoleRegion reg text
      return res

  humaniseW64 :: Word64 -> String
  humaniseW64 = humanise . fromIntegral

  humanise :: Double -> String
  humanise = go 0
   where
    go :: Int -> Double -> String
    go k d
      | d < 10 = printf "%.03f %s" d (unit k)
      | d < 100 = printf "%.02f %s" d (unit k)
      | d < 1000 = printf "%.01f %s" d (unit k)
      | otherwise = go (k + 1) (d / 1000)

    unit :: Int -> String
    unit 0 = ""
    unit 1 = "k"
    unit 2 = "M"
    unit 3 = "G"
    unit 4 = "T"
    unit 5 = "E"
    unit _ = "alot"

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data PathType = PathTypeSocket | PathTypeFile
  deriving (Show)

data Opts = Opts
  { optEventLogPath :: FilePath
  , optPathType :: PathType
  , optEkgPort :: Maybe Int
  , optToFile :: Maybe FilePath
  }
  deriving (Show)

optsP :: O.Parser Opts
optsP = do
  optEventLogPath <-
    O.strArgument $
      O.metavar "PATH"
        <> O.help "Eventlog path"
  optPathType <- pathTypeP
  optEkgPort <-
    optional $
      O.option O.auto $
        O.long "ekg"
          <> O.metavar "PORT"
          <> O.help "EKG server port"
  optToFile <-
    optional $
      O.strOption $
        O.long "write-to-file"
          <> O.metavar "PATH"
          <> O.help "Additionally write received eventlog to file"

  pure Opts{..}
 where
  pathTypeP :: O.Parser PathType
  pathTypeP =
    O.flag' PathTypeSocket (O.long "unix" <> O.help "Path is to UNIX socket (default)")
      <|> O.flag' PathTypeFile (O.long "file" <> O.help "Path is to ordinary file")
      <|> pure PathTypeSocket
