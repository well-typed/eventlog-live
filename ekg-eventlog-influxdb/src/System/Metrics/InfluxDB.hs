{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
module System.Metrics.InfluxDB where

import System.Metrics.Eventlog

import Control.Concurrent
import Control.Monad
import Debug.Trace
import qualified Database.InfluxDB as Influxdb
import qualified Database.InfluxDB.Format as Influxdb
import System.Mem
import Control.Lens
import Data.Time
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map as Map
import Control.Applicative
import GHC.IO.Handle (hFlush)
import System.IO (stdout)

type ILine = Influxdb.Line NominalDiffTime

formatLine :: Line -> ILine
formatLine (Line name time fields) =
  Influxdb.Line @NominalDiffTime (Influxdb.formatMeasurement Influxdb.text name) Map.empty (Map.fromList (map convertField fields)) (Just time)
  where
    convertField (Field k v) = (Influxdb.formatKey Influxdb.text k, convertFieldVal v)

    convertFieldVal (FieldInt i)  = Influxdb.FieldInt i
    convertFieldVal (FieldFloat d) = Influxdb.FieldFloat d
    convertFieldVal (FieldString t) = Influxdb.FieldString t
    convertFieldVal (FieldBool b)   = Influxdb.FieldBool b

batchSize = 1024

maxWaitTime = 1_000_000 -- 1s

workerThread :: Influxdb.WriteParams -> TChan ILine -> IO ()
workerThread p chan = forever $ do
  lines <- makeBatch chan
  case lines of
    [] -> return ()
    _ -> do
      putStrLn $ "writing batch of size " ++ show (length lines)
      hFlush stdout
      Influxdb.writeBatch p lines
      putStrLn $ "wrote batch of size " ++ show (length lines)
      hFlush stdout

makeBatch :: TChan ILine -> IO [ILine]
makeBatch t = do
    d <- registerDelay maxWaitTime
    atomically $ loop d batchSize []
  where
    loop :: TVar Bool -> Integer -> [ILine] -> STM [ILine]
    loop d 0 acc = return acc
    loop d n acc = do
      elem <- (Just <$> readTChan t) <|> (readTVar d >>= \done -> if done then return Nothing else retry)
      case elem of
        Nothing -> return acc
        Just v -> loop d (n - 1) (v : acc)

influxContinuation :: Influxdb.WriteParams -> IO (Line -> IO (), IO ())
influxContinuation p = do
    chan <- newTChanIO
    worker <- async (workerThread p chan)
    return (writeLine chan, cancel worker)
  where
    writeLine :: TChan ILine -> Line -> IO ()
    writeLine chan line = do
      let formatted = formatLine line
      atomically $ writeTChan chan formatted
