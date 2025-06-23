{-# LANGUAGE LambdaCase #-}

module Main where

import Data.String
import Control.Concurrent.Async
import Control.Lens
import System.Environment
import System.Exit

import System.Metrics.InfluxDB
import System.Metrics.Eventlog

import qualified Database.InfluxDB as InfluxDB

main :: IO ()
main = do
    -- Read the environment variables we need
    env <- readEnv

    -- Initialise the communication channel between the eventlog socket and
    -- InfluxDB
    (influxWrite, kill) <- influxContinuation (eventlogInfluxEnvWriteParams env)

    -- Listen to the socket
    (a, _) <- eventlogMetrics (eventlogInfluxEnvSocketPath env) influxWrite
    _ <- wait a

    -- Kill the InfluxDB populator
    kill


data EventlogInfluxEnv = EventlogInfluxEnv
    { eventlogInfluxEnvSocketPath  :: FilePath
    , eventlogInfluxEnvWriteParams :: InfluxDB.WriteParams
    , eventlogInfluxEnvQueryParams :: InfluxDB.QueryParams
    }

readEnv :: IO EventlogInfluxEnv
readEnv = do
  socket <-
    lookupEnv "GHC_EVENTLOG_SOCKET" >>= \case
      Nothing -> dieUsage
      Just p  -> return p
  dbHost <-
    lookupEnv "GHC_EVENTLOG_INFLUXDB_HOST" >>= \case
      Nothing -> dieUsage
      Just h  -> return $ fromString h
  dbName <-
    lookupEnv "GHC_EVENTLOG_INFLUXDB_DB" >>= \case
      Nothing -> dieUsage
      Just h  -> return $ fromString h
  dbUser <-
    lookupEnv "GHC_EVENTLOG_INFLUXDB_USERNAME" >>= \case
      Nothing -> dieUsage
      Just u  -> return $ fromString u
  dbPass <-
    lookupEnv "GHC_EVENTLOG_INFLUXDB_PASSWORD" >>= \case
      Nothing -> dieUsage
      Just p  -> return $ fromString p

  return $
    EventlogInfluxEnv
      socket
      ( InfluxDB.writeParams dbName &
          set (InfluxDB.server . InfluxDB.host) dbHost &
          set InfluxDB.authentication (Just $ InfluxDB.credentials dbUser dbPass)
      )
      ( InfluxDB.queryParams dbName &
          set (InfluxDB.server . InfluxDB.host) dbHost &
          set InfluxDB.authentication (Just $ InfluxDB.credentials dbUser dbPass)
      )

dieUsage :: IO a
dieUsage = do
    putStrLn "eventlog-influxdb: missing configuration variable(s):"
    putStrLn "  GHC_EVENTLOG_SOCKET            must specify eventlog socket path"
    putStrLn "  GHC_EVENTLOG_INFLUXDB_HOST     must specify InfluxDB host"
    putStrLn "  GHC_EVENTLOG_INFLUXDB_DB       must specify InfluxDB database name"
    putStrLn "  GHC_EVENTLOG_INFLUXDB_USERNAME must specify InfluxDB database username"
    putStrLn "  GHC_EVENTLOG_INFLUXDB_PASSWORD must specify InfluxDB database password"
    exitFailure
