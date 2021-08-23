{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where


import System.Metrics.Eventlog
import Control.Concurrent
import System.Mem
import Control.Monad
import Debug.Trace
import System.Metrics
import qualified Database.InfluxDB as Influxdb
import System.Remote.Monitoring.Influxdb
import System.Metrics.InfluxDB
import Control.Monad
import System.Mem
import Control.Concurrent.Async
import Control.Lens

main :: IO ()
main = do
  let db = "mydb"
  let qp = (Influxdb.queryParams db)
            & set (Influxdb.server. Influxdb.host) "10.233.1.2"
  Influxdb.manage qp $ Influxdb.formatQuery ("DROP DATABASE mydb")
  Influxdb.manage qp $ Influxdb.formatQuery ("CREATE DATABASE mydb")

  let p = (Influxdb.writeParams db)
            & set (Influxdb.server. Influxdb.host)"10.233.1.2"
  (influxWrite, kill) <- influxContinuation p
  (a1, store) <- eventlogMetrics "/tmp/ghc-eventlog-socket" influxWrite
  --forkInfluxdb (defaultInfluxdbOptions p) store
  wait a1
  kill
















