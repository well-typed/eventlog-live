{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import GHC.Eventlog.Live.InfluxDB qualified (main)

main = GHC.Eventlog.Live.InfluxDB.main
