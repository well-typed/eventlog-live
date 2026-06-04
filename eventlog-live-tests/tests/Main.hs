{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import GHC.Eventlog.Live.Test
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
import Text.Read (readMaybe)

main :: IO ()
main = do
  -- Allow the user to overwrite the TCP port:
  tcpPort <- fromMaybe "4242" . (readMaybe =<<) <$> lookupEnv "GHC_EVENTLOG_INET_PORT"

  -- Create list of tasty ingredients:
  let ingredients = [includingOptions [keepProgramBuildOption]] <> defaultIngredients

  -- Create logger:
  withLogger $ do
    -- Create temporary directory:
    withTempDirectory "/tmp" "eventlog-socket" $ \tmpDir -> do
      -- Base socket addresses
      let unixTests = tests <*> pure (EventlogSocketUnixAddr $ tmpDir </> "ghc_eventlog.sock")
      let inetTests = tests <*> pure (EventlogSocketInetAddr "127.0.0.1" tcpPort)
      defaultMainWithIngredients ingredients . testGroup "Tests" . runProgramTests $ unixTests <> inetTests
 where
  tests :: (HasLogger) => [EventlogSocketAddr -> ProgramTest]
  tests =
    [ test_oddball
    ]

test_oddball :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_oddball =
  let oddball =
        (buildProgram "oddball")
          { rtsopts = ["-l-au", "-hT", "-A256K", "-i0", "--eventlog-flush-interval=1"]
          }
      eventlogLiveOtelcolArgs = ["-hT", "--eventlog-flush-interval=1"]
   in programTestFor "test_oddball" oddball eventlogLiveOtelcolArgs $ do
        assertResourceTelemetryData $
          hasInput
