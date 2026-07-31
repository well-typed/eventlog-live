{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isDigit)
import Data.Machine ((~>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Version (Version)
import Data.Version qualified as V (makeVersion)
import GHC.Eventlog.Live.Test
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import System.Process (readProcess)
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
import Text.Read (readMaybe)

main :: IO ()
main = do
  -- Allow the user to overwrite the TCP port:
  tcpPort <- fromMaybe "4242" . (readMaybe =<<) <$> lookupEnv "GHC_EVENTLOG_INET_PORT"

  -- Check if GHC version supports @ghc-stack-profiler@.
  ghc <- fromMaybe "ghc" <$> lookupEnv "GHC"
  ghcVersion <- parseVersion <$> readProcess ghc ["--numeric-version"] ""

  -- Create list of tasty ingredients:
  let ingredients = [includingOptions [keepProgramBuildOption]] <> defaultIngredients

  -- Create list of tests:
  let tests :: (HasLogger) => [EventlogSocketAddr -> ProgramTest]
      tests =
        concat
          [ [test_oddball_HasHeapProfSample]
          , [test_oddball_HasUserMarker'Summing]
          , [test_jumpyJump_HasCostCentreProfile]
          , [test_jumpyJump_HasGhcStackProfilerProfile | ghcVersion >= V.makeVersion [9, 10]]
          ]

  -- Create logger:
  withLogger $ do
    -- Create temporary directory:
    withTempDirectory "/tmp" "eventlog-socket" $ \tmpDir -> do
      -- Base socket addresses
      let unixTests = tests <*> pure (EventlogSocketUnixAddr $ tmpDir </> "ghc_eventlog.sock")
      let inetTests = tests <*> pure (EventlogSocketInetAddr "127.0.0.1" tcpPort)
      defaultMainWithIngredients ingredients . testGroup "Tests" . runProgramTests $ unixTests <> inetTests

test_oddball_HasHeapProfSample :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_oddball_HasHeapProfSample =
  let oddball =
        (buildProgram "oddball")
          { rtsopts = ["-l-au", "-hT", "--eventlog-flush-interval=1"]
          }
      options =
        defaultOptions
          { extraArgs = ["-hT", "--eventlog-flush-interval=1"]
          , extraEnv = [("OTEL_SERVICE_NAME", "oddball")]
          , maybeConfigBody =
              Just
                "processors:\n\
                \  metrics:\n\
                \    heap_prof_sample:\n\
                \      name: ghc_eventlog_HeapProfSample\n\
                \      description: A heap profile sample.\n\
                \      aggregate: 1s\n\
                \      export: 1s\n\
                \"
          }
   in programTestFor "test_oddball_HasHeapProfSample" oddball options $ do
        assertResourceTelemetryData $
          toResourceMetrics
            ~> withServiceName "oddball"
            ~> toScopeMetrics
            ~> toMetrics
            ~> withMetric'name (== "ghc_eventlog_HeapProfSample")
            ~> hasInput

test_oddball_HasUserMarker'Summing :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_oddball_HasUserMarker'Summing =
  let oddball =
        (buildProgram "oddball")
          { rtsopts = ["-l-au", "--eventlog-flush-interval=1"]
          }
      options =
        defaultOptions
          { extraArgs = ["--eventlog-flush-interval=1"]
          , extraEnv = [("OTEL_SERVICE_NAME", "oddball")]
          , maybeConfigBody =
              Just
                "processors:\n\
                \  logs:\n\
                \    user_marker:\n\
                \      name: ghc_eventlog_UserMarker\n\
                \      description: A user marker.\n\
                \      export: 1s\n\
                \"
          }
   in programTestFor "test_oddball_HasUserMarker'Summing" oddball options $ do
        assertResourceTelemetryData $
          toResourceLogs
            ~> withServiceName "oddball"
            ~> toScopeLogs
            ~> toLogRecords
            ~> withLogRecord'body ("Summing " `T.isPrefixOf`)
            ~> hasInput

test_jumpyJump_HasCostCentreProfile :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_jumpyJump_HasCostCentreProfile =
  let jumpyJump =
        (buildProgramWith "jumpy-jump" ["-f-use-ghc-stack-profiler", "--enable-profiling"])
          { rtsopts = ["-l", "-p", "--eventlog-flush-interval=1"]
          }
      options =
        defaultOptions
          { extraArgs = ["--eventlog-flush-interval=1"]
          , extraEnv = [("OTEL_SERVICE_NAME", "jumpy-jump")]
          , maybeConfigBody =
              Just
                "processors:\n\
                \  profiles:\n\
                \    cost_centre_stack_profile:\n\
                \      name: ghc_eventlog_CostCentreStackProfile\n\
                \      description: A GHC cost-centre stack profile.\n\
                \      export: 1s\n\
                \"
          }
   in programTestFor "test_jumpyJump_HasCostCentreProfile" jumpyJump options $ do
        assertResourceTelemetryData $
          toResourceProfiles
            ~> toScopeProfiles
            ~> toProfiles
            ~> logging
            ~> hasInput

test_jumpyJump_HasGhcStackProfilerProfile :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_jumpyJump_HasGhcStackProfilerProfile =
  let jumpyJump =
        (buildProgramWith "jumpy-jump" ["-f+use-ghc-stack-profiler"])
          { rtsopts = ["-l", "--eventlog-flush-interval=1"]
          }
      options =
        defaultOptions
          { extraArgs = ["--eventlog-flush-interval=1"]
          , extraEnv = [("OTEL_SERVICE_NAME", "jumpy-jump")]
          , maybeConfigBody =
              Just
                "processors:\n\
                \  profiles:\n\
                \    call_stack_profile:\n\
                \      name: ghc_eventlog_CallStackProfile\n\
                \      description: A GHC call-stack profile.\n\
                \      export: 1s\n\
                \"
          }
   in programTestFor "test_jumpyJump_HasGhcStackProfilerProfile" jumpyJump options $ do
        assertResourceTelemetryData $
          toResourceProfiles
            ~> toScopeProfiles
            ~> toProfiles
            ~> logging
            ~> hasInput

{- |
Internal helper.

Parse the output of @ghc --numeric-version@.
-}
parseVersion :: String -> Version
parseVersion =
  V.makeVersion
    . fmap (read . T.unpack . T.takeWhile isDigit)
    . T.splitOn "."
    . T.pack
