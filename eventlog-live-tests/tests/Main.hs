{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Machine ((~>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
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
    [ test_oddball_HasHeapProfSample
    , test_oddball_HasUserMarker'Summing
    , test_jumpyJump_HasCostCentreProfile
    , test_jumpyJump_HasGhcStackProfilerProfile
    ]

test_oddball_HasHeapProfSample :: (HasLogger) => EventlogSocketAddr -> ProgramTest
test_oddball_HasHeapProfSample =
  let oddball =
        (buildProgram "oddball")
          { rtsopts = ["-l-au", "-hT", "--eventlog-flush-interval=1"]
          }
      options =
        defaultOptions
          { extraArgs = ["--service-name=oddball", "-hT", "--eventlog-flush-interval=1"]
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
          { extraArgs = ["--service-name=oddball", "--eventlog-flush-interval=1"]
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
          { extraArgs = ["--service-name=jumpy-jump", "--eventlog-flush-interval=1"]
          , maybeConfigBody =
              Just
                "processors:\n\
                \  profiles:\n\
                \    cost_centre_sample:\n\
                \      name: ghc_eventlog_CostCentreProfile\n\
                \      description: A cost centre callstack sample.\n\
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
          { extraArgs = ["--service-name=jumpy-jump", "--eventlog-flush-interval=1"]
          , maybeConfigBody =
              Just
                "processors:\n\
                \  profiles:\n\
                \    stack_sample:\n\
                \      name: ghc_eventlog_StackSampleProfile\n\
                \      description: A thread RTS callstack sample.\n\
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
