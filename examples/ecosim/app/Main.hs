-- EcoSim.hs
-- Tiny colony simulator with computation tied to simulation state
-- Run indefinitely and produces varied call stacks naturally
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Foldable
import Data.List qualified as List
import GHC.Eventlog.Socket
import GHC.Stack.Annotation
import GHC.Stack.Profiler.Sampler
import System.Environment
import System.Random

-- Simulation state --------------------------------------------------------

data Colony = Colony
  { population :: TVar Int
  , energy :: TVar Int
  }

-- Parameters
tickMicros :: Int
tickMicros = 200 * 1000 -- 200 ms per tick

maxPopulationGrowth :: Int
maxPopulationGrowth = 5

maxEnergyChange :: Int
maxEnergyChange = 10

-- Initialize colony
initColony :: IO Colony
initColony = do
  pop <- newTVarIO 500000
  en <- newTVarIO 1000000
  return $ Colony pop en

-- Simulation tick ---------------------------------------------------------

tick :: Colony -> IO ()
tick colony = annotateStackStringIO "Tick" $ do
  -- 1. Read current state
  pop <- readTVarIO (population colony)
  en <- readTVarIO (energy colony)

  -- 2. Light demographics change
  deltaPop <- randomRIO (-maxPopulationGrowth, maxPopulationGrowth)
  deltaEn <- randomRIO (-maxEnergyChange, maxEnergyChange)

  annotateStackStringIO "Next Colony step" $ atomically $ do
    modifyTVar' (population colony) (\x -> max 0 (x + deltaPop))
    modifyTVar' (energy colony) (\x -> max 0 (x + deltaEn))

  -- 3. Heavy-ish computations tied to state

  -- Prime counting -> stability modifier
  let stabilityScore = countPrimes pop
      popModifier = if even stabilityScore then 1 else -1

  -- Noise -> environment modifier
  let envForecast = noise (pop + en)
      energyModifier = if envForecast > 0 then 2 else -2

  -- Matrix multiply -> resource balancing
  let size = (en `mod` 15) + 5
      matrixScore = sum (concat (matMul (mkMatrix size) (mkMatrix size)))
      energyAdj = ceiling matrixScore `mod` 5

  -- Apply feedback
  annotateStackStringIO "Apply Colony Feedback" $ atomically $ do
    modifyTVar' (population colony) (\x -> max 0 (x + popModifier))
    modifyTVar' (energy colony) (\x -> max 0 (x + energyModifier + energyAdj))

-- Utilities ---------------------------------------------------------------

countPrimes :: Int -> Int
countPrimes n = length [x | x <- [2 .. n], isPrime x]
 where
  isPrime k = all (\d -> k `mod` d /= 0) [2 .. floor (sqrt (fromIntegral @Int @Double k))]

noise :: Int -> Double
noise n = go n 0
 where
  go 0 acc = acc
  go k acc = go (k - 1) (sin acc + cos (fromIntegral k) * 0.1)

mkMatrix :: Int -> [[Double]]
mkMatrix n = replicate n (replicate n 1.0)

matMul :: [[Double]] -> [[Double]] -> [[Double]]
matMul a b =
  let bt = List.transpose b
   in [[sum (zipWith (*) row col) | col <- bt] | row <- a]

-- Logger ------------------------------------------------------------------

logger :: Colony -> IO ()
logger colony = annotateStackStringIO "Logger Thread" $ forever $ do
  annotateStackStringIO "Logger Sleep" $ threadDelay (5 * 1000 * 1000) -- every 5 seconds
  pop <- readTVarIO (population colony)
  en <- readTVarIO (energy colony)
  putStrLn $ "Population: " ++ show pop ++ ", Energy: " ++ show en

-- Main --------------------------------------------------------------------

main :: IO ()
main = withStackProfiler (SampleIntervalMs 30) $ do
  traverse_ startWait =<< lookupEnv "GHC_EVENTLOG_SOCKET"
  putStrLn "Starting ColonySim... (Ctrl-C to stop)"
  colony <- initColony

  -- Logger thread
  _ <- forkIO $ logger colony

  -- Simulation loop
  let
    loop :: Integer -> IO ()
    loop tickNum = annotateStackStringIO "CallLoop" $ do
      tick colony
      annotateStackStringIO "Colony Sleep" $ threadDelay tickMicros
      loop (tickNum + 1)
  loop 0
