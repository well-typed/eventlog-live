module Main where

import Control.Concurrent
import Control.Monad
import Data.Foldable (traverse_, for_)
import Data.Maybe
import Debug.Trace
import GHC.Eventlog.Socket
import System.Environment
import System.Random

main :: IO ()
main = do
  startFromEnv
  n <- getNumCapabilities
  for_ [0..n-1] $ \_i -> do
    forkIO . forever $
      threadDelay 3000000 >> doRandom
  forever $
    threadDelay 3000000 >> doRandom

-- | Generate a random length stream of random numbers and sum them (poorly)
doRandom :: IO ()
doRandom = do
  g <- newStdGen
  n <- randomRIO (1000, 10000000)
  traceMarkerIO $ "Summing " ++ show n ++ " numbers"
  putStrLn $ "Generating " ++ show n ++ " random numbers"
  let stream = randomRs @Integer (-1000, 1000) g
      result = foldr (+) 00 $ take n stream
  putStrLn $ "Sum: " ++ show result
