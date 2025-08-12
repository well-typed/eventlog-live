module Main where

import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import GHC.Eventlog.Socket
import System.Environment

main :: IO ()
main = do
  eventlogSocket <-
    fromMaybe "/tmp/ghc_eventlog.sock"
      <$> lookupEnv "GHC_EVENTLOG_SOCKET"
  startWait eventlogSocket
  args <- getArgs
  for_ args $ \arg ->
    print (fib (read arg))

-- Taken from the Haskell Wiki:
-- https://wiki.haskell.org/The_Fibonacci_sequence#Naive_definition
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
