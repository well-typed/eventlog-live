-----------------------------------------------------------------------------
-- -*- haskell -*-
-- Ever popular nfib, now in parallel.
-----------------------------------------------------------------------------

module Main (main) where

import Control.Parallel
import Data.Foldable (for_, traverse_)
import Data.Maybe
import GHC.Eventlog.Socket
import System.Environment

main :: IO ()
main = do
  traverse_ startWait =<< lookupEnv "GHC_EVENTLOG_SOCKET"
  (t : ns) <- getArgs
  for_ (map read ns) $ \n -> do
    let res = parfib n (read t)
    putStrLn ("parfib " ++ show n ++ " = " ++ show res)

-- parallel version of the code with thresholding
parfib :: Int -> Int -> Int
parfib n t
  | n <= t = nfib n
  | otherwise = n1 `par` (n2 `pseq` n1 + n2 + 1)
 where
  n1 = parfib (n - 1) t
  n2 = parfib (n - 2) t

-- sequential version of the code
nfib :: Int -> Int
nfib 0 = 1
nfib 1 = 1
nfib x = nfib (x - 2) + nfib (x - 1) + 1
