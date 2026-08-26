module Main where

import Control.Concurrent (threadDelay)
import Control.DeepSeq (force)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceEventIO)
import GHC.Eventlog.Socket (startFromEnv)
import System.Environment (getArgs)
import System.Mem (performGC)
import Text.Read (readMaybe)

main :: IO ()
main = do
  startFromEnv
  traverse_ run . parseArgs =<< getArgs

-- | Allocate fragmented memory.
hogger :: Int -> IO ()
hogger n = do
  -- Allocate lots of ByteStrings (ByteStrings are backed with pinned data)
  let !superset = force $ take n [BS.singleton x | x <- cycle [minBound .. maxBound]]
  traceEventIO ("hogger " <> show n <> ": 1st Plateau start")
  spin 3

  -- Extract only a small subset of the superset and allow superset to be
  -- garbage collected. Specifically retain every 10th element.
  let subsetFactor = 10 :: Int
  let !subset = force $ [x | (x, 1) <- zip superset (cycle [1 .. subsetFactor])]
  traceEventIO ("hogger " <> show n <> ": 2nd Plateau start")
  spin (3 * subsetFactor)

  -- Stop `subset` from being garbage collected by using it here.
  traceEventIO ("hogger " <> show n <> ": " <> show (length subset))

-- | Spin and allow heap profiler to collect samples.
spin :: Int -> IO ()
spin i = forM_ [1 .. i] (\_ -> threadDelay 1 >> performGC)

data Command
  = Hogger !Int
  | HoggerRepeat !Int

-- | Execute a command.
run :: Command -> IO ()
run (Hogger n) = hogger n
run c@(HoggerRepeat n) = hogger n >> run c

-- | Parse the arguments into commands.
parseArgs :: [String] -> [Command]
parseArgs [] = []
parseArgs ("--repeat" : n : rest)
  | null rest = [HoggerRepeat (parseInt n)]
  | otherwise = error "hogger: found regular run after infinite run"
parseArgs (n : rest) = Hogger (parseInt n) : parseArgs rest

parseInt :: String -> Int
parseInt n = fromMaybe (error $ "Could not parse number " <> n) (readMaybe n)
