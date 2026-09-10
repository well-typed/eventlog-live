module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (encode)
import Data.Aeson.Types (FromJSON (..), ToJSON (..), defaultOptions, genericToEncoding)
import Data.Int (Int64)
import GHC.Generics (Generic)
import Network.WebSockets qualified as WS
import System.Clock (TimeSpec (..), getTime)
import System.Clock.Seconds (Clock (..))
import System.IO (hPutStrLn, stderr)
import System.Random (randomRIO)

data Measure = Measure
  { timestamp :: !Int64
  , value :: !Int64
  }
  deriving (Show, Generic)

instance ToJSON Measure where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Measure

main :: IO ()
main = do
  info "Start eventlog-live-viewer-test-server"
  WS.runServer "127.0.0.1" 30180 application

application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  info "Accepted connection"
  WS.withPingThread conn 30 (return ()) $ do
    forever $ do
      TimeSpec{..} <- getTime Realtime
      value <- randomRIO (-100, 100)
      let measure = Measure{timestamp = sec, ..}
      putStrLn $ "Send: " <> show measure
      WS.sendTextData conn (encode measure)
      threadDelay 1_000_000

info :: String -> IO ()
info = hPutStrLn stderr
