module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Machine.Plan (await, yield)
import Data.Machine.Process (ProcessT, (~>))
import Data.Machine.Runner (runT_)
import Data.Machine.Type (MachineT, repeatedly)
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Eventlog.Live.Machine.Core (sortByTick)
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Source
import GHC.Eventlog.Live.Source.Core (EventlogSourceOptions (..))
import GHC.RTS.Events (Event)
import qualified GHC.RTS.Events as E
import qualified Network.Socket as S
import qualified Options.Applicative as O
import System.IO (Handle)
import qualified System.IO as IO
import qualified Text.Regex.TDFA as RE

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  Options{..} <- O.execParser options

  -- Convert the eventlog flush interval to milliseconds
  let batchIntervalMs = round (eventlogFlushIntervalS * 1_000)

  runWithEventlogSourceOptions mempty eventlogSourceOptions eventlogSocketTimeout eventlogSocketTimeoutExponent batchIntervalMs Nothing Nothing $
    sortByTick E.evTime
      ~> printSink (RE.makeRegex <$> eventlogPattern)

-- | Filter entries and print them to standard output.
printSink :: (Show a) => Maybe RE.Regex -> ProcessT IO a Void
printSink maybePattern = repeatedly go
 where
  go =
    await >>= \ev -> do
      let evStr = show ev
      when (maybe True (`RE.matchTest` evStr) maybePattern) . liftIO $
        putStrLn evStr

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

options :: O.ParserInfo Options
options = O.info (optionsParser O.<**> O.helper) O.idm

data Options = Options
  { eventlogSourceOptions :: EventlogSourceOptions
  , eventlogSocketTimeout :: Double
  , eventlogSocketTimeoutExponent :: Double
  , eventlogFlushIntervalS :: Double
  , eventlogPattern :: Maybe ByteString
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSourceOptionsParser
    <*> eventlogSocketTimeoutSParser
    <*> eventlogSocketTimeoutExponentParser
    <*> eventlogFlushIntervalSParser
    <*> O.optional
      ( O.strOption
          ( O.short 'P'
              <> O.long "pattern"
              <> O.help "Regular expression to filter events"
              <> O.metavar "PATTERN"
          )
      )
