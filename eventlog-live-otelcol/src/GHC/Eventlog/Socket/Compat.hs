{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Socket.Compat
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Socket.Compat (
  MyEventlogSocket (..),
  maybeMyEventlogSocketParser,
  startMyEventlogSocket,
) where

import Control.Applicative (asum)
import GHC.Eventlog.Live.Logger (Logger)
import Options.Applicative qualified as O
import Options.Applicative.Extra.Feature (Feature (..))
import Options.Applicative.Extra.Feature qualified as OF

#ifdef EVENTLOG_LIVE_OTELCOL_USE_EVENTLOG_SOCKET
import Data.Foldable (for_)
import Data.Text qualified as T
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (writeLog)
import GHC.Eventlog.Socket qualified as Eventlog.Socket
#else
import Control.Monad (when)
import Data.Maybe (isJust)
#endif

--------------------------------------------------------------------------------
-- Feature: use-eventlog-socket
--------------------------------------------------------------------------------

useEventlogSocket :: Feature
useEventlogSocket = Feature{flag = "use-eventlog-socket", isOn = isOn, info = "Cannot open eventlog socket."}
 where
  isOn :: Bool
#ifdef EVENTLOG_LIVE_OTELCOL_USE_EVENTLOG_SOCKET
  isOn = True
#else
  isOn = False
#endif

--------------------------------------------------------------------------------
-- My Eventlog Socket
--------------------------------------------------------------------------------

newtype MyEventlogSocket
  = MyEventlogSocketUnix FilePath

maybeMyEventlogSocketParser :: O.Parser (Maybe MyEventlogSocket)
maybeMyEventlogSocketParser =
  asum $
    [ myEventlogSocketUnixParser
    , pure Nothing
    ]

myEventlogSocketUnixParser :: O.Parser (Maybe MyEventlogSocket)
myEventlogSocketUnixParser =
  OF.onlyFor useEventlogSocket (O.option (Just . MyEventlogSocketUnix <$> O.str)) (O.metavar "FILE") $
    O.long "my-eventlog-socket-unix"
      <> OF.helpFor useEventlogSocket "Open an eventlog socket for this program on the given Unix socket."

{- |
Set @eventlog-socket@ as the eventlog writer.
-}
startMyEventlogSocket :: Logger IO -> Maybe MyEventlogSocket -> IO ()
#ifdef EVENTLOG_LIVE_OTELCOL_USE_EVENTLOG_SOCKET
startMyEventlogSocket logger maybeMyEventlogSocket =
  for_ maybeMyEventlogSocket $ \case
    MyEventlogSocketUnix myEventlogSocketUnix -> do
      writeLog logger INFO $
        "Start eventlog-socket with Unix domain socket at " <> T.pack myEventlogSocketUnix <> "."
      Eventlog.Socket.startWait myEventlogSocketUnix
#else
startMyEventlogSocket logger maybeMyEventlogSocket =
  when (isJust maybeMyEventlogSocket) $
    OF.exitIfUnsupported useEventlogSocket logger
#endif
