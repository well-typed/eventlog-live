{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Debug.Stub.Compat
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Debug.Stub.Compat (
  MyGhcDebugSocket (..),
  withMyGhcDebug,
  maybeMyGhcDebugSocketParser,
) where

import Control.Applicative (asum)
import GHC.Eventlog.Live.Logger (Logger)
import Options.Applicative qualified as O
import Options.Applicative.Extra.Feature (Feature (..))
import Options.Applicative.Extra.Feature qualified as OF

#ifdef EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB
import Data.Text qualified as T
import GHC.Debug.Stub qualified as GHC.Debug (withGhcDebug, withGhcDebugTCP, withGhcDebugUnix)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (writeLog)
import System.Exit (exitFailure)
import Text.Read (readEither)
#else
import Data.Maybe (isJust)
import Control.Monad (when)
#endif

--------------------------------------------------------------------------------
-- Feature: use-ghc-debug-stub
--------------------------------------------------------------------------------

useGhcDebugStub :: Feature
useGhcDebugStub = Feature{flag = "use-ghc-debug-stub", isOn = isOn, info = "Cannot open ghc-debug socket."}
 where
  isOn :: Bool
#ifdef EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB
  isOn = True
#else
  isOn = False
#endif

--------------------------------------------------------------------------------
-- My GHC Debug
--------------------------------------------------------------------------------

data MyGhcDebugSocket
  = MyGhcDebugSocketDefault
  | MyGhcDebugSocketUnix FilePath
  | MyGhcDebugSocketTcp String
  deriving (Show)

{- |
Internal helper.
Start @ghc-debug@ on the given `MyGhcDebugSocket`.
-}
withMyGhcDebug :: Logger IO -> Maybe MyGhcDebugSocket -> IO a -> IO a
#ifdef EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB
withMyGhcDebug logger maybeMyGhcDebugSocket action =
  case maybeMyGhcDebugSocket of
    Nothing -> action
    Just MyGhcDebugSocketDefault -> do
      writeLog logger INFO $
        "Start ghc-debug with default socket."
      GHC.Debug.withGhcDebug action
    Just (MyGhcDebugSocketUnix myGhcDebugSocketUnix) -> do
      writeLog logger INFO $
        "Start ghc-debug with Unix domain socket at " <> T.pack myGhcDebugSocketUnix <> "."
      GHC.Debug.withGhcDebugUnix myGhcDebugSocketUnix action
    Just (MyGhcDebugSocketTcp myGhcDebugSocketTcp) -> do
      let (host, port) = break (== ':') myGhcDebugSocketTcp
      writeLog logger INFO $
        "Start ghc-debug with TCP/IP socket at " <> T.pack host <> ":" <> T.pack port <> "."
      case readEither port of
        Left _parseError -> do
          writeLog logger FATAL $
            T.pack $ "Could not parse ghc-debug TCP address " <> myGhcDebugSocketTcp <> "."
          exitFailure
        Right portWord16 ->
          GHC.Debug.withGhcDebugTCP host portWord16 action
#else
withMyGhcDebug logger maybeMyGhcDebugSocket action = do
  when (isJust maybeMyGhcDebugSocket) $
    OF.exitIfUnsupported useGhcDebugStub logger
  action
#endif

--------------------------------------------------------------------------------
-- My GHC Debug

maybeMyGhcDebugSocketParser :: O.Parser (Maybe MyGhcDebugSocket)
maybeMyGhcDebugSocketParser =
  asum $
    [ myGhcDebugSocketDefaultParser
    , myGhcDebugSocketUnixParser
    , myGhcDebugSocketTcpParser
    , pure Nothing
    ]

myGhcDebugSocketDefaultParser :: O.Parser (Maybe MyGhcDebugSocket)
myGhcDebugSocketDefaultParser =
  OF.onlyFor useGhcDebugStub (O.flag' $ Just MyGhcDebugSocketDefault) mempty $
    O.long "my-ghc-debug-socket"
      <> OF.helpFor useGhcDebugStub "Open the default ghc-debug socket for this program."

myGhcDebugSocketUnixParser :: O.Parser (Maybe MyGhcDebugSocket)
myGhcDebugSocketUnixParser =
  OF.onlyFor useGhcDebugStub (O.option (Just . MyGhcDebugSocketUnix <$> O.str)) (O.metavar "FILE") $
    O.long "my-ghc-debug-socket-unix"
      <> OF.helpFor useGhcDebugStub "Open a ghc-debug Unix domain socket with the given file path."

myGhcDebugSocketTcpParser :: O.Parser (Maybe MyGhcDebugSocket)
myGhcDebugSocketTcpParser =
  OF.onlyFor useGhcDebugStub (O.option (Just . MyGhcDebugSocketTcp <$> O.str)) (O.metavar "ADDRESS") $
    O.long "my-ghc-debug-socket-tcp"
      <> OF.helpFor useGhcDebugStub "Open a ghc-debug TCP/IP socket with the given address as 'host:port'."
