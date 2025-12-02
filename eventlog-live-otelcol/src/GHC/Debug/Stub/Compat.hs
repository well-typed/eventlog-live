{-# LANGUAGE CPP #-}

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
import Data.Text qualified as T (pack)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Logger (LogAction, (&>), (<&))
import Options.Applicative qualified as O

#if defined(EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB)
import GHC.Debug.Stub qualified as GHC.Debug (withGhcDebug, withGhcDebugTCP, withGhcDebugUnix)
import System.Exit (exitFailure)
import Text.Read (readEither)
#else
import Data.Maybe (isJust)
import System.Exit (exitFailure)
import Options.Applicative.Help.Pretty qualified as OP
#endif

--------------------------------------------------------------------------------
-- My GHC Debug

data MyGhcDebugSocket
  = MyGhcDebugSocketDefault
  | MyGhcDebugSocketUnix FilePath
  | MyGhcDebugSocketTcp String
  deriving (Show)

{- |
Internal helper.
Start @ghc-debug@ on the given `MyGhcDebugSocket`.
-}
withMyGhcDebug :: LogAction IO -> Maybe MyGhcDebugSocket -> IO a -> IO a
#if defined(EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB)
withMyGhcDebug logAction maybeMyGhcDebugSocket action =
  case maybeMyGhcDebugSocket of
    Nothing -> action
    Just MyGhcDebugSocketDefault ->
      GHC.Debug.withGhcDebug action
    Just (MyGhcDebugSocketUnix myGhcDebugSocketUnix) ->
      GHC.Debug.withGhcDebugUnix myGhcDebugSocketUnix action
    Just (MyGhcDebugSocketTcp myGhcDebugSocketTcp) -> do
      let (host, port) = break (== ':') myGhcDebugSocketTcp
      case readEither port of
        Left _parseError -> do
          let msg = T.pack $ "Could not parse ghc-debug TCP address " <> myGhcDebugSocketTcp <> "."
          logAction <& FATAL &> msg
          exitFailure
        Right portWord16 ->
          GHC.Debug.withGhcDebugTCP host portWord16 action
#else
withMyGhcDebug logAction maybeMyGhcDebugSocket action
  | isJust maybeMyGhcDebugSocket = do
    let msg = T.pack myGhcDebugSocketUnsupportedErrorMessage
    logAction <& FATAL &> msg
    exitFailure
  | otherwise = action
#endif

--------------------------------------------------------------------------------
-- My GHC Debug

maybeMyGhcDebugSocketParser :: O.Parser (Maybe MyGhcDebugSocket)
#if defined(EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB)
maybeMyGhcDebugSocketParser =
  O.optional . asum $
    [ myGhcDebugSocketDefaultParser
    , myGhcDebugSocketUnixParser
    , myGhcDebugSocketTcpParser
    ]
 where
  myGhcDebugSocketDefaultParser =
    O.flag'
      MyGhcDebugSocketDefault
      ( O.long myGhcDebugSocketDefaultLong
          <> O.help myGhcDebugSocketDefaultHelp
      )
  myGhcDebugSocketUnixParser =
    MyGhcDebugSocketUnix
      <$> O.strOption
        ( O.long myGhcDebugSocketUnixLong
            <> O.metavar myGhcDebugSocketUnixMetavar
            <> O.help myGhcDebugSocketUnixHelp
        )
  myGhcDebugSocketTcpParser =
    MyGhcDebugSocketUnix
      <$> O.strOption
        ( O.long myGhcDebugSocketTcpLong
            <> O.metavar myGhcDebugSocketTcpMetavar
            <> O.help myGhcDebugSocketTcpHelp
        )
#else
maybeMyGhcDebugSocketParser =
  asum . fmap mkUnsupportedParser $
    [ myGhcDebugSocketDefaultMod
    , myGhcDebugSocketUnixMod
    , myGhcDebugSocketTcpMod
    ]
 where
  mkUnsupportedParser modOptionFields =
    Nothing <$ O.infoOption myGhcDebugSocketUnsupportedErrorMessage modOptionFields
  mkUnsupportedHelpDoc help =
    Just $ OP.vcat [OP.pretty $ "Unsupported. Requires build with -f+" <> myGhcDebugFeatureFlag <> ".", OP.pretty help]
  myGhcDebugSocketDefaultMod =
     O.long myGhcDebugSocketDefaultLong
      <> O.hidden
      <> O.helpDoc (mkUnsupportedHelpDoc myGhcDebugSocketDefaultHelp)
  myGhcDebugSocketUnixMod =
      O.long myGhcDebugSocketUnixLong
      <> O.metavar myGhcDebugSocketUnixMetavar
      <> O.hidden
      <> O.helpDoc (mkUnsupportedHelpDoc myGhcDebugSocketUnixHelp)
  myGhcDebugSocketTcpMod =
      O.long myGhcDebugSocketTcpLong
      <> O.metavar myGhcDebugSocketTcpMetavar
      <> O.hidden
      <> O.helpDoc (mkUnsupportedHelpDoc myGhcDebugSocketTcpHelp)
#endif

#if !defined(EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB)
myGhcDebugFeatureFlag :: String
myGhcDebugFeatureFlag =
  "use-ghc-debug-stub"
#endif

#if !defined(EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB)
myGhcDebugSocketUnsupportedErrorMessage :: String
myGhcDebugSocketUnsupportedErrorMessage =
  "Cannot open ghc-debug socket. This executable was built without -f+" <> myGhcDebugFeatureFlag <> "."
{-# INLINE myGhcDebugSocketUnsupportedErrorMessage #-}
#endif

myGhcDebugSocketDefaultLong :: String
myGhcDebugSocketDefaultLong =
  "enable-my-ghc-debug-socket"
{-# INLINE myGhcDebugSocketDefaultLong #-}

myGhcDebugSocketDefaultHelp :: String
myGhcDebugSocketDefaultHelp =
  "Enable ghc-debug for this program."
{-# INLINE myGhcDebugSocketDefaultHelp #-}

myGhcDebugSocketUnixLong :: String
myGhcDebugSocketUnixLong =
  "enable-my-ghc-debug-socket-unix"
{-# INLINE myGhcDebugSocketUnixLong #-}

myGhcDebugSocketUnixMetavar :: String
myGhcDebugSocketUnixMetavar =
  "SOCKET"
{-# INLINE myGhcDebugSocketUnixMetavar #-}

myGhcDebugSocketUnixHelp :: String
myGhcDebugSocketUnixHelp =
  "Enable ghc-debug for this program on the given Unix socket."
{-# INLINE myGhcDebugSocketUnixHelp #-}

myGhcDebugSocketTcpLong :: String
myGhcDebugSocketTcpLong =
  "enable-my-ghc-debug-socket-tcp"
{-# INLINE myGhcDebugSocketTcpLong #-}

myGhcDebugSocketTcpMetavar :: String
myGhcDebugSocketTcpMetavar =
  "ADDRESS"
{-# INLINE myGhcDebugSocketTcpMetavar #-}

myGhcDebugSocketTcpHelp :: String
myGhcDebugSocketTcpHelp =
  "Enable ghc-debug for this program on the given TCP socket specified as 'host:port'."
{-# INLINE myGhcDebugSocketTcpHelp #-}
