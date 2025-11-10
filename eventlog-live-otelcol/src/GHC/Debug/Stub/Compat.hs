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
) where

import Data.Text qualified as T (pack)
import GHC.Eventlog.Live.Logger (logError)
import GHC.Eventlog.Live.Verbosity (Verbosity)

#if defined(EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB)
import GHC.Debug.Stub qualified as GHC.Debug (withGhcDebug, withGhcDebugTCP, withGhcDebugUnix)
import System.Exit (exitFailure)
import Text.Read (readEither)
#else
import Data.Maybe (isJust)
import System.Exit (exitFailure)
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
withMyGhcDebug :: Verbosity -> Maybe MyGhcDebugSocket -> IO a -> IO a
#if defined(EVENTLOG_LIVE_OTELCOL_USE_GHC_DEBUG_STUB)
withMyGhcDebug verbosity maybeMyGhcDebugSocket action =
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
          logError verbosity . T.pack $
            "Could not parse ghc-debug TCP address " <> myGhcDebugSocketTcp <> "."
          exitFailure
        Right portWord16 ->
          GHC.Debug.withGhcDebugTCP host portWord16 action
#else
withMyGhcDebug verbosity maybeMyGhcDebugSocket action
  | isJust maybeMyGhcDebugSocket = do
    logError verbosity . T.pack $
      "This binary was compiled without support for ghc-debug."
    action
  | otherwise = exitFailure
#endif
