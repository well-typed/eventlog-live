{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Verbosity
Description : Logging verbosity for eventlog machines.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Verbosity (
  Verbosity,
  showVerbosity,
  verbosityQuiet,
  verbosityError,
  verbosityWarning,
  verbosityInfo,
  verbosityDebug,
) where

import Data.Text (Text)

-------------------------------------------------------------------------------
-- Verbosity
-------------------------------------------------------------------------------

{- |
The type of logging verbosities supported by the machines
in "GHC.Eventlog.Live.Machines".
-}
data Verbosity
  = VerbosityDebug
  | VerbosityInfo
  | VerbosityWarning
  | VerbosityError
  | VerbosityQuiet
  deriving (Eq, Ord)

{- |
Pretty-printer for t`Verbosity`.
-}
showVerbosity :: Verbosity -> Text
showVerbosity = \case
  VerbosityDebug -> "Debug"
  VerbosityInfo -> "Info"
  VerbosityWarning -> "Warning"
  VerbosityError -> "Error"
  VerbosityQuiet -> "Quiet"

{- |
Quiet t`Verbosity`.
-}
verbosityQuiet :: Verbosity
verbosityQuiet = VerbosityQuiet

{- |
Error t`Verbosity`.
-}
verbosityError :: Verbosity
verbosityError = VerbosityError

{- |
Warning t`Verbosity`.
-}
verbosityWarning :: Verbosity
verbosityWarning = VerbosityWarning

{- |
Info t`Verbosity`.
-}
verbosityInfo :: Verbosity
verbosityInfo = VerbosityInfo

{- |
Debug t`Verbosity`.
-}
verbosityDebug :: Verbosity
verbosityDebug = VerbosityDebug
