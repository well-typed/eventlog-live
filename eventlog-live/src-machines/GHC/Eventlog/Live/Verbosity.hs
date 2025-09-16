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
  = VerbosityWarning
  | VerbosityError
  | VerbosityQuiet
  deriving (Eq, Ord)

{- |
Pretty-printer for t`Verbosity`.
-}
showVerbosity :: Verbosity -> Text
showVerbosity = \case
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
