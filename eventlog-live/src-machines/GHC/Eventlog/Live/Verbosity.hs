{-# LANGUAGE OverloadedStrings #-}

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

data Verbosity
  = VerbosityWarning
  | VerbosityError
  | VerbosityQuiet
  deriving (Eq, Ord)

{- |
Pretty-printer for `Verbosity`.
-}
showVerbosity :: Verbosity -> Text
showVerbosity = \case
  VerbosityWarning -> "Warning"
  VerbosityError -> "Error"
  VerbosityQuiet -> "Quiet"

{- |
Quiet `Verbosity`.
-}
verbosityQuiet :: Verbosity
verbosityQuiet = VerbosityQuiet

{- |
Error `Verbosity`.
-}
verbosityError :: Verbosity
verbosityError = VerbosityError

{- |
Warning `Verbosity`.
-}
verbosityWarning :: Verbosity
verbosityWarning = VerbosityWarning
