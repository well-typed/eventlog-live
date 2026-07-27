{-# LANGUAGE CPP #-}

module Language.Haskell.TH.Lift.Compat (
  Exp,
  Lift (..),
  Q,
) where

#if defined(EVENTLOG_LIVE_OTELCOL_USE_TEMPLATE_HASKELL_LIFT)
import Language.Haskell.TH.Lift (Exp, Lift (..), Q)
#else
import Language.Haskell.TH.Syntax (Exp, Lift (..), Q)
#endif
