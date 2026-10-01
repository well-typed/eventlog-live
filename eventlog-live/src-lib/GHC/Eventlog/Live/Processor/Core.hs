{- |
Module      : GHC.Eventlog.Live.Processor.Core
Description : Common utilities shared across telemetry data types.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Processor.Core (
  runIf,
  runWith,
)
where

import Data.Machine (MachineT, stopped)

-- | Run a machine if a boolean is @True@, otherwise stop.
runIf :: (Monad m) => Bool -> MachineT m k o -> MachineT m k o
runIf b m = if b then m else stopped

-- | Run a machine with the value from a @Maybe a@, otherwise stop.
runWith :: (Monad m) => Maybe a -> (a -> MachineT m k o) -> MachineT m k o
runWith ma mf = maybe stopped mf ma
