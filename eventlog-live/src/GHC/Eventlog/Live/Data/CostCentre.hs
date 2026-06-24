{- |
Module      : GHC.Eventlog.Live.Data.CostCentre
Description : Representation for GHC cost centres.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.CostCentre (
  CostCentreId (..),
  CostCentre (..),
) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Word (Word64)

{- |
The type of cost-centre IDs.
-}
newtype CostCentreId = CostCentreId
  { value :: Word64
  }
  deriving newtype (Show, Eq, Ord, Hashable)

{- |
The type of a cost-centre entry, as produced by the `GHC.RTS.Events.HeapProfCostCentre` event.
-}
data CostCentre = CostCentre
  { ccId :: !CostCentreId
  , ccLabel :: !Text
  , ccModule :: !Text
  , ccSrcLoc :: !Text
  , ccIsCAF :: !Bool
  }
  deriving stock (Show, Eq)
