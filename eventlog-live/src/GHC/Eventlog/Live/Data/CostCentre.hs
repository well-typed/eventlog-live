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
import GHC.Eventlog.Live.Data.SrcLoc (SrcLoc)
import GHC.Generics (Generic)

newtype CostCentreId = CostCentreId
  { value :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

data CostCentre = CostCentre
  { ccId :: !CostCentreId
  , ccLabel :: !Text
  , ccModule :: !Text
  , ccSrcLoc :: !SrcLoc
  , ccIsCAF :: !Bool
  }
  deriving stock (Show, Eq, Generic)
