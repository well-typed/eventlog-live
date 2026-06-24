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

import Data.Binary (Binary (..), Get, Put)
import Data.Binary.Text (getTextUtf8, putTextUtf8)
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

--------------------------------------------------------------------------------
-- Instances for binary serialisation of CostCentre
--------------------------------------------------------------------------------

deriving newtype instance Binary CostCentreId

instance Binary CostCentre where
  get :: Get CostCentre
  get = do
    ccId <- get
    ccLabel <- getTextUtf8
    ccModule <- getTextUtf8
    ccSrcLoc <- getTextUtf8
    ccIsCAF <- get
    pure CostCentre{..}

  put :: CostCentre -> Put
  put CostCentre{..} = do
    put ccId
    putTextUtf8 ccLabel
    putTextUtf8 ccModule
    putTextUtf8 ccSrcLoc
    put ccIsCAF
