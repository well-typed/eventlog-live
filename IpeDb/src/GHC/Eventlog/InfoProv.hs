module GHC.Eventlog.InfoProv (
  InfoProv (..),
  IpeId (..),
) where

import Data.Int
import Data.Text (Text)
import Data.Word
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite
import qualified Numeric

data InfoProv = InfoProv
  { infoId :: !IpeId
  , tableName :: !Text
  , closureDesc :: !Int64
  , typeDesc :: !Text
  , label :: !Text
  , moduleName :: !Text
  , srcLoc :: !Text
  }
  deriving (Show, Eq, Ord)

newtype IpeId = IpeId {id :: Word64}
  deriving (Eq, Ord)
  deriving newtype (Sqlite.FromField, Sqlite.ToField)

instance Show IpeId where
  show ipeId = "IpeId {id = " <> showAsHex (ipeId.id) <> "}"

showAsHex :: (Integral a) => a -> String
showAsHex n = "0x" <> Numeric.showHex n ""
