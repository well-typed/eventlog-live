module GHC.Eventlog.InfoProv (
  InfoProv (..),
  IpeId (..),
  prettyIpeId,
) where

import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite
import GHC.Generics (Generic)
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
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Sqlite.FromRow, Sqlite.ToRow)

newtype IpeId = IpeId {id :: Word64}
  deriving (Eq, Ord)
  deriving newtype (Sqlite.FromField, Sqlite.ToField)

instance Show IpeId where
  show ipeId = "IpeId {id = " <> showAsHex (ipeId.id) <> "}"

showAsHex :: (Integral a) => a -> String
showAsHex n = "0x" <> Numeric.showHex n ""

prettyIpeId :: IpeId -> Text
prettyIpeId = Text.pack . showAsHex . (.id)
