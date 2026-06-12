{- |
Module      : GHC.Eventlog.Live.Data.InfoProv
Description : Core machines for processing data in batches.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.InfoProv (
  InfoProvPtr (..),
  InfoProv (..),
) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Word (Word64)
import Numeric (showHex)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read.Lex (readHexP)

{- |
The type of info table provenance pointers.
-}
newtype InfoProvPtr = InfoProvPtr Word64
  deriving newtype (Eq, Hashable, Ord)

instance Show InfoProvPtr where
  showsPrec :: Int -> InfoProvPtr -> ShowS
  showsPrec _ (InfoProvPtr ptr) = showString "0x" . showHex ptr

instance Read InfoProvPtr where
  readsPrec :: Int -> ReadS InfoProvPtr
  readsPrec _ = P.readP_to_S (InfoProvPtr <$> (P.string "0x" *> readHexP))

{- |
The type of an info table provenance entry, as produced by the `GHC.RTS.Events.InfoTableProv` event.
-}
data InfoProv = InfoProv
  { ipPtr :: !InfoProvPtr -- TODO: Remove this field.
  , ipName :: !Text
  , ipClosureDesc :: !Int
  , ipTyDesc :: !Text
  , ipLabel :: !Text
  , ipModule :: !Text
  , ipSrcLoc :: !Text
  }
  deriving (Show, Eq, Ord)
