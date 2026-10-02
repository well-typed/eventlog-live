{- |
Module      : GHC.Eventlog.Live.Attribute
Description : Representation for OTLP attributes.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Types.Attribute (
  Attrs,
  lookup,
  toList,
  Attr,
  AttrKey,
  AttrValue (..),
  IsAttrValue (..),
  (~=),
) where

import Data.Aeson (ToJSON)
import Data.Aeson.Encoding (null_)
import Data.Aeson.Types (Encoding, ToJSON (..), Value (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version, showVersion)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import GHC.IsList (IsList (..))
import Prelude hiding (lookup)

{- |
A set of attributes is a t`HashMap`
-}
newtype Attrs = Attrs {attrMap :: HashMap AttrKey AttrValue}
  deriving (Eq, Generic, Show)

lookup :: AttrKey -> Attrs -> Maybe AttrValue
lookup attrKey attrs = M.lookup attrKey attrs.attrMap

instance Hashable Attrs

instance Semigroup Attrs where
  (<>) :: Attrs -> Attrs -> Attrs
  x <> y = Attrs{attrMap = x.attrMap <> y.attrMap}

instance Monoid Attrs where
  mempty = Attrs mempty

instance IsList Attrs where
  type Item Attrs = Attr

  fromList :: [Item Attrs] -> Attrs
  fromList = Attrs . M.fromList

  toList :: Attrs -> [Item Attrs]
  toList = M.toList . (.attrMap)

instance ToJSON Attrs where
  toJSON :: Attrs -> Value
  toJSON Attrs{..} = toJSON attrMap

  toEncoding :: Attrs -> Encoding
  toEncoding Attrs{..} = toEncoding attrMap

  omitField :: Attrs -> Bool
  omitField Attrs{..} = M.null attrMap

{- |
An attribute is a key-value pair where the key is any string and the value is
some numeric type, string, or null. Attributes should be constructed using the
`(~=)` operator, which automatically converts Haskell types to t`AttrValue`.
-}
type Attr = (AttrKey, AttrValue)

{- |
Construct an t`Attr` as a pair of an t`AttrKey` and an t`AttrValue`,
constructed via the t`IsAttrValue` class.
-}
(~=) :: (IsAttrValue v) => AttrKey -> v -> Attr
k ~= v = (ak, av)
 where
  !ak = k
  !av = toAttrValue v
{-# INLINE (~=) #-}

{- |
The type of attribute keys.
-}
type AttrKey =
  Text

{- |
The type of attribute values.
-}
data AttrValue
  = AttrBool !Bool
  | AttrInt !Int
  | AttrInt8 !Int8
  | AttrInt16 !Int16
  | AttrInt32 !Int32
  | AttrInt64 !Int64
  | AttrWord !Word
  | AttrWord8 !Word8
  | AttrWord16 !Word16
  | AttrWord32 !Word32
  | AttrWord64 !Word64
  | AttrDouble !Double
  | AttrText !Text
  | AttrNull
  deriving (Eq, Generic, Show)

instance Hashable AttrValue

instance ToJSON AttrValue where
  toJSON :: AttrValue -> Value
  toJSON = \case
    AttrBool v -> toJSON v
    AttrInt v -> toJSON v
    AttrInt8 v -> toJSON v
    AttrInt16 v -> toJSON v
    AttrInt32 v -> toJSON v
    AttrInt64 v -> toJSON v
    AttrWord v -> toJSON v
    AttrWord8 v -> toJSON v
    AttrWord16 v -> toJSON v
    AttrWord32 v -> toJSON v
    AttrWord64 v -> toJSON v
    AttrDouble v -> toJSON v
    AttrText v -> toJSON v
    AttrNull -> Null

  toEncoding :: AttrValue -> Encoding
  toEncoding = \case
    AttrBool v -> toEncoding v
    AttrInt v -> toEncoding v
    AttrInt8 v -> toEncoding v
    AttrInt16 v -> toEncoding v
    AttrInt32 v -> toEncoding v
    AttrInt64 v -> toEncoding v
    AttrWord v -> toEncoding v
    AttrWord8 v -> toEncoding v
    AttrWord16 v -> toEncoding v
    AttrWord32 v -> toEncoding v
    AttrWord64 v -> toEncoding v
    AttrDouble v -> toEncoding v
    AttrText v -> toEncoding v
    AttrNull -> null_

{- |
Utility class to help construct values of the t`AttrValue` type.
-}
class IsAttrValue v where
  toAttrValue :: v -> AttrValue

instance IsAttrValue AttrValue where
  toAttrValue :: AttrValue -> AttrValue
  toAttrValue = id
  {-# INLINE toAttrValue #-}

instance IsAttrValue Bool where
  toAttrValue :: Bool -> AttrValue
  toAttrValue = AttrBool
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int where
  toAttrValue :: Int -> AttrValue
  toAttrValue = AttrInt
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int8 where
  toAttrValue :: Int8 -> AttrValue
  toAttrValue = AttrInt8
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int16 where
  toAttrValue :: Int16 -> AttrValue
  toAttrValue = AttrInt16
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int32 where
  toAttrValue :: Int32 -> AttrValue
  toAttrValue = AttrInt32
  {-# INLINE toAttrValue #-}

instance IsAttrValue Int64 where
  toAttrValue :: Int64 -> AttrValue
  toAttrValue = AttrInt64
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word where
  toAttrValue :: Word -> AttrValue
  toAttrValue = AttrWord
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word8 where
  toAttrValue :: Word8 -> AttrValue
  toAttrValue = AttrWord8
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word16 where
  toAttrValue :: Word16 -> AttrValue
  toAttrValue = AttrWord16
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word32 where
  toAttrValue :: Word32 -> AttrValue
  toAttrValue = AttrWord32
  {-# INLINE toAttrValue #-}

instance IsAttrValue Word64 where
  toAttrValue :: Word64 -> AttrValue
  toAttrValue = AttrWord64
  {-# INLINE toAttrValue #-}

instance IsAttrValue Double where
  toAttrValue :: Double -> AttrValue
  toAttrValue = AttrDouble
  {-# INLINE toAttrValue #-}

instance IsAttrValue String where
  toAttrValue :: String -> AttrValue
  toAttrValue = AttrText . T.pack
  {-# INLINE toAttrValue #-}

instance IsAttrValue Text where
  toAttrValue :: Text -> AttrValue
  toAttrValue = AttrText
  {-# INLINE toAttrValue #-}

instance IsAttrValue Version where
  toAttrValue :: Version -> AttrValue
  toAttrValue = toAttrValue . showVersion
  {-# INLINE toAttrValue #-}

instance (IsAttrValue v) => IsAttrValue (Maybe v) where
  toAttrValue :: Maybe v -> AttrValue
  toAttrValue = maybe AttrNull toAttrValue
  {-# INLINE toAttrValue #-}
