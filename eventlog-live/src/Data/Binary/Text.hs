module Data.Binary.Text (
  getTextUtf8,
  putTextUtf8,
  getByteStringLEB128,
  putByteStringLEB128,
) where

import Codec.LEB128.Generic (decodeLEB128, encodeLEB128)
import Data.Binary (Binary (..))
import Data.Binary.Get (Get, getByteString, getWord8)
import Data.Binary.Put (Put, putByteString, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

{- |
Decode `Text` into a UTF-8 `ByteString`.
-}
getTextUtf8 :: Get Text
getTextUtf8 = TE.decodeUtf8 <$> get

{- |
Encode `Text` from a UTF-8 `ByteString`.
-}
putTextUtf8 :: Text -> Put
putTextUtf8 = put . TE.encodeUtf8

{- |
Encode a `ByteString` using a LEB128-encoded length.
-}
getByteStringLEB128 :: Get ByteString
getByteStringLEB128 = do
  n <- decodeLEB128 getWord8
  getByteString n

{- |
Decode a `ByteString` using a LEB128-encoded length.
-}
putByteStringLEB128 :: ByteString -> Put
putByteStringLEB128 bs = do
  encodeLEB128 putWord8 (BS.length bs)
  putByteString bs
