module Data.Binary.Text (
  getTextUtf8,
  putTextUtf8,
) where

import Data.Binary (Binary (..), Get, Put)
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
