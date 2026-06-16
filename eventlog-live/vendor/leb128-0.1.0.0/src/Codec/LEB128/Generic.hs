{- HLINT ignore -}
{- FOURMOLU_DISABLE -}
{- |
    Module      : Codec.LEB128.Generic
    Description : Encode values via (S)LEB128
    Copyright   : (c) Andreas Klebinger 2020
    License     : BSD3
    Maintainer  : Andreas Klebinger
    Portability : GHC >= 7.10

     This module provides a generic interface over the encoding
     and decoding algorithm. It can be instantiated to a wide
     variate of types.

     Instantiations based on bytestring and lists are provided in the
     "Codec.LEB128.List" and "Codec.LEB128.Internal.BS" modules.

     Size checks for inputs or output types are not performed by default.
     However they can be included in the put/get functions if desired.
-}
module Codec.LEB128.Generic (
  encodeLEB128,
  decodeLEB128,
) where

import Data.Bits (Bits (clearBit, setBit, testBit, unsafeShiftL, unsafeShiftR, (.|.)))
import Data.Word (Word8)
import GHC.Exts (inline)

-- 2026-06-16:
-- The LEB128 constraint (present in the actual leb128 package) has been inlined.

{- |
LEB128-encode a unsigned value into a sequence of bytes.

For example to encode a integer into a list of words you might use.

> encodeLEB128 pure :: Integer -> [Word8]

To do the same using a serialization library like bytestrings builder:

> encodeLEB128 (B.word8)

For performance reasons it can be important to make sure @encodeLEB128@
is sufficiently specialized. One way to achieve this is to force inlining
using the @inline@ function from GHC.Magic (defined in the ghc-prim package).
For an efficient example generic over the value type this gives us for lists:

@
   toULEB128 :: (Integral a, Bits a) => a -> [Word8]
   toULEB128 = (inline G.encodeLEB128) pure
@

Results are undefined for negative numbers.
-}
encodeLEB128 :: forall a m. (Monoid m, Integral a, Bits a) => (Word8 -> m) -> a -> m
encodeLEB128 !putWord8 = go
 where
  go !i
    | i <= 127 =
        (inline putWord8) $! (fromIntegral i :: Word8)
    | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        let !byte = (setBit (fromIntegral i) 7)
         in (inline putWord8) byte <> go (i `unsafeShiftR` 7)
{-# INLINE encodeLEB128 #-}

{- |
LEB128-decodes a unsigned value given a monadic way to request bytes.

For example a implementation over a state monad might look like:

> execState . decodeLEB128 getByte

This pattern is used by the bytestring based decoder in this package.
See there for a complete example.
-}
decodeLEB128 :: forall a m. (Monad m, Integral a, Bits a) => m Word8 -> m a
decodeLEB128 getWord8 = go 0 0
 where
  go :: Int -> a -> m a
  go !shift !w = do
    byte <- getWord8
    let !byteVal = fromIntegral (clearBit byte 7)
    let !hasMore = testBit byte 7
    let !val = w .|. (byteVal `unsafeShiftL` shift)
    let !shift' = shift + 7
    if hasMore
      then go shift' val
      else return $! val
{-# INLINE decodeLEB128 #-}
