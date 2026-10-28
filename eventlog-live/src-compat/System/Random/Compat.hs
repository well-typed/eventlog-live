{-# LANGUAGE CPP #-}

module System.Random.Compat (
  uniformByteString,
) where

#if MIN_VERSION_random(1,3,0)
import System.Random (uniformByteString)
#else
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import System.Random (RandomGen (genShortByteString))

uniformByteString :: RandomGen g => Int -> g -> (ByteString, g)
uniformByteString n g = first fromShort (genShortByteString n g)
#endif
