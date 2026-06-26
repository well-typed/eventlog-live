{- |
Module      : GHC.Eventlog.Live.Data.SrcLoc
Description : Representation for GHC source locations.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.SrcLoc (
  SrcLoc (SrcLoc, UnhelpfulSrcLoc, srcFilePath, srcRange),
  Range (Range, start, end, ..),
  Point (..),
) where

import Codec.LEB128.Generic (decodeLEB128, encodeLEB128)
import Data.Binary (Binary (..), Get, Put, getWord8, putWord8)
import Data.Binary.Text (getTextUtf8, putTextUtf8)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Text.ParserCombinators.ReadPrec qualified as P (readP_to_Prec)
import Text.Read (Read (..))

--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

{- |
The type of source location information.
-}
data SrcLoc = SrcLoc_
  { srcFilePath :: !(Maybe FilePath)
  -- ^ @srcFilePath@ should not be @Just ""@.
  , srcRange :: !(Maybe Range)
  }
  deriving stock (Eq, Generic)

{- |
Internal helper.

Smart constructor for `SrcLoc`.
-}
toSrcLoc :: Maybe FilePath -> Maybe Range -> SrcLoc
toSrcLoc srcFilePath =
  SrcLoc_ (if srcFilePath == Just "" then Nothing else srcFilePath)

{- |
Smart constructor for `SrcLoc` that ensures that `srcFilePath` is not @Just ""@.
-}
pattern SrcLoc :: Maybe FilePath -> Maybe Range -> SrcLoc
pattern SrcLoc{srcFilePath, srcRange} <- SrcLoc_ srcFilePath srcRange
  where
    SrcLoc srcFilePath srcRange = toSrcLoc srcFilePath srcRange

{-# COMPLETE SrcLoc #-}

{- |
Simple constructor for unhelpful source locations.
-}
pattern UnhelpfulSrcLoc :: SrcLoc
pattern UnhelpfulSrcLoc = SrcLoc_ Nothing Nothing

{- |
Internal helper.

Pretty-printer for source location information
-}
ppSrcLoc :: SrcLoc -> ShowS
ppSrcLoc SrcLoc{..} =
  maybe mempty showString srcFilePath . showString ":" . maybe mempty ppRange srcRange

instance Show SrcLoc where
  showsPrec :: Int -> SrcLoc -> ShowS
  showsPrec = const ppSrcLoc

{- |
Internal helper.

Parser for source location information.
-}
pSrcLoc :: ReadP SrcLoc
pSrcLoc = SrcLoc <$> pSrcFilePath <* P.char ':' <*> pSrcRange
 where
  pSrcFilePath :: ReadP (Maybe FilePath)
  pSrcFilePath = (Just <$> P.munch1 (/= ':')) P.<++ (Nothing <$ P.eof)

  pSrcRange :: ReadP (Maybe Range)
  pSrcRange = (Just <$> pRange) P.<++ (Nothing <$ P.eof)

instance Read SrcLoc where
  readPrec :: ReadPrec SrcLoc
  readPrec = P.readP_to_Prec (const pSrcLoc)

--------------------------------------------------------------------------------
-- Source Ranges
--------------------------------------------------------------------------------

{- |
A position in a source file.
-}
data Point
  = Point
  { line :: !Word32
  , column :: !Word32
  }
  deriving (Eq, Ord, Generic)

{- |
A range in a source file.
-}
data Range
  = Range'Point
      { line :: !Word32
      , column :: !Word32
      }
  | Range'OneLine
      { line :: !Word32
      , column :: !Word32
      , endColumn :: !Word32
      }
  | Range'MultiLine
      { line :: !Word32
      , column :: !Word32
      , endLine :: !Word32
      , endColumn :: !Word32
      }
  deriving stock (Eq, Generic)

{- |
Smart constructor for ranges.
-}
toRange :: (Point, Point) -> Range
toRange (start@Point{..}, end)
  | start == end = Range'Point{..}
  | line == end.line = Range'OneLine{endColumn = end.column, ..}
  | otherwise = Range'MultiLine{endLine = end.line, endColumn = end.column, ..}

{- |
Smart destructor for ranges.
-}
fromRange :: Range -> (Point, Point)
fromRange r = (r.start, r.end)

{- |
Smart constructor for ranges.
-}
pattern Range :: Point -> Point -> Range
pattern Range{start, end} <- (fromRange -> (start, end))
  where
    Range start end = toRange (start, end)

{-# COMPLETE Range #-}

{- |
Get the start position of a range.
-}
instance HasField "start" Range Point where
  getField :: Range -> Point
  getField = \case
    Range'Point{..} -> Point{..}
    Range'OneLine{..} -> Point{..}
    Range'MultiLine{..} -> Point{..}

{- |
Get the end position of a range.
-}
instance HasField "end" Range Point where
  getField :: Range -> Point
  getField = \case
    Range'Point{..} -> Point{..}
    Range'OneLine{..} -> Point{column = endColumn, ..}
    Range'MultiLine{..} -> Point{line = endLine, column = endColumn}

instance Semigroup Range where
  (<>) :: Range -> Range -> Range
  r1 <> r2 = Range (r1.start `min` r2.start) (r1.end `max` r2.end)

{- Note [Parsing and Pretty-Printing]
The parser and pretty-printer are based on the following pretty-printer for @RealSrcSpan@ in the GHC source:
https://github.com/ghc/ghc/blob/674858e3fb1b7fe644d7ec5d4b0c6d9f18d7bc51/compiler/GHC/Types/SrcLoc.hs#L752-L774
-}

{- |
Internal helper.

Pretty-printer for ranges.
-}
ppRange :: Range -> ShowS
ppRange = \case
  Range'Point{..} ->
    shows line . showString ":" . shows column
  Range'OneLine{..} ->
    shows line . showString ":" . shows column . showString "-" . shows endColumn
  Range{..} ->
    ppPoint start . showString "-" . ppPoint end
 where
  ppPoint :: Point -> ShowS
  ppPoint p =
    showParen True (shows p.line . showString "," . shows p.column)

instance Show Range where
  showsPrec :: Int -> Range -> ShowS
  showsPrec = const ppRange

{- |
Internal helper.

Parser for ranges.
-}
pRange :: ReadP Range
pRange = pRange'MultiLine P.<++ (pRange'OneLine P.+++ pPointRange)
 where
  pRange'MultiLine :: ReadP Range
  pRange'MultiLine = Range <$> pPoint <* P.char '-' <*> pPoint

  pRange'OneLine :: ReadP Range
  pRange'OneLine = Range'OneLine <$> pIndex <* P.char ':' <*> pIndex <* P.char '-' <*> pIndex

  pPointRange :: ReadP Range
  pPointRange = Range'Point <$> pIndex <* P.char ':' <*> pIndex

  pIndex :: ReadP Word32
  pIndex = read <$> P.munch1 isDigit

  pPoint :: ReadP Point
  pPoint = Point <$ P.char '(' <*> pIndex <* P.char ',' <*> pIndex <* P.char ')'

instance Read Range where
  readPrec :: ReadPrec Range
  readPrec = P.readP_to_Prec (const pRange)

--------------------------------------------------------------------------------
-- Instances for binary serialisation of SrcLoc
--------------------------------------------------------------------------------

{- |
Internal helper.

Serialise source location information.
-}
putSrcLoc :: SrcLoc -> Put
putSrcLoc SrcLoc{..} = do
  putTextUtf8 . T.pack . fromMaybe "" $ srcFilePath
  putMaybeRange srcRange

{- |
Internal helper.

Deserialise source location information.
-}
getSrcLoc :: Get SrcLoc
getSrcLoc = do
  srcFilePath <- Just . T.unpack <$> getTextUtf8
  srcRange <- getMaybeRange
  pure SrcLoc{..}

instance Binary SrcLoc where
  put :: SrcLoc -> Put
  put = putSrcLoc
  get :: Get SrcLoc
  get = getSrcLoc

--------------------------------------------------------------------------------
-- Instances for binary serialisation of optional ranges
--------------------------------------------------------------------------------

{- |
Internal helper.

Constructor tags for binary serialisation of ranges.
-}
data MaybeRangeTag
  = TagNothing
  | TagJustRange'Point
  | TagJustRange'OneLine
  | TagJustRange'MultiLine

{- |
Internal helper.

Serialise a constructor tag for an optional range.
-}
putMaybeRangeTag :: MaybeRangeTag -> Put
putMaybeRangeTag = \case
  TagNothing -> putWord8 0x01
  TagJustRange'Point -> putWord8 0x02
  TagJustRange'OneLine -> putWord8 0x03
  TagJustRange'MultiLine -> putWord8 0x04

{- |
Internal helper.

Deserialise a constructor tag for an optional range.
-}
getMaybeRangeTag :: Get MaybeRangeTag
getMaybeRangeTag = do
  maybeRangeTag <- getWord8
  case maybeRangeTag of
    0x01 -> pure TagNothing
    0x02 -> pure TagJustRange'Point
    0x03 -> pure TagJustRange'OneLine
    0x04 -> pure TagJustRange'MultiLine
    _otherwise -> fail $ "Unexpected tag " <> show maybeRangeTag

{- |
Internal helper.

Serialise an optional range to binary.
-}
putMaybeRange :: Maybe Range -> Put
putMaybeRange = \case
  Nothing ->
    putMaybeRangeTag TagNothing
  Just Range'Point{..} -> do
    putMaybeRangeTag TagJustRange'Point
    encodeLEB128 putWord8 line
    encodeLEB128 putWord8 column
  Just Range'OneLine{..} -> do
    putMaybeRangeTag TagJustRange'OneLine
    encodeLEB128 putWord8 line
    encodeLEB128 putWord8 column
    encodeLEB128 putWord8 endColumn
  Just Range'MultiLine{..} -> do
    putMaybeRangeTag TagJustRange'MultiLine
    encodeLEB128 putWord8 line
    encodeLEB128 putWord8 column
    encodeLEB128 putWord8 endLine
    encodeLEB128 putWord8 endColumn

{- |
Internal helper.

Deserialise an optional range from binary.
-}
getMaybeRange :: Get (Maybe Range)
getMaybeRange =
  getMaybeRangeTag >>= \case
    TagNothing ->
      pure Nothing
    TagJustRange'Point -> do
      line <- decodeLEB128 getWord8
      column <- decodeLEB128 getWord8
      pure . Just $! Range'Point{..}
    TagJustRange'OneLine -> do
      line <- decodeLEB128 getWord8
      column <- decodeLEB128 getWord8
      endColumn <- decodeLEB128 getWord8
      pure . Just $! Range'OneLine{..}
    TagJustRange'MultiLine -> do
      line <- decodeLEB128 getWord8
      column <- decodeLEB128 getWord8
      endLine <- decodeLEB128 getWord8
      endColumn <- decodeLEB128 getWord8
      pure . Just $! Range'MultiLine{..}
