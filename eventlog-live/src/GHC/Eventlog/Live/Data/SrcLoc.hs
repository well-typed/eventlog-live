{- |
Module      : GHC.Eventlog.Live.Data.SrcLoc
Description : Representation for GHC source locations.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.SrcLoc (
  SrcLoc (SrcLoc, srcFilePath, srcRange),
  Range (Range, start, end, ..),
  Point (..),
) where

import Data.Char (isDigit)
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
