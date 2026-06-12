{- |
Module      : GHC.Eventlog.Live.Data.SrcLoc
Description : Core machines for processing data in batches.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.SrcLoc (
  SrcSpan (..),
  srcSpanShowS,
  srcSpanReadP,
  srcSpanReadS,
  SrcLoc (..),
  srcLocShowS,
  srcLocReadP,
  srcLocReadS,
) where

import Control.Applicative (asum)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

{- Note [Parsing and Pretty-Printing]
The parser and pretty-printer are based on the following pretty-printer for @RealSrcSpan@ in the GHC source:
https://github.com/ghc/ghc/blob/674858e3fb1b7fe644d7ec5d4b0c6d9f18d7bc51/compiler/GHC/Types/SrcLoc.hs#L752-L774
-}

data SrcSpan
  = SrcSpan'Point
      { srcSpanLine :: !Int
      , srcSpanColumn :: !Int
      }
  | SrcSpan'Line
      { srcSpanLine :: !Int
      , srcSpanColumn :: !Int
      , srcSpanEndColumn :: !Int
      }
  | SrcSpan
      { srcSpanLine :: !Int
      , srcSpanColumn :: !Int
      , srcSpanEndLine :: !Int
      , srcSpanEndColumn :: !Int
      }

srcSpanShowS :: SrcSpan -> ShowS
srcSpanShowS = \case
  SrcSpan'Point{..} ->
    shows srcSpanLine . showString ":" . shows srcSpanColumn
  SrcSpan'Line{..} ->
    shows srcSpanLine . showString ":" . shows srcSpanColumn . showString "-" . shows srcSpanEndColumn
  SrcSpan{..} ->
    showParen True (shows srcSpanLine . showString "," . shows srcSpanColumn)
      . showString "-"
      . showParen True (shows srcSpanEndLine . showString "," . shows srcSpanEndColumn)

srcSpanReadP :: ReadP SrcSpan
srcSpanReadP =
  asum
    [ SrcSpan'Point
        <$> nonNegativeIntReadP
        <* P.char ':'
        <*> nonNegativeIntReadP
    , SrcSpan'Line
        <$> nonNegativeIntReadP
        <* P.char ':'
        <*> nonNegativeIntReadP
        <* P.char '-'
        <*> nonNegativeIntReadP
    , SrcSpan
        <$ P.char '('
        <*> nonNegativeIntReadP
        <* P.char ','
        <*> nonNegativeIntReadP
        <* P.char ')'
        <* P.char '-'
        <* P.char '('
        <*> nonNegativeIntReadP
        <* P.char ','
        <*> nonNegativeIntReadP
        <* P.char ')'
    ]
 where
  nonNegativeIntReadP :: ReadP Int
  nonNegativeIntReadP = read <$> P.munch1 isDigit

srcSpanReadS :: ReadS SrcSpan
srcSpanReadS = P.readP_to_S srcSpanReadP

data SrcLoc = SrcLoc
  { srcFile :: !FilePath
  , srcSpan :: !SrcSpan
  }

srcLocShowS :: SrcLoc -> ShowS
srcLocShowS SrcLoc{..} =
  showString srcFile . showString ":" . srcSpanShowS srcSpan

srcLocReadP :: ReadP SrcLoc
srcLocReadP =
  SrcLoc <$> P.munch (not . (== ':')) <* P.char ':' <*> srcSpanReadP

srcLocReadS :: ReadS SrcLoc
srcLocReadS = P.readP_to_S srcLocReadP
