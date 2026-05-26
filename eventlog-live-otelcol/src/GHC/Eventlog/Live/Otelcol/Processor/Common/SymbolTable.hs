{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Common.SymbolTable
Description : Abstract symbol table datatype.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Common.SymbolTable (
  SymbolIndex,
  SymbolTable,
  empty,
  elemIndex,
  toList,
  fromList,
  insert,
)
where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)

type SymbolIndex = Int32

data SymbolTable a
  = SymbolTable
  { nextSymbolIndex :: !SymbolIndex
  -- ^
  --   The next unused `SymbolIndex`.
  --
  --   > st.nextSymbolIndex == length st.entriesReversed
  , entryToSymbolIndex :: !(Map a SymbolIndex)
  -- ^
  --   A map from entries to their `SymbolIndex`.
  --
  --   > toList st !! (st.entryToSymbolIndex Map.! a) == a
  , entriesReversed :: ![a] -- reverse order of insertion into entryToSymbolIndex

  -- ^
  --   A list of entries in the `SymbolTable` in reverse order.
  }
  deriving (Show, Ord, Eq, Generic)

empty :: SymbolTable a
empty =
  SymbolTable
    { nextSymbolIndex = 0
    , entryToSymbolIndex = Map.empty
    , entriesReversed = []
    }

elemIndex :: (Ord a) => a -> SymbolTable a -> Maybe SymbolIndex
elemIndex a st = Map.lookup a st.entryToSymbolIndex

toList :: SymbolTable a -> [a]
toList st = reverse st.entriesReversed

fromList :: (Ord a) => [a] -> SymbolTable a
fromList = foldr (\val st -> snd (insert val st)) empty

insert :: (Ord a) => a -> SymbolTable a -> (SymbolIndex, SymbolTable a)
insert a st = (si, st'')
 where
  ((si, isNew, st'), entryToSymbolIndex') = Map.alterF (updateEntry st) a st.entryToSymbolIndex
  entriesReversed' = if isNew then a : st'.entriesReversed else st'.entriesReversed
  st'' = st'{entryToSymbolIndex = entryToSymbolIndex', entriesReversed = entriesReversed'}

updateEntry :: SymbolTable a -> Maybe SymbolIndex -> ((SymbolIndex, Bool, SymbolTable a), Maybe SymbolIndex)
updateEntry st Nothing = let (si, st') = freshSymbolIndex st in ((si, True, st'), Just si)
updateEntry st (Just si) = ((si, False, st), Just si)

freshSymbolIndex :: SymbolTable a -> (SymbolIndex, SymbolTable a)
freshSymbolIndex st = (st.nextSymbolIndex, st{nextSymbolIndex = st.nextSymbolIndex + 1})
