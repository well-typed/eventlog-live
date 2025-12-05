{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Profiles.Dictionary
Description : Abstraction over ProfilesDictionary for the OTLP protocol.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Profiles.Dictionary (
  -- * Dictionary for deduplication logic of common values
  ProfileDictionary,
  emptyProfileDictionary,

  -- * Turn a 'ProfileDictionary' into a 'OP.ProfilesDictionary'
  toProfilesDictionary,

  -- * Retrieve the 'SymbolIndex' for various 'OP.ProfilesData' fields
  SymbolIndex,
  getLocation,
  getFunction,
  getString,
  getMapping,
  getLink,
  getAttribute,
  getStack,
)
where

import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Data.Int
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.ProtoLens
import Data.Text (Text)
import GHC.Generics
import Lens.Family2
import Lens.Family2.Unchecked
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP

type SymbolIndex = Int32

data ProfileDictionary = ProfileDictionary
  { locationTable :: CommonSymbolTable OP.Location
  -- ^ Common 'OP.Location' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , functionTable :: CommonSymbolTable OP.Function
  -- ^ Common 'OP.Function' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , stringTable :: CommonSymbolTable Text
  -- ^ Common string table, the first entry must be "" per the protobuf
  --   documentation.
  --
  --   @
  --    // A common table for strings referenced by various messages.
  --    // string_table[0] must always be "".
  --    repeated string string_table = 5;
  --   @
  , mappingTable :: CommonSymbolTable OP.Mapping
  -- ^ Common 'OP.Mapping' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , linkTable :: CommonSymbolTable OP.Link
  -- ^ Common 'OP.Link' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , attributeTable :: CommonSymbolTable OP.KeyValueAndUnit
  -- ^ Common 'OP.KeyValueAndUnit' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , stackTable :: CommonSymbolTable OP.Stack
  -- ^ Common 'OP.Stack' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  }
  deriving (Show, Ord, Eq, Generic)

toProfilesDictionary :: ProfileDictionary -> OP.ProfilesDictionary
toProfilesDictionary st =
  defMessage
    & OP.locationTable
      .~ locationTableList st
    & OP.functionTable
      .~ functionTableList st
    & OP.stringTable
      .~ stringTableList st
    & OP.mappingTable
      .~ mappingTableList st
    & OP.linkTable
      .~ linkTableList st
    & OP.attributeTable
      .~ attributeTableList st
    & OP.stackTable
      .~ stackTableList st

emptyProfileDictionary :: ProfileDictionary
emptyProfileDictionary =
  ProfileDictionary
    { locationTable = commonSymbolTableFromList [defMessage]
    , functionTable = commonSymbolTableFromList [defMessage]
    , stringTable = commonSymbolTableFromList [""]
    , mappingTable = commonSymbolTableFromList [defMessage]
    , linkTable = commonSymbolTableFromList [defMessage]
    , attributeTable = commonSymbolTableFromList [defMessage]
    , stackTable = commonSymbolTableFromList [defMessage]
    }

locationTableList :: ProfileDictionary -> [OP.Location]
locationTableList st = reverse st.locationTable.contents

functionTableList :: ProfileDictionary -> [OP.Function]
functionTableList st = reverse st.functionTable.contents

stringTableList :: ProfileDictionary -> [Text]
stringTableList st = reverse st.stringTable.contents

mappingTableList :: ProfileDictionary -> [OP.Mapping]
mappingTableList st = reverse st.mappingTable.contents

linkTableList :: ProfileDictionary -> [OP.Link]
linkTableList st = reverse st.linkTable.contents

attributeTableList :: ProfileDictionary -> [OP.KeyValueAndUnit]
attributeTableList st = reverse st.attributeTable.contents

stackTableList :: ProfileDictionary -> [OP.Stack]
stackTableList st = reverse st.stackTable.contents

getSymbolIndexFor :: (Ord a, Monad m) => Lens' ProfileDictionary (CommonSymbolTable a) -> a -> StateT ProfileDictionary m SymbolIndex
getSymbolIndexFor accessor a = do
  tbl <- State.gets (^. accessor)
  let (idx, tbl1) = insertCommonSymbolTable a tbl
  State.modify' (\st -> st & accessor .~ tbl1)
  pure idx

getLocation :: (Monad m) => OP.Location -> StateT ProfileDictionary m SymbolIndex
getLocation = getSymbolIndexFor (lens (.locationTable) (\s v -> s{locationTable = v}))

getFunction :: (Monad m) => OP.Function -> StateT ProfileDictionary m SymbolIndex
getFunction = getSymbolIndexFor (lens (.functionTable) (\s v -> s{functionTable = v}))

getString :: (Monad m) => Text -> StateT ProfileDictionary m SymbolIndex
getString = getSymbolIndexFor (lens (.stringTable) (\s v -> s{stringTable = v}))

getMapping :: (Monad m) => OP.Mapping -> StateT ProfileDictionary m SymbolIndex
getMapping = getSymbolIndexFor (lens (.mappingTable) (\s v -> s{mappingTable = v}))

getLink :: (Monad m) => OP.Link -> StateT ProfileDictionary m SymbolIndex
getLink = getSymbolIndexFor (lens (.linkTable) (\s v -> s{linkTable = v}))

getAttribute :: (Monad m) => OP.KeyValueAndUnit -> StateT ProfileDictionary m SymbolIndex
getAttribute = getSymbolIndexFor (lens (.attributeTable) (\s v -> s{attributeTable = v}))

getStack :: (Monad m) => OP.Stack -> StateT ProfileDictionary m SymbolIndex
getStack = getSymbolIndexFor (lens (.stackTable) (\s v -> s{stackTable = v}))

-------------------------------------------------------------------------------
-- Common Symbol Table implementation
-- TODO: share with `ghc-stack-profiler-core` table?

data CommonSymbolTable a
  = CommonSymbolTable
  { counter :: !SymbolIndex
  , table :: Map a SymbolIndex
  , contents :: ![a]
  }
  deriving (Show, Ord, Eq, Generic)

emptyCommonSymbolTable :: CommonSymbolTable a
emptyCommonSymbolTable =
  CommonSymbolTable
    { counter = 0
    , table = Map.empty
    , contents = []
    }

commonSymbolTableFromList :: (Ord a) => [a] -> CommonSymbolTable a
commonSymbolTableFromList = foldr go emptyCommonSymbolTable
 where
  go val tbl0 =
    let (_, tbl1) = insertCommonSymbolTable val tbl0
     in tbl1

nextCounter :: CommonSymbolTable a -> (SymbolIndex, CommonSymbolTable a)
nextCounter tbl = (tbl.counter, tbl{counter = tbl.counter + 1})

insertCommonSymbolTable :: (Ord a) => a -> CommonSymbolTable a -> (SymbolIndex, CommonSymbolTable a)
insertCommonSymbolTable val tbl =
  let updateEntry tbl0 Nothing =
        let (sid, newTbl) = nextCounter tbl0
         in ((sid, True, newTbl), Just sid)
      updateEntry newTbl (Just old) =
        ((old, False, newTbl), Just old)

      ((idx, newEntry, tbl1), newTable) = Map.alterF (updateEntry tbl) val tbl.table
   in ( idx
      , tbl1
          { table = newTable
          , contents =
              if newEntry
                then val : tbl1.contents
                else tbl1.contents
          }
      )
