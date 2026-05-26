{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Common.Profiles
Description : Abstraction over ProfilesDictionary for the OTLP protocol.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Common.Profiles (
  toExportProfileServiceRequest,

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
import Data.ProtoLens (Message (..))
import Data.Text (Text)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core (messageWith)
import GHC.Eventlog.Live.Otelcol.Processor.Common.SymbolTable (SymbolIndex, SymbolTable)
import GHC.Eventlog.Live.Otelcol.Processor.Common.SymbolTable qualified as ST
import GHC.Generics (Generic)
import Lens.Family2 (Lens', (&), (.~), (^.))
import Lens.Family2.Unchecked (lens)
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService qualified as OPS
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService_Fields qualified as OPS
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP

toExportProfileServiceRequest :: OP.ProfilesData -> OPS.ExportProfilesServiceRequest
toExportProfileServiceRequest profilesData =
  messageWith
    [ OPS.resourceProfiles .~ profilesData ^. OPS.resourceProfiles
    , OPS.dictionary .~ profilesData ^. OPS.dictionary
    ]

data ProfileDictionary = ProfileDictionary
  { locationTable :: SymbolTable OP.Location
  -- ^ Common 'OP.Location' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , functionTable :: SymbolTable OP.Function
  -- ^ Common 'OP.Function' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , stringTable :: SymbolTable Text
  -- ^ Common string table, the first entry must be "" per the protobuf
  --   documentation.
  --
  --   @
  --    // A common table for strings referenced by various messages.
  --    // string_table[0] must always be "".
  --    repeated string string_table = 5;
  --   @
  , mappingTable :: SymbolTable OP.Mapping
  -- ^ Common 'OP.Mapping' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , linkTable :: SymbolTable OP.Link
  -- ^ Common 'OP.Link' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , attributeTable :: SymbolTable OP.KeyValueAndUnit
  -- ^ Common 'OP.KeyValueAndUnit' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  , stackTable :: SymbolTable OP.Stack
  -- ^ Common 'OP.Stack' table, first entry is the 'defMessage'.
  --   This holds for OTLP 1.9.0.
  }
  deriving (Show, Ord, Eq, Generic)

toProfilesDictionary :: ProfileDictionary -> OP.ProfilesDictionary
toProfilesDictionary st =
  messageWith
    [ OP.locationTable .~ locationTableList st
    , OP.functionTable .~ functionTableList st
    , OP.stringTable .~ stringTableList st
    , OP.mappingTable .~ mappingTableList st
    , OP.linkTable .~ linkTableList st
    , OP.attributeTable .~ attributeTableList st
    , OP.stackTable .~ stackTableList st
    ]

emptyProfileDictionary :: ProfileDictionary
emptyProfileDictionary =
  ProfileDictionary
    { locationTable = ST.fromList [defMessage]
    , functionTable = ST.fromList [defMessage]
    , stringTable = ST.fromList [""]
    , mappingTable = ST.fromList [defMessage]
    , linkTable = ST.fromList [defMessage]
    , attributeTable = ST.fromList [defMessage]
    , stackTable = ST.fromList [defMessage]
    }

locationTableList :: ProfileDictionary -> [OP.Location]
locationTableList st = ST.toList st.locationTable

functionTableList :: ProfileDictionary -> [OP.Function]
functionTableList st = ST.toList st.functionTable

stringTableList :: ProfileDictionary -> [Text]
stringTableList st = ST.toList st.stringTable

mappingTableList :: ProfileDictionary -> [OP.Mapping]
mappingTableList st = ST.toList st.mappingTable

linkTableList :: ProfileDictionary -> [OP.Link]
linkTableList st = ST.toList st.linkTable

attributeTableList :: ProfileDictionary -> [OP.KeyValueAndUnit]
attributeTableList st = ST.toList st.attributeTable

stackTableList :: ProfileDictionary -> [OP.Stack]
stackTableList st = ST.toList st.stackTable

getSymbolIndexFor :: (Ord a, Monad m) => Lens' ProfileDictionary (SymbolTable a) -> a -> StateT ProfileDictionary m SymbolIndex
getSymbolIndexFor accessor a = do
  tbl <- State.gets (^. accessor)
  let (idx, tbl1) = ST.insert a tbl
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
