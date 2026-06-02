{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Common.Profiles
Description : Abstraction over ProfilesDictionary for the OTLP protocol.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Common.ProfilesDictionary (
  -- * Dictionary for deduplication logic of common values
  ProfilesDictionary,
  empty,

  -- * Retrieve the 'SymbolIndex' for various 'OP.ProfilesData' fields
  SymbolIndex,
  getLocation,
  getFunction,
  getString,
  getMapping,
  getLink,
  getAttribute,
  getStack,

  -- * Convert data to the formats used by @hs-opentelemetry-otlp@.
  toExportProfileServiceRequest,
  toProfilesDictionary,
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
import Lens.Family2 (Lens', (.~), (^.))
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

data ProfilesDictionary = ProfilesDictionary
  { locationTable :: SymbolTable OP.Location
  {- ^ Common 'OP.Location' table, first entry is the 'defMessage'.
  This holds for OTLP 1.9.0.
  -}
  , functionTable :: SymbolTable OP.Function
  {- ^ Common 'OP.Function' table, first entry is the 'defMessage'.
  This holds for OTLP 1.9.0.
  -}
  , stringTable :: SymbolTable Text
  {- ^ Common string table, the first entry must be "" per the protobuf
  documentation.

  @
   // A common table for strings referenced by various messages.
   // string_table[0] must always be "".
   repeated string string_table = 5;
  @
  -}
  , mappingTable :: SymbolTable OP.Mapping
  {- ^ Common 'OP.Mapping' table, first entry is the 'defMessage'.
  This holds for OTLP 1.9.0.
  -}
  , linkTable :: SymbolTable OP.Link
  {- ^ Common 'OP.Link' table, first entry is the 'defMessage'.
  This holds for OTLP 1.9.0.
  -}
  , attributeTable :: SymbolTable OP.KeyValueAndUnit
  {- ^ Common 'OP.KeyValueAndUnit' table, first entry is the 'defMessage'.
  This holds for OTLP 1.9.0.
  -}
  , stackTable :: SymbolTable OP.Stack
  {- ^ Common 'OP.Stack' table, first entry is the 'defMessage'.
  This holds for OTLP 1.9.0.
  -}
  }
  deriving (Show, Ord, Eq, Generic)

toProfilesDictionary :: ProfilesDictionary -> OP.ProfilesDictionary
toProfilesDictionary st =
  messageWith
    [ OP.locationTable .~ locations st
    , OP.functionTable .~ functions st
    , OP.stringTable .~ strings st
    , OP.mappingTable .~ mappings st
    , OP.linkTable .~ links st
    , OP.attributeTable .~ attributes st
    , OP.stackTable .~ stacks st
    ]

empty :: ProfilesDictionary
empty =
  ProfilesDictionary
    { locationTable = ST.fromList [defMessage]
    , functionTable = ST.fromList [defMessage]
    , stringTable = ST.fromList [""]
    , mappingTable = ST.fromList [defMessage]
    , linkTable = ST.fromList [defMessage]
    , attributeTable = ST.fromList [defMessage]
    , stackTable = ST.fromList [defMessage]
    }

locations :: ProfilesDictionary -> [OP.Location]
locations pd = ST.toList pd.locationTable

functions :: ProfilesDictionary -> [OP.Function]
functions pd = ST.toList pd.functionTable

strings :: ProfilesDictionary -> [Text]
strings pd = ST.toList pd.stringTable

mappings :: ProfilesDictionary -> [OP.Mapping]
mappings pd = ST.toList pd.mappingTable

links :: ProfilesDictionary -> [OP.Link]
links pd = ST.toList pd.linkTable

attributes :: ProfilesDictionary -> [OP.KeyValueAndUnit]
attributes pd = ST.toList pd.attributeTable

stacks :: ProfilesDictionary -> [OP.Stack]
stacks pd = ST.toList pd.stackTable

getSymbolIndexFor :: (Ord a, Monad m) => Lens' ProfilesDictionary (SymbolTable a) -> a -> StateT ProfilesDictionary m SymbolIndex
getSymbolIndexFor accessor a = do
  st <- State.gets (^. accessor)
  let (si, pd') = ST.insert a st
  State.modify' (accessor .~ pd')
  pure si

getLocation :: (Monad m) => OP.Location -> StateT ProfilesDictionary m SymbolIndex
getLocation = getSymbolIndexFor (lens (.locationTable) (\pd st -> pd{locationTable = st}))

getFunction :: (Monad m) => OP.Function -> StateT ProfilesDictionary m SymbolIndex
getFunction = getSymbolIndexFor (lens (.functionTable) (\pd st -> pd{functionTable = st}))

getString :: (Monad m) => Text -> StateT ProfilesDictionary m SymbolIndex
getString = getSymbolIndexFor (lens (.stringTable) (\pd st -> pd{stringTable = st}))

getMapping :: (Monad m) => OP.Mapping -> StateT ProfilesDictionary m SymbolIndex
getMapping = getSymbolIndexFor (lens (.mappingTable) (\pd st -> pd{mappingTable = st}))

getLink :: (Monad m) => OP.Link -> StateT ProfilesDictionary m SymbolIndex
getLink = getSymbolIndexFor (lens (.linkTable) (\pd st -> pd{linkTable = st}))

getAttribute :: (Monad m) => OP.KeyValueAndUnit -> StateT ProfilesDictionary m SymbolIndex
getAttribute = getSymbolIndexFor (lens (.attributeTable) (\pd st -> pd{attributeTable = st}))

getStack :: (Monad m) => OP.Stack -> StateT ProfilesDictionary m SymbolIndex
getStack = getSymbolIndexFor (lens (.stackTable) (\pd st -> pd{stackTable = st}))
