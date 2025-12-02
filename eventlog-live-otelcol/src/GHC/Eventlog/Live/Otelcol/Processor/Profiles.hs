{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Profiles.Dictionary
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Profiles (
  processCallStackData,
)
where

import Control.Monad.Trans.State.Strict (State, runState)
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Profile qualified as M
import GHC.Eventlog.Live.Otelcol.Processor.Profiles.Dictionary (ProfileDictionary, SymbolIndex)
import GHC.Eventlog.Live.Otelcol.Processor.Profiles.Dictionary qualified as ProfDictionary
import GHC.Stack.Profiler.Core.SourceLocation qualified as Profiler
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR

processCallStackData :: OR.Resource -> OC.InstrumentationScope -> [M.CallStackData] -> (OP.ResourceProfiles, OP.ProfilesDictionary)
processCallStackData resource instrumentationScope callstacks =
  let (profile, st) = flip runState ProfDictionary.emptyProfileDictionary $ do
        sampleNameStrId <- ProfDictionary.getString "__name__"
        sampleTypeStrId <- ProfDictionary.getString "String"
        sampleAttrId <-
          ProfDictionary.getAttribute $
            messageWith @OP.KeyValueAndUnit
              [ OP.keyStrindex .~ sampleNameStrId
              , OP.unitStrindex .~ sampleTypeStrId
              , OP.value .~ messageWith [OC.stringValue .~ "process_cpu"]
              ]

        samples <- traverse (asSample sampleAttrId) callstacks
        cpuId <- ProfDictionary.getString "stack"
        unitId <- ProfDictionary.getString "samples"
        let sampleType :: OP.ValueType
            sampleType =
              messageWith
                [ OP.typeStrindex .~ cpuId
                , OP.unitStrindex .~ unitId
                ]

        pure $
          messageWith
            [ OP.samples .~ samples
            , OP.sampleType .~ sampleType
            ]

      scopedProfiles =
        messageWith
          [ OP.profiles .~ [profile]
          , OP.scope .~ instrumentationScope
          ]

      resourceProfiles =
        messageWith
          [ OP.scopeProfiles .~ [scopedProfiles]
          , OP.resource .~ resource
          ]

      profilesDictionary = ProfDictionary.toProfilesDictionary st
   in (resourceProfiles, profilesDictionary)

asSample :: SymbolIndex -> M.CallStackData -> State ProfileDictionary OP.Sample
asSample six stackData = do
  locIndices <- traverse toIndex stackData.stack
  s <-
    ProfDictionary.getStack $
      messageWith
        [ OP.locationIndices .~ locIndices
        ]

  sampleThreadKeyStrId <- ProfDictionary.getString "thread"
  sampleThreadUnitStrId <- ProfDictionary.getString "Number"

  threadAttrId <-
    ProfDictionary.getAttribute $
      messageWith @OP.KeyValueAndUnit
        [ OP.keyStrindex .~ sampleThreadKeyStrId
        , OP.unitStrindex .~ sampleThreadUnitStrId
        , OP.value .~ messageWith [OC.intValue .~ fromIntegral stackData.threadId]
        ]

  pure $
    messageWith
      [ OP.values .~ [1]
      , OP.stackIndex .~ s
      , OP.attributeIndices .~ [six, threadAttrId]
      ]
 where
  toIndex :: M.StackItemData -> State ProfileDictionary SymbolIndex
  toIndex = \case
    M.IpeData infoTable -> getLocationIndexForInfoTable infoTable
    M.UserMessageData message -> getLocationIndexForText message
    M.SourceLocationData srcLoc -> getLocationIndexForSourceLocation srcLoc

getLocationIndexForSourceLocation :: Profiler.SourceLocation -> State ProfileDictionary SymbolIndex
getLocationIndexForSourceLocation srcLoc = do
  functionNameId <- ProfDictionary.getString $ Profiler.functionName srcLoc
  fileNameId <- ProfDictionary.getString $ Profiler.fileName srcLoc
  funcIdx <-
    ProfDictionary.getFunction $
      messageWith
        [ OP.nameStrindex .~ functionNameId
        , OP.systemNameStrindex .~ 0 -- 0 means unset
        , OP.filenameStrindex .~ fileNameId
        , OP.startLine .~ fromIntegral (Profiler.line srcLoc) -- TODO: better casts
        ]

  let line :: OP.Line
      line =
        messageWith
          [ OP.functionIndex .~ funcIdx
          , OP.line .~ fromIntegral (Profiler.line srcLoc)
          , OP.column .~ fromIntegral (Profiler.column srcLoc)
          ]

  ProfDictionary.getLocation $
    messageWith
      [ OP.lines .~ [line]
      , OP.mappingIndex .~ 0 -- 0 means unset
      ]

getLocationIndexForText :: Text -> State ProfileDictionary SymbolIndex
getLocationIndexForText msg = do
  textId <- ProfDictionary.getString msg
  funcIdx <-
    ProfDictionary.getFunction $
      messageWith
        [ OP.nameStrindex .~ textId
        , OP.systemNameStrindex .~ 0 -- 0 means unset
        , OP.filenameStrindex .~ 0 -- 0 means unset
        , OP.startLine .~ 0 -- 0 means unset
        ]

  let line :: OP.Line
      line =
        messageWith
          [ OP.functionIndex .~ funcIdx
          , OP.line .~ 0 -- 0 means unset
          , OP.column .~ 0 -- 0 means unset
          ]

  ProfDictionary.getLocation $
    messageWith
      [ OP.lines .~ [line]
      ]

getLocationIndexForInfoTable :: M.InfoTable -> State ProfileDictionary SymbolIndex
getLocationIndexForInfoTable infoTable = do
  infoTableNameId <- ProfDictionary.getString infoTable.infoTableName
  let label =
        if (infoTable.infoTableLabel) == ""
          then infoTable.infoTableModule <> ":" <> infoTable.infoTableName
          else infoTable.infoTableModule <> ":" <> infoTable.infoTableLabel
  infoTableFuncNameId <- ProfDictionary.getString label
  -- tyDesc <- getText infoTable.infoTableTyDesc
  --
  infoTableSrcLocId <- ProfDictionary.getString infoTable.infoTableSrcLoc
  funcIdx <-
    ProfDictionary.getFunction $
      messageWith
        [ OP.nameStrindex .~ infoTableFuncNameId
        , OP.systemNameStrindex .~ infoTableNameId
        , OP.filenameStrindex .~ infoTableSrcLocId -- 0 means unset
        , OP.startLine .~ 0 -- 0 means unset
        ]

  let line :: OP.Line
      line =
        messageWith
          [ OP.functionIndex .~ funcIdx
          , OP.line .~ 0 -- 0 means unset
          , OP.column .~ 0 -- 0 means unset
          ]

  ProfDictionary.getLocation $
    messageWith
      [ OP.lines .~ [line]
      , OP.address .~ 0 -- 0 means unset
      ]

--------------------------------------------------------------------------------
-- DSL for writing messages

-- | Construct a message with a list of modifications applied.
messageWith :: (Message msg) => [msg -> msg] -> msg
messageWith = foldr ($) defMessage
