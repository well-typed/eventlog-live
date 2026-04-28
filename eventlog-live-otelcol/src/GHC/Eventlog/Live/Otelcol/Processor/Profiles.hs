{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Profiles
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Profiles (
  processProfileEvents,
  processCallStackData,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State.Strict (State, runState)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (ProcessT, asParts, mapping, (~>))
import Data.Text (Text)
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Profile qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core
import GHC.Eventlog.Live.Otelcol.Processor.Common.Profiles (ProfileDictionary, SymbolIndex)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Profiles qualified as ProfDictionary
import GHC.RTS.Events (Event (..))
import GHC.Stack.Profiler.Core.SourceLocation qualified as Profiler
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP
import Proto.Opentelemetry.Proto.Resource.V1.Resource (Resource)

--------------------------------------------------------------------------------
-- processProfileEvents
--------------------------------------------------------------------------------

processProfileEvents ::
  (MonadIO m) =>
  Logger m ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processProfileEvents verbosity config =
  M.fanoutTick
    [ processStackProfSample verbosity config
    , processCostCentreProfSample verbosity config
    ]

--------------------------------------------------------------------------------
-- StackProfSample

processStackProfSample ::
  (MonadIO m) =>
  Logger m ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processStackProfSample logger config =
  runIf (C.processorEnabled (.profiles) (.stackSample) config) $
    M.liftTick
      ( M.processStackProfSampleData logger
          ~> mapping M.stackProfSamples
          ~> asParts
          ~> mapping (D.singleton . (.value))
      )
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.stackSample) config)

processCostCentreProfSample ::
  (MonadIO m) =>
  Logger m ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processCostCentreProfSample logger config =
  runIf (C.processorEnabled (.profiles) (.costCentreSample) config) $
    M.liftTick
      ( M.processCostCentreProfSampleData logger
          ~> mapping M.stackProfSamples
          ~> asParts
          ~> mapping (D.singleton . (.value))
      )
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.costCentreSample) config)

processCallStackData :: Resource -> OC.InstrumentationScope -> [M.CallStackData] -> (OP.ResourceProfiles, OP.ProfilesDictionary)
processCallStackData resource instrumentationScope callstacks = (resourceProfiles, profilesDictionary)
 where
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

  (profile, st) = flip runState ProfDictionary.emptyProfileDictionary $ do
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

asSample :: SymbolIndex -> M.CallStackData -> State ProfileDictionary OP.Sample
asSample six stackData = do
  locIndices <- traverse toIndex stackData.stack
  s <-
    ProfDictionary.getStack $
      messageWith
        [ OP.locationIndices .~ locIndices
        ]

  sampleThreadKeyStrId <- ProfDictionary.getString "thread"
  sampleCapKeyStrId <- ProfDictionary.getString "capability"
  sampleNumberUnitStrId <- ProfDictionary.getString "Number"

  threadAttrId <-
    ProfDictionary.getAttribute $
      messageWith @OP.KeyValueAndUnit
        [ OP.keyStrindex .~ sampleThreadKeyStrId
        , OP.unitStrindex .~ sampleNumberUnitStrId
        , OP.value .~ messageWith [OC.intValue .~ maybe 0 (fromIntegral . (.value)) stackData.threadId]
        ]

  capAttrId <-
    ProfDictionary.getAttribute $
      messageWith @OP.KeyValueAndUnit
        [ OP.keyStrindex .~ sampleCapKeyStrId
        , OP.unitStrindex .~ sampleNumberUnitStrId
        , OP.value .~ messageWith [OC.intValue .~ fromIntegral stackData.capabilityId.value]
        ]

  pure $
    messageWith
      [ OP.values .~ [1]
      , OP.stackIndex .~ s
      , OP.attributeIndices
          .~ [ six
             , threadAttrId
             , capAttrId
             ]
      ]
 where
  toIndex :: M.StackItemData -> State ProfileDictionary SymbolIndex
  toIndex = \case
    M.IpeData infoTable -> getLocationIndexForInfoTable infoTable
    M.UserMessageData message -> getLocationIndexForText message
    M.SourceLocationData srcLoc -> getLocationIndexForSourceLocation srcLoc
    M.CostCentreData costCentre -> getLocationIndexForCostCentre costCentre

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

getLocationIndexForCostCentre :: M.CostCentre -> State ProfileDictionary SymbolIndex
getLocationIndexForCostCentre costCentre = do
  let label = costCentre.costCentreModule <> ":" <> costCentre.costCentreLabel
  costCentreFuncNameId <- ProfDictionary.getString label
  -- tyDesc <- getText infoTable.infoTableTyDesc
  --
  costCentreSrcLocId <- ProfDictionary.getString costCentre.costCentreSrcLoc
  funcIdx <-
    ProfDictionary.getFunction $
      messageWith
        [ OP.nameStrindex .~ costCentreFuncNameId
        , OP.systemNameStrindex .~ costCentreFuncNameId
        , OP.filenameStrindex .~ costCentreSrcLocId -- 0 means unset
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
