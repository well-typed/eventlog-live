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
import GHC.Eventlog.Live.Data.InfoProv qualified as M
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Profile qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core
import GHC.Eventlog.Live.Otelcol.Processor.Common.ProfilesDictionary (ProfilesDictionary, SymbolIndex)
import GHC.Eventlog.Live.Otelcol.Processor.Common.ProfilesDictionary qualified as PD
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

  profilesDictionary = PD.toProfilesDictionary st

  (profile, st) = flip runState PD.empty $ do
    sampleNameStrId <- PD.getString "__name__"
    sampleTypeStrId <- PD.getString "String"
    sampleAttrId <-
      PD.getAttribute $
        messageWith @OP.KeyValueAndUnit
          [ OP.keyStrindex .~ sampleNameStrId
          , OP.unitStrindex .~ sampleTypeStrId
          , OP.value .~ messageWith [OC.stringValue .~ "process_cpu"]
          ]

    samples <- traverse (asSample sampleAttrId) callstacks
    cpuId <- PD.getString "stack"
    unitId <- PD.getString "samples"
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

asSample :: SymbolIndex -> M.CallStackData -> State ProfilesDictionary OP.Sample
asSample six stackData = do
  locIndices <- traverse toIndex stackData.stack
  s <-
    PD.getStack $
      messageWith
        [ OP.locationIndices .~ locIndices
        ]

  sampleThreadKeyStrId <- PD.getString "thread"
  sampleCapKeyStrId <- PD.getString "capability"
  sampleNumberUnitStrId <- PD.getString "Number"

  threadAttrId <-
    PD.getAttribute $
      messageWith @OP.KeyValueAndUnit
        [ OP.keyStrindex .~ sampleThreadKeyStrId
        , OP.unitStrindex .~ sampleNumberUnitStrId
        , OP.value .~ messageWith [OC.intValue .~ maybe 0 (fromIntegral . (.value)) stackData.threadId]
        ]

  capAttrId <-
    PD.getAttribute $
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
  toIndex :: M.StackItemData -> State ProfilesDictionary SymbolIndex
  toIndex = \case
    M.IpeData infoProv -> getLocationIndexForInfoTable infoProv
    M.UserMessageData message -> getLocationIndexForText message
    M.SourceLocationData srcLoc -> getLocationIndexForSourceLocation srcLoc
    M.CostCentreData costCentre -> getLocationIndexForCostCentre costCentre

getLocationIndexForSourceLocation :: Profiler.SourceLocation -> State ProfilesDictionary SymbolIndex
getLocationIndexForSourceLocation srcLoc = do
  functionNameId <- PD.getString $ Profiler.functionName srcLoc
  fileNameId <- PD.getString $ Profiler.fileName srcLoc
  funcIdx <-
    PD.getFunction $
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

  PD.getLocation $
    messageWith
      [ OP.lines .~ [line]
      , OP.mappingIndex .~ 0 -- 0 means unset
      ]

getLocationIndexForText :: Text -> State ProfilesDictionary SymbolIndex
getLocationIndexForText msg = do
  textId <- PD.getString msg
  funcIdx <-
    PD.getFunction $
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

  PD.getLocation $
    messageWith
      [ OP.lines .~ [line]
      ]

getLocationIndexForInfoTable :: M.InfoProv -> State ProfilesDictionary SymbolIndex
getLocationIndexForInfoTable infoProv = do
  ipNameId <- PD.getString infoProv.ipName
  let label =
        if (infoProv.ipLabel) == ""
          then infoProv.ipModule <> ":" <> infoProv.ipName
          else infoProv.ipModule <> ":" <> infoProv.ipLabel
  infoProvFuncNameId <- PD.getString label
  -- tyDesc <- getText infoProv.infoProvTyDesc
  --
  ipSrcLocId <- PD.getString infoProv.ipSrcLoc
  funcIdx <-
    PD.getFunction $
      messageWith
        [ OP.nameStrindex .~ infoProvFuncNameId
        , OP.systemNameStrindex .~ ipNameId
        , OP.filenameStrindex .~ ipSrcLocId -- 0 means unset
        , OP.startLine .~ 0 -- 0 means unset
        ]

  let line :: OP.Line
      line =
        messageWith
          [ OP.functionIndex .~ funcIdx
          , OP.line .~ 0 -- 0 means unset
          , OP.column .~ 0 -- 0 means unset
          ]

  PD.getLocation $
    messageWith
      [ OP.lines .~ [line]
      , OP.address .~ 0 -- 0 means unset
      ]

getLocationIndexForCostCentre :: M.CostCentre -> State ProfilesDictionary SymbolIndex
getLocationIndexForCostCentre costCentre = do
  let label = costCentre.costCentreModule <> ":" <> costCentre.costCentreLabel
  costCentreFuncNameId <- PD.getString label
  -- tyDesc <- getText infoProv.infoProvTyDesc
  --
  costCentreSrcLocId <- PD.getString costCentre.costCentreSrcLoc
  funcIdx <-
    PD.getFunction $
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

  PD.getLocation $
    messageWith
      [ OP.lines .~ [line]
      , OP.address .~ 0 -- 0 means unset
      ]
