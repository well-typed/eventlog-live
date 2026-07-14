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
import Data.Machine (ProcessT, mapping, (~>))
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Eventlog.Live.Data.Capability (CapNo (..))
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
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP
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
  DB.Table CC.CostCentreId CC.CostCentre ->
  DB.Table IP.InfoProvId IP.InfoProv ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processProfileEvents verbosity costCentreTable infoProvTable config =
  M.fanoutTick
    [ processProfSampleCostCentre verbosity costCentreTable config
    , processGhcStackProfilerData verbosity infoProvTable config
    ]

--------------------------------------------------------------------------------
-- StackProfSample

processGhcStackProfilerData ::
  (MonadIO m) =>
  Logger m ->
  DB.Table IP.InfoProvId IP.InfoProv ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processGhcStackProfilerData logger infoProvTable config =
  runIf (C.processorEnabled (.profiles) (.stackSample) config) $
    M.liftTick
      ( M.processGhcStackProfilerData logger infoProvTable
          ~> mapping D.singleton
      )
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.stackSample) config)

processProfSampleCostCentre ::
  (MonadIO m) =>
  Logger m ->
  DB.Table CC.CostCentreId CC.CostCentre ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList M.CallStackData))
processProfSampleCostCentre logger costCentreTable config =
  runIf (C.processorEnabled (.profiles) (.costCentreSample) config) $
    M.liftTick
      ( M.processProfSampleCostCentre logger costCentreTable
          ~> mapping D.singleton
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
        , OP.value .~ messageWith [OC.intValue .~ fromIntegral stackData.capNo.value]
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
    M.UserMessageData message mSrcLoc -> getLocationIndexForText message mSrcLoc
    M.CostCentreData costCentre -> getLocationIndexForCostCentre costCentre

getLocationIndexForText :: Text -> Maybe Profiler.SourceLocation -> State ProfilesDictionary SymbolIndex
getLocationIndexForText msg mSrcLoc = do
  let srcLoc = Maybe.fromMaybe unhelpfulSrcLoc mSrcLoc
  fileNameId <- case mSrcLoc of
    Nothing -> pure 0 -- 0 means unset
    Just loc -> PD.getString $ Profiler.fileName loc
  textId <- PD.getString msg
  funcIdx <-
    PD.getFunction $
      messageWith
        [ OP.nameStrindex .~ textId
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
      ]

unhelpfulSrcLoc :: Profiler.SourceLocation
unhelpfulSrcLoc =
  Profiler.MkSourceLocation
    { line = 0 -- 0 means unset
    , column = 0 -- 0 means unset
    , fileName = ""
    }

getLocationIndexForInfoTable ::
  IP.InfoProv ->
  State ProfilesDictionary SymbolIndex
getLocationIndexForInfoTable infoProv = do
  ipNameId <- PD.getString infoProv.ipName
  let label =
        if (infoProv.ipLabel) == ""
          then infoProv.ipModule <> ":" <> infoProv.ipName
          else infoProv.ipModule <> ":" <> infoProv.ipLabel
  infoProvFuncNameId <- PD.getString label
  -- tyDesc <- getText infoProv.infoProvTyDesc
  --
  ipSrcLocId <- PD.getString (T.pack (show infoProv.ipSrcLoc))
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

getLocationIndexForCostCentre ::
  CC.CostCentre ->
  State ProfilesDictionary SymbolIndex
getLocationIndexForCostCentre costCentre = do
  let label = costCentre.ccModule <> ":" <> costCentre.ccLabel
  costCentreFuncNameId <- PD.getString label
  -- tyDesc <- getText infoProv.infoProvTyDesc
  --
  ccSrcLocId <- PD.getString (T.pack (show costCentre.ccSrcLoc))
  funcIdx <-
    PD.getFunction $
      messageWith
        [ OP.nameStrindex .~ costCentreFuncNameId
        , OP.systemNameStrindex .~ costCentreFuncNameId
        , OP.filenameStrindex .~ ccSrcLocId -- 0 means unset
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
