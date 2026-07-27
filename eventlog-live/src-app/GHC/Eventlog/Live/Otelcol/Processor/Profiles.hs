{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Profiles
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Profiles (
  -- * Profile processing
  Sample (..),
  Stack (..),
  processProfileEvents,
  toProfiles,

  -- * Conversion to OTLP profiles
  toExportProfileServiceRequest,
  toProfilesData,
  toResourceProfiles,
  toScopeProfiles,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State.Strict (State, StateT (..))
import Data.Bifunctor (Bifunctor (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, mapping, (~>))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Eventlog.Live.Data.Attribute (HasAttrs (..), (~=))
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
import GHC.IsList (IsList (..))
import GHC.RTS.Events (Event (..), Timestamp)
import GHC.Records (HasField)
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP
import IpeDB.Types.SrcLoc (Point (..), SrcLoc (..))
import Lens.Family2 ((.~), (^.))
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService qualified as OPS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OPS
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR

--------------------------------------------------------------------------------
-- Samples
--------------------------------------------------------------------------------

data Sample a = Sample
  { name :: !Text
  , stack :: !a
  }
  deriving stock (Show, Functor)

data Stack
  = CostCentreStack !M.CostCentreStack
  | CallStack !M.CallStack
  deriving (Show)

processProfileEvents ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  DB.Table CC.CostCentreId CC.CostCentre ->
  DB.Table IP.InfoProvId IP.InfoProv ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList (Sample Stack)))
processProfileEvents logger ccdb ipedb config =
  M.fanoutTick
    [ processProfSampleCostCentre logger ccdb config
        ~> mapping (fmap (fmap (fmap CostCentreStack)))
    , processGhcStackProfiler logger ipedb config
        ~> mapping (fmap (fmap (fmap CallStack)))
    ]

--------------------------------------------------------------------------------
-- Processor for `ghc-stack-profiler` call-stack samples
--------------------------------------------------------------------------------

processGhcStackProfiler ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  DB.Table IP.InfoProvId IP.InfoProv ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList (Sample M.CallStack)))
processGhcStackProfiler logger ipedb config =
  runIf (C.processorEnabled (.profiles) (.callStackProfile) config) $
    M.liftTick
      ( M.processGhcStackProfilerData logger ipedb
          ~> mapping (\stack -> D.singleton Sample{..})
      )
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.callStackProfile) config)
 where
  !name = C.processorName (.profiles) (.callStackProfile) config

--------------------------------------------------------------------------------
-- Processor for cost-centre stack samples
--------------------------------------------------------------------------------

processProfSampleCostCentre ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  DB.Table CC.CostCentreId CC.CostCentre ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList (Sample M.CostCentreStack)))
processProfSampleCostCentre logger ccdb config =
  runIf (C.processorEnabled (.profiles) (.costCentreStackProfile) config) $
    M.liftTick
      ( M.processProfSampleCostCentreData logger ccdb
          ~> mapping (\stack -> D.singleton Sample{..})
      )
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.costCentreStackProfile) config)
 where
  !name = C.processorName (.profiles) (.costCentreStackProfile) config

--------------------------------------------------------------------------------
-- Translation to OTLP profiles
--------------------------------------------------------------------------------

toExportProfileServiceRequest :: OP.ProfilesData -> OPS.ExportProfilesServiceRequest
toExportProfileServiceRequest profilesData =
  messageWith
    [ OPS.resourceProfiles .~ profilesData ^. OPS.resourceProfiles
    , OPS.dictionary .~ profilesData ^. OPS.dictionary
    ]

toProfilesData :: [OP.ResourceProfiles] -> OP.ProfilesDictionary -> Maybe OP.ProfilesData
toProfilesData resourceProfiles dictionary =
  ifNonEmpty resourceProfiles $
    messageWith [OP.resourceProfiles .~ resourceProfiles, OP.dictionary .~ dictionary]

toResourceProfiles :: OR.Resource -> [OP.ScopeProfiles] -> Maybe OP.ResourceProfiles
toResourceProfiles resource scopeProfiles =
  ifNonEmpty scopeProfiles $
    messageWith [OP.resource .~ resource, OP.scopeProfiles .~ scopeProfiles]

toScopeProfiles :: OC.InstrumentationScope -> [OP.Profile] -> Maybe OP.ScopeProfiles
toScopeProfiles instrumentationScope profiles =
  ifNonEmpty profiles $
    messageWith [OP.scope .~ instrumentationScope, OP.profiles .~ profiles]

toProfiles :: [Sample Stack] -> Maybe ([OP.Profile], OP.ProfilesDictionary)
toProfiles samples = ifNonEmpty profiles profilesData
 where
  (costCentreStacks, callStacks) = partitionSamples samples

  profilesData@(profiles, _) =
    second PD.toProfilesDictionary . runIdentity . flip runStateT PD.empty $ do
      -- Convert any cost-centre profiles.
      maybeCostCentreProfile <-
        sequence . ifNonEmpty costCentreStacks $
          getProfile costCentreStacks
      -- Convert any call-stack profiles.
      maybeCallStackProfiles <-
        sequence . ifNonEmpty callStacks $
          getProfile callStacks
      pure $ catMaybes [maybeCostCentreProfile, maybeCallStackProfiles]

partitionSamples :: [Sample Stack] -> ([Sample M.CostCentreStack], [Sample M.CallStack])
partitionSamples = go ([], [])
 where
  go :: ([Sample M.CostCentreStack], [Sample M.CallStack]) -> [Sample Stack] -> ([Sample M.CostCentreStack], [Sample M.CallStack])
  go (costCentreStackSamplesRev, callStackSamplesRev) = \case
    [] -> (reverse costCentreStackSamplesRev, reverse callStackSamplesRev)
    (Sample{stack = CostCentreStack costCentreStack, ..} : rest) -> go (Sample{stack = costCentreStack, ..} : costCentreStackSamplesRev, callStackSamplesRev) rest
    (Sample{stack = CallStack callStack, ..} : rest) -> go (costCentreStackSamplesRev, Sample{stack = callStack, ..} : callStackSamplesRev) rest

--------------------------------------------------------------------------------
-- Translating profiles to OTLP profiles

{-# SPECIALIZE getProfile ::
  [Sample M.CallStack] -> State ProfilesDictionary OP.Profile
  #-}
{-# SPECIALIZE getProfile ::
  [Sample M.CostCentreStack] -> State ProfilesDictionary OP.Profile
  #-}
getProfile ::
  forall m a.
  (Monad m, ToSample a) =>
  [Sample a] -> StateT ProfilesDictionary m OP.Profile
getProfile xs = do
  samples <- traverse toSample xs
  typeStrindex <- PD.getText (getSampleType (Proxy @a))
  unitStrindex <- PD.getText (getSampleUnit (Proxy @a))
  let sampleType :: OP.ValueType
      sampleType =
        messageWith
          [ OP.typeStrindex .~ typeStrindex
          , OP.unitStrindex .~ unitStrindex
          ]
  let profile :: OP.Profile
      profile =
        messageWith
          [ OP.samples .~ samples
          , OP.sampleType .~ sampleType
          ]
  pure profile

--------------------------------------------------------------------------------
-- Translating samples to OTLP samples

type IsSample a = (IsStack a, HasAttrs a, HasField "maybeTimeUnixNano" a (Maybe Timestamp))

class (IsSample a, ToLocation (StackFrame a)) => ToSample a where
  getSampleType :: Proxy a -> Text
  getSampleUnit :: Proxy a -> Text

  getSampleValue :: a -> Int64
  getSampleValue _x = 1
  {-# INLINE getSampleValue #-}

{-# SPECIALIZE toSample ::
  Sample M.CallStack -> State ProfilesDictionary OP.Sample
  #-}
{-# SPECIALIZE toSample ::
  Sample M.CostCentreStack -> State ProfilesDictionary OP.Sample
  #-}
toSample ::
  forall m a.
  (Monad m, ToSample a) =>
  Sample a -> StateT ProfilesDictionary m OP.Sample
toSample x = do
  stackIndex <- PD.getStack =<< toStack x.stack
  let attrs = "__name__" ~= x.name : toList (getAttrs x.stack)
  attributeIndices <- catMaybes <$> traverse PD.getAttr attrs
  let sample :: OP.Sample
      sample =
        messageWith
          [ OP.values .~ [getSampleValue x.stack]
          , OP.stackIndex .~ stackIndex
          , OP.attributeIndices .~ attributeIndices
          , OP.timestampsUnixNano .~? sequence [x.stack.maybeTimeUnixNano]
          ]
  pure sample

instance ToSample M.CallStack where
  getSampleType :: Proxy M.CallStack -> Text
  getSampleType _proxy = "cpu"
  {-# INLINE getSampleType #-}

  getSampleUnit :: Proxy M.CallStack -> Text
  getSampleUnit _proxy = "samples"
  {-# INLINE getSampleUnit #-}

instance ToSample M.CostCentreStack where
  getSampleType :: Proxy a -> Text
  getSampleType _proxy = "cpu"
  {-# INLINE getSampleType #-}

  getSampleUnit :: Proxy a -> Text
  getSampleUnit _proxy = "samples"
  {-# INLINE getSampleUnit #-}

--------------------------------------------------------------------------------
-- Translating stacks to OTLP stacks

class IsStack a where
  type StackFrame a
  getStackFrames :: a -> Vector (StackFrame a)

instance IsStack M.CallStack where
  type StackFrame M.CallStack = M.CallStackFrame
  getStackFrames :: M.CallStack -> Vector (StackFrame M.CallStack)
  getStackFrames = (.callStack)
  {-# INLINE getStackFrames #-}

instance IsStack M.CostCentreStack where
  type StackFrame M.CostCentreStack = M.CostCentreStackFrame
  getStackFrames :: M.CostCentreStack -> Vector (StackFrame M.CostCentreStack)
  getStackFrames = (.costCentreStack)
  {-# INLINE getStackFrames #-}

{-# SPECIALIZE toStack ::
  M.CallStack -> State ProfilesDictionary OP.Stack
  #-}
{-# SPECIALIZE toStack ::
  M.CostCentreStack -> State ProfilesDictionary OP.Stack
  #-}
toStack ::
  forall m a.
  (Monad m, IsStack a, ToLocation (StackFrame a)) =>
  a -> StateT ProfilesDictionary m OP.Stack
toStack x = do
  locationIndices <- traverse toLocation (getStackFrames x)
  let stack :: OP.Stack
      stack =
        messageWith
          [ OP.vec'locationIndices .~ V.convert locationIndices
          ]
  pure stack

--------------------------------------------------------------------------------
-- Translating stack frames to OTLP Locations

class ToLocation a where
  toLocation ::
    (Monad m) =>
    a -> StateT ProfilesDictionary m SymbolIndex

instance ToLocation M.CallStackFrame where
  toLocation ::
    (Monad m) =>
    M.CallStackFrame -> StateT ProfilesDictionary m SymbolIndex
  toLocation = \case
    M.CallStackFrame _infoProvId (Just infoProv)
      -- If there's a non-empty ipLabel, use it.
      | not (T.null infoProv.ipLabel) ->
          toLocation (infoProv.ipModule <> ":" <> infoProv.ipLabel, infoProv.ipSrcLoc)
      -- If there's a non-empty ipName, use it.
      | not (T.null infoProv.ipName) ->
          toLocation (infoProv.ipModule <> ":" <> infoProv.ipName, infoProv.ipSrcLoc)
    -- Otherwise, there's no helpful location information.
    M.CallStackFrame infoProvId maybeInfoProv ->
      let name = T.pack (show infoProvId)
          srcLoc = maybe UnhelpfulSrcLoc (.ipSrcLoc) maybeInfoProv
       in toLocation (name, srcLoc)
    M.CallStackMessage name srcLoc -> toLocation (name, srcLoc)
  {-# INLINE toLocation #-}

instance ToLocation M.CostCentreStackFrame where
  toLocation ::
    (Monad m) =>
    M.CostCentreStackFrame -> StateT ProfilesDictionary m SymbolIndex
  toLocation = \case
    M.CostCentreStackFrame _costCentreId (Just costCentre)
      -- If there's a non-empty ccLabel, use it.
      | not (T.null costCentre.ccLabel) ->
          toLocation (costCentre.ccModule <> ":" <> costCentre.ccLabel, costCentre.ccSrcLoc)
    -- Otherwise, there's no helpful location information.
    M.CostCentreStackFrame costCentreId maybeCostCentre ->
      let name = T.pack (show costCentreId)
          srcLoc = maybe UnhelpfulSrcLoc (.ccSrcLoc) maybeCostCentre
       in toLocation (name, srcLoc)
  {-# INLINE toLocation #-}

instance ToLocation (Text, SrcLoc) where
  toLocation ::
    (Monad m) =>
    (Text, SrcLoc) -> StateT ProfilesDictionary m SymbolIndex
  toLocation (name, srcLoc) = do
    -- Encode the filename.
    filenameStrindex <-
      if null srcLoc.srcFilePath
        then pure 0
        else PD.getString srcLoc.srcFilePath

    -- Encode the function name.
    nameStrindex <- PD.getText name

    -- Encode the start point.
    let !maybeStart = (.start) <$> srcLoc.srcRange
    let !maybeStartLine = fromIntegral @Word32 @Int64 . (.line) <$> maybeStart
    let !maybeStartColumn = fromIntegral @Word32 @Int64 . (.column) <$> maybeStart

    -- Encode the function metadata.
    let function :: OP.Function
        function =
          messageWith
            [ OP.nameStrindex .~ nameStrindex
            , OP.filenameStrindex .~ filenameStrindex
            , OP.startLine .~? maybeStartLine
            ]
    functionIndex <- PD.getFunction function

    -- Encode the location metadata.
    let line :: OP.Line
        line =
          messageWith
            [ OP.functionIndex .~ functionIndex
            , OP.line .~? maybeStartLine
            , OP.column .~? maybeStartColumn
            ]
    let location :: OP.Location
        location =
          messageWith
            [ OP.lines .~ [line]
            ]
    PD.getLocation location
