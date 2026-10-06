{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.App.Exporter.Otlp.Profiles (
  -- * Export
  exportResourceProfiles,

  -- * Conversion to OTLP
  toExportProfileServiceRequest,
  toProfilesData,
  toResourceProfiles,
  toScopeProfiles,
  toProfiles,
)
where

import Control.Exception (SomeException (..), catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, traversing)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Eventlog.Live.App.Exporter.Otlp.Core
import GHC.Eventlog.Live.App.Exporter.Otlp.ProfilesDictionary (ProfilesDictionary, SymbolIndex)
import GHC.Eventlog.Live.App.Exporter.Otlp.ProfilesDictionary qualified as PD
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (ExportResult (..), InternalMetric (..), Logger, logException, logMetric)
import GHC.Eventlog.Live.Types.Attribute ((~=))
import GHC.Eventlog.Live.Types.Profiles (KnownProfile (..), Location (..), Sample (..), SomeSamples (..), profileConfig)
import GHC.IsList (IsList (..))
import GHC.TypeLits (symbolVal)
import IpeDB.Types.SrcLoc (Point (..), SrcLoc (..))
import Lens.Family2 ((.~), (^.))
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService qualified as OPS
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService_Fields qualified as OPS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter for Profiles

exportResourceProfiles ::
  Logger IO ->
  Exporter ->
  ProcessT IO OPS.ExportProfilesServiceRequest ()
exportResourceProfiles logger exporter =
  traversing sendResourceProfiles
 where
  sendResourceProfiles :: OPS.ExportProfilesServiceRequest -> IO ()
  sendResourceProfiles exportProfilesServiceRequest =
    doExport `catch` handleSomeException
   where
    doExport :: IO ()
    doExport = do
      resp <- export @OPS.ProfilesService @"export" logger exporter exportProfilesServiceRequest
      let !exported = countSamplesInExportProfileServiceRequest exportProfilesServiceRequest
      let !rejected = resp ^. OPS.partialSuccess . OPS.rejectedProfiles
      liftIO (logMetric logger ExportSamples ExportResult{..})
      unless (rejected == 0) $
        liftIO (logException logger $ ExportError $ resp ^. OPS.partialSuccess . OPS.errorMessage)

    handleSomeException :: SomeException -> IO ()
    handleSomeException = logException logger

--------------------------------------------------------------------------------
-- CanExportToConsole

instance CanExportToConsole OPS.ProfilesService "export"

--------------------------------------------------------------------------------
-- CanExportToOltpViaGrpc

type instance G.RequestMetadata (Protobuf OPS.ProfilesService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OPS.ProfilesService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OPS.ProfilesService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- CanExportToOltpViaHttpProtobuf

instance CanExportToOltpViaHttpProtobuf OPS.ProfilesService "export" where
  apiPath :: String
  apiPath = "/v1development/profiles"

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

{- |
Internal helper.
Count the number of 'OP.Sample' values in an 'OPS.ExportProfilesServiceRequest'.
-}
{-# SPECIALIZE countSamplesInExportProfileServiceRequest :: OPS.ExportProfilesServiceRequest -> Int64 #-}
{-# SPECIALIZE countSamplesInExportProfileServiceRequest :: OPS.ExportProfilesServiceRequest -> Word #-}
countSamplesInExportProfileServiceRequest :: (Integral i) => OPS.ExportProfilesServiceRequest -> i
countSamplesInExportProfileServiceRequest exportProfileServiceRequest =
  getSum $ foldMap (Sum . countSamplesInResourceProfiles) (exportProfileServiceRequest ^. OPS.vec'resourceProfiles)

countSamplesInResourceProfiles :: (Integral i) => OP.ResourceProfiles -> i
countSamplesInResourceProfiles resourceProfiles =
  getSum $ foldMap (Sum . countSamplesInScopeProfiles) (resourceProfiles ^. OP.vec'scopeProfiles)

countSamplesInScopeProfiles :: (Integral i) => OP.ScopeProfiles -> i
countSamplesInScopeProfiles scopeProfiles =
  getSum $ foldMap (Sum . countSamplesInProfile) (scopeProfiles ^. OP.vec'profiles)

countSamplesInProfile :: (Integral i) => OP.Profile -> i
countSamplesInProfile profile =
  fromIntegral $
    V.length (profile ^. OP.vec'samples)

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

toProfiles ::
  FullConfig ->
  [SomeSamples] ->
  Maybe ([OP.Profile], OP.ProfilesDictionary)
toProfiles fullConfig profiles =
  ifNonEmpty opProfiles opProfilesData
 where
  opProfilesData@(opProfiles, _) =
    second PD.toProfilesDictionary . runIdentity . flip runStateT PD.empty $ do
      traverse (getProfile fullConfig) profiles

--------------------------------------------------------------------------------
-- Translating profiles to OTLP profiles

getProfile ::
  forall m.
  (Monad m) =>
  FullConfig ->
  SomeSamples ->
  StateT ProfilesDictionary m OP.Profile
getProfile fullConfig (SomeSamples (profile :: Proxy profile) samples) = do
  -- Construct the sample type.
  let metricType = symbolVal (Proxy @(GetProfileMetricName profile))
  let metricUnit = symbolVal (Proxy @(GetProfileMetricUnit profile))
  typeStrindex <- PD.getText (T.pack metricType)
  unitStrindex <- PD.getText (T.pack metricUnit)
  let opSampleType :: OP.ValueType
      opSampleType =
        messageWith
          [ OP.typeStrindex .~ typeStrindex
          , OP.unitStrindex .~ unitStrindex
          ]

  -- Construct the profile.
  opSamples <- traverse (getSample fullConfig profile) samples
  let opProfile :: OP.Profile
      opProfile =
        messageWith
          [ OP.samples .~ opSamples
          , OP.sampleType .~ opSampleType
          ]
  pure opProfile

getSample ::
  (Monad m, KnownProfile profile) =>
  FullConfig ->
  Proxy profile ->
  Sample (GetProfileMetricType profile) ->
  StateT ProfilesDictionary m OP.Sample
getSample fullConfig (_profile :: Proxy profile) sample = do
  -- Encode the stack.
  stackIndex <- getStack sample.stack

  -- Encode the attributes.
  let name = C.processorName (.profiles) (profileConfig $ Proxy @profile) fullConfig
  let attributes = "__name__" ~= name : toList sample.attrs
  attributeIndices <- catMaybes <$> traverse PD.getAttr attributes

  -- Construct the sample.
  let opSample :: OP.Sample
      opSample =
        messageWith $
          [ OP.values .~ [fromIntegral @(GetProfileMetricType profile) @Int64 sample.value]
          , OP.stackIndex .~ stackIndex
          , OP.attributeIndices .~ attributeIndices
          , OP.timestampsUnixNano .~? sequence [sample.maybeTimeUnixNano]
          ]
  pure opSample

getStack ::
  (Monad m) =>
  Vector Location ->
  StateT ProfilesDictionary m SymbolIndex
getStack stack = do
  -- Encode the locations.
  locationIndices <- traverse getLocation stack

  -- Create the stack.
  let opStack :: OP.Stack
      opStack =
        messageWith $
          [ OP.vec'locationIndices .~ V.convert locationIndices
          ]

  -- Index the stack.
  PD.getStack opStack

getLocation ::
  (Monad m) =>
  Location ->
  StateT ProfilesDictionary m SymbolIndex
getLocation Location{..} = do
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

  -- Create the location.
  let opLocation :: OP.Location
      opLocation =
        messageWith $
          [ OP.lines .~ [line]
          ]

  -- Index the location.
  PD.getLocation opLocation
