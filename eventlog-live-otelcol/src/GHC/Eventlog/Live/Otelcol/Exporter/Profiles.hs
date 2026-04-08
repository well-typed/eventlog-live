{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Otelcol.Exporter.Profiles (
  -- * Profiles
  ExportProfileResult (..),
  RejectedProfilesError (..),
  exportResourceProfiles,
)
where

import Control.Exception (Exception (..), SomeException (..), catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Machine (ProcessT, await, construct, yield)
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Eventlog.Live.Machine.Core (Tick (..))
import Lens.Family2 ((^.))
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Network.GRPC.Common.Protobuf qualified as G
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService qualified as OPS
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService_Fields qualified as OPS
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP
import Text.Printf (printf)

data ExportProfileResult
  = ExportProfileResult
  { exportedProfiles :: !Int64
  , rejectedProfiles :: !Int64
  , maybeSomeException :: Maybe SomeException
  }
  deriving (Show)

pattern ExportProfileSuccess :: Int64 -> ExportProfileResult
pattern ExportProfileSuccess exportedProfiles =
  ExportProfileResult exportedProfiles 0 Nothing

pattern ExportProfileError :: Int64 -> Int64 -> SomeException -> ExportProfileResult
pattern ExportProfileError exportedProfiles rejectedProfiles someException =
  ExportProfileResult exportedProfiles rejectedProfiles (Just someException)

data RejectedProfilesError
  = RejectedProfilesError
  { rejectedProfiles :: !Int64
  , errorMessage :: !Text
  }
  deriving (Show)

instance Exception RejectedProfilesError where
  displayException :: RejectedProfilesError -> String
  displayException RejectedProfilesError{..} =
    printf "Error: OpenTelemetry Collector rejectedProfiles %d data points with message: %s" rejectedProfiles errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Profiles

exportResourceProfiles ::
  G.Connection ->
  ProcessT IO (Tick OPS.ExportProfilesServiceRequest) (Tick ExportProfileResult)
exportResourceProfiles conn =
  construct $ go False
 where
  go exportedProfiles =
    await >>= \case
      Tick -> do
        unless exportedProfiles $
          yield (Item $ ExportProfileSuccess 0)
        yield Tick
        go False
      Item exportProfilesServiceRequest -> do
        exportTraceResult <- liftIO (sendResourceProfiles exportProfilesServiceRequest)
        yield (Item exportTraceResult)
        go True

  sendResourceProfiles :: OPS.ExportProfilesServiceRequest -> IO ExportProfileResult
  sendResourceProfiles exportProfilesServiceRequest =
    doGrpc `catch` handleSomeException
   where
    !exportedProfiles = countSamplesInExportProfileServiceRequest exportProfilesServiceRequest

    doGrpc :: IO ExportProfileResult
    doGrpc = do
      G.nonStreaming conn (G.rpc @(Protobuf OPS.ProfilesService "export")) (G.Proto exportProfilesServiceRequest) >>= \case
        G.Proto resp
          | resp ^. OPS.partialSuccess . OPS.rejectedProfiles == 0 ->
              pure $ ExportProfileSuccess exportedProfiles
          | otherwise -> do
              let !rejectedProfiles = resp ^. OPS.partialSuccess . OPS.rejectedProfiles
              let !rejectedMetricsError = RejectedProfilesError{errorMessage = resp ^. OPS.partialSuccess . OPS.errorMessage, ..}
              pure $ ExportProfileError exportedProfiles rejectedProfiles (SomeException rejectedMetricsError)

    handleSomeException :: SomeException -> IO ExportProfileResult
    handleSomeException someException = pure $ ExportProfileError 0 exportedProfiles someException

type instance G.RequestMetadata (Protobuf OPS.ProfilesService meth) = G.NoMetadata

type instance G.ResponseInitialMetadata (Protobuf OPS.ProfilesService meth) = G.NoMetadata

type instance G.ResponseTrailingMetadata (Protobuf OPS.ProfilesService meth) = G.NoMetadata

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
