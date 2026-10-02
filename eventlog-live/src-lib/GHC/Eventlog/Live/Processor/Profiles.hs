{- |
Module      : GHC.Eventlog.Live.Processor.Profiles
Description : Profile Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Processor.Profiles (
  processProfileEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (ProcessT, mapping, (~>))
import Data.Proxy (Proxy (..))
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Profile qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Processor.Core
import GHC.Eventlog.Live.Types.Profiles (SomeSamples (..), toSample)
import GHC.RTS.Events (Event (..))
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre qualified as CC
import IpeDB.Types.InfoProv qualified as IP

--------------------------------------------------------------------------------
-- Profiles
--------------------------------------------------------------------------------

processProfileEvents ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  DB.Table CC.CostCentreId CC.CostCentre ->
  DB.Table IP.InfoProvId IP.InfoProv ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList SomeSamples))
processProfileEvents logger ccdb ipedb config =
  M.fanoutTick
    [ processProfSampleCostCentre logger ccdb config
        ~> M.liftTick (mapping D.singleton)
    , processGhcStackProfiler logger ipedb config
        ~> M.liftTick (mapping D.singleton)
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
  ProcessT m (Tick (WithStartTime Event)) (Tick SomeSamples)
processGhcStackProfiler logger ipedb config =
  runIf (C.processorEnabled (.profiles) (.callStackProfile) config) $
    M.liftTick (M.processGhcStackProfilerData logger ipedb ~> mapping (D.singleton . toSample))
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.callStackProfile) config)
      ~> M.liftTick (mapping $ SomeSamples (Proxy @"callStackProfile") . D.toList)

--------------------------------------------------------------------------------
-- Processor for cost-centre stack samples
--------------------------------------------------------------------------------

processProfSampleCostCentre ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  DB.Table CC.CostCentreId CC.CostCentre ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick SomeSamples)
processProfSampleCostCentre logger ccdb config =
  runIf (C.processorEnabled (.profiles) (.costCentreStackProfile) config) $
    M.liftTick (M.processProfSampleCostCentreData logger ccdb ~> mapping (D.singleton . toSample))
      ~> M.batchByTicks (C.processorExportBatches (.profiles) (.callStackProfile) config)
      ~> M.liftTick (mapping $ SomeSamples (Proxy @"costCentreStackProfile") . D.toList)
