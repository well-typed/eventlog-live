{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GHC.Eventlog.Live.Processor.Heap
Description : Heap Event Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Processor.Heap (
  processHeapEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (Process, ProcessT, mapping, (~>))
import Data.Proxy (Proxy (..))
import GHC.Eventlog.Live.Config (FullConfig (..))
import GHC.Eventlog.Live.Config qualified as C
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Heap (GcStats (..), MemReturn (..))
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Processor.Core.Metrics
import GHC.Eventlog.Live.Types.Metrics (SomeMetrics (..))
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..))
import IpeDB.Database qualified as DB
import IpeDB.Types.InfoProv qualified as IP

--------------------------------------------------------------------------------
-- processHeapEvents
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Logger m ->
  Maybe (DB.Table IP.InfoProvId IP.InfoProv) ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList SomeMetrics))
processHeapEvents verbosity maybeInfoProvTable maybeHeapProfBreakdown fullConfig =
  M.fanoutTick
    [ processHeapAllocated fullConfig
        ~> M.liftTick (mapping D.singleton)
    , processHeapSize fullConfig
        ~> M.liftTick (mapping D.singleton)
    , processBlocksSize fullConfig
        ~> M.liftTick (mapping D.singleton)
    , processHeapLive fullConfig
        ~> M.liftTick (mapping D.singleton)
    , processMemReturn fullConfig
    , processGcStats fullConfig
    , processHeapProfSample verbosity maybeInfoProvTable maybeHeapProfBreakdown fullConfig
        ~> M.liftTick (mapping D.singleton)
    ]

--------------------------------------------------------------------------------
-- HeapAllocated

processHeapAllocated :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetrics)
processHeapAllocated =
  process (Proxy @C.HeapAllocatedMetric) M.processHeapAllocated

--------------------------------------------------------------------------------
-- HeapSize

processHeapSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetrics)
processHeapSize =
  process (Proxy @C.HeapSizeMetric) M.processHeapSize

--------------------------------------------------------------------------------
-- BlocksSize

processBlocksSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetrics)
processBlocksSize =
  process (Proxy @C.BlocksSizeMetric) M.processBlocksSize

--------------------------------------------------------------------------------
-- HeapLive

processHeapLive :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetrics)
processHeapLive =
  process (Proxy @C.HeapLiveMetric) M.processHeapLive

--------------------------------------------------------------------------------
-- MemReturn

processMemReturn :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList SomeMetrics))
processMemReturn fullConfig =
  processAllWith fullConfig M.processMemReturn $
    select (Proxy @C.MemCurrentMetric) (.current)
      :&: select (Proxy @C.MemNeededMetric) (.needed)
      :&: select (Proxy @C.MemReturnedMetric) (.returned)
      :&: End

--------------------------------------------------------------------------------
-- GcStats

processGcStats :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList SomeMetrics))
processGcStats fullConfig =
  processAllWith fullConfig M.processGcStats $
    select (Proxy @C.GcCopiedMetric) (.copied)
      :&: select (Proxy @C.GcSlopMetric) (.slop)
      :&: select (Proxy @C.GcFragmentationMetric) (.fragmentation)
      :&: End

--------------------------------------------------------------------------------
-- HeapProfSample

processHeapProfSample ::
  (MonadIO m) =>
  Logger m ->
  Maybe (DB.Table IP.InfoProvId IP.InfoProv) ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick SomeMetrics)
processHeapProfSample logger maybeInfoProvTable maybeHeapProfBreakdown =
  processWith @C.HeapProfSampleMetric
    MetricProcessor
      { processor = M.processHeapProfSample logger maybeInfoProvTable maybeHeapProfBreakdown
      , aggregators = viaLast
      , ungroup = M.heapProfSamples
      }
