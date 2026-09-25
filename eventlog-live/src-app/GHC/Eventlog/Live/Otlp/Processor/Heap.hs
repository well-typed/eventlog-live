{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : GHC.Eventlog.Live.Otlp.Processor.Heap
Description : Heap Event Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otlp.Processor.Heap (
  processHeapEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (Process, ProcessT, mapping, (~>))
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import GHC.Eventlog.Live.Data.Metric (AggregationTemporality (..), MetricPointKind (..), MetricUnit (..), Monotonicity (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Heap (GcStats (..), MemReturn (..))
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otlp.Processor.Common.Metrics
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
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList SomeMetric))
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

instance KnownMetric "heapAllocated" where
  type ConfigOf "heapAllocated" = C.HeapAllocatedMetric
  type TypeOf "heapAllocated" = Word64
  type UnitOf "heapAllocated" = 'Byte
  type PointKindOf "heapAllocated" = 'Sum 'Cumulative 'Monotonic

processHeapAllocated :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapAllocated =
  process (Proxy @"heapAllocated") M.processHeapAllocated

--------------------------------------------------------------------------------
-- HeapSize

instance KnownMetric "heapSize" where
  type ConfigOf "heapSize" = C.HeapSizeMetric
  type TypeOf "heapSize" = Word64
  type UnitOf "heapSize" = 'Byte
  type PointKindOf "heapSize" = 'Gauge

processHeapSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapSize =
  process (Proxy @"heapSize") M.processHeapSize

--------------------------------------------------------------------------------
-- BlocksSize

instance KnownMetric "blocksSize" where
  type ConfigOf "blocksSize" = C.BlocksSizeMetric
  type TypeOf "blocksSize" = Word64
  type UnitOf "blocksSize" = 'Byte
  type PointKindOf "blocksSize" = 'Gauge

processBlocksSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processBlocksSize =
  process (Proxy @"blocksSize") M.processBlocksSize

--------------------------------------------------------------------------------
-- HeapLive

instance KnownMetric "heapLive" where
  type ConfigOf "heapLive" = C.HeapLiveMetric
  type TypeOf "heapLive" = Word64
  type UnitOf "heapLive" = 'Byte
  type PointKindOf "heapLive" = 'Gauge

processHeapLive :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapLive =
  process (Proxy @"heapLive") M.processHeapLive

--------------------------------------------------------------------------------
-- MemReturn

instance KnownMetric "memCurrent" where
  type ConfigOf "memCurrent" = C.MemCurrentMetric
  type TypeOf "memCurrent" = Word32
  type UnitOf "memCurrent" = 'MegaBlock
  type PointKindOf "memCurrent" = 'Gauge

instance KnownMetric "memNeeded" where
  type ConfigOf "memNeeded" = C.MemNeededMetric
  type TypeOf "memNeeded" = Word32
  type UnitOf "memNeeded" = 'MegaBlock
  type PointKindOf "memNeeded" = 'Gauge

instance KnownMetric "memReturned" where
  type ConfigOf "memReturned" = C.MemReturnedMetric
  type TypeOf "memReturned" = Word32
  type UnitOf "memReturned" = 'MegaBlock
  type PointKindOf "memReturned" = 'Gauge

processMemReturn :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList SomeMetric))
processMemReturn fullConfig =
  processAllWith fullConfig M.processMemReturn $
    select (Proxy @"memCurrent") (.current)
      :&: select (Proxy @"memNeeded") (.needed)
      :&: select (Proxy @"memReturned") (.returned)
      :&: End

--------------------------------------------------------------------------------
-- GcStats

instance KnownMetric "gcCopied" where
  type ConfigOf "gcCopied" = C.GcCopiedMetric
  type TypeOf "gcCopied" = Word64
  type UnitOf "gcCopied" = 'Byte
  type PointKindOf "gcCopied" = 'Gauge

instance KnownMetric "gcSlop" where
  type ConfigOf "gcSlop" = C.GcSlopMetric
  type TypeOf "gcSlop" = Word64
  type UnitOf "gcSlop" = 'Byte
  type PointKindOf "gcSlop" = 'Gauge

instance KnownMetric "gcFragmentation" where
  type ConfigOf "gcFragmentation" = C.GcFragmentationMetric
  type TypeOf "gcFragmentation" = Word64
  type UnitOf "gcFragmentation" = 'Byte
  type PointKindOf "gcFragmentation" = 'Gauge

processGcStats :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList SomeMetric))
processGcStats fullConfig =
  processAllWith fullConfig M.processGcStats $
    select (Proxy @"gcCopied") (.copied)
      :&: select (Proxy @"gcSlop") (.slop)
      :&: select (Proxy @"gcFragmentation") (.fragmentation)
      :&: End

--------------------------------------------------------------------------------
-- HeapProfSample

instance KnownMetric "heapProfSample" where
  type ConfigOf "heapProfSample" = C.HeapProfSampleMetric
  type TypeOf "heapProfSample" = Word64
  type UnitOf "heapProfSample" = 'Byte
  type PointKindOf "heapProfSample" = 'Gauge

processHeapProfSample ::
  (MonadIO m) =>
  Logger m ->
  Maybe (DB.Table IP.InfoProvId IP.InfoProv) ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapProfSample logger maybeInfoProvTable maybeHeapProfBreakdown =
  processWith @"heapProfSample"
    MetricProcessor
      { processor = M.processHeapProfSample logger maybeInfoProvTable maybeHeapProfBreakdown
      , aggregators = viaLast
      , ungroup = M.heapProfSamples
      }
