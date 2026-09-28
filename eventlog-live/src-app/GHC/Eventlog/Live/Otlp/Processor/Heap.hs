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
import GHC.Eventlog.Live.Data.Metric (AggregationTemporality (..), MetricKind (..), MetricUnit (..), Monotonicity (..))
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

instance KnownMetric C.HeapAllocatedMetric where
  type NameOf C.HeapAllocatedMetric = "heapAllocated"
  type TypeOf C.HeapAllocatedMetric = Word64
  type UnitOf C.HeapAllocatedMetric = 'Byte
  type KindOf C.HeapAllocatedMetric = 'Sum 'Cumulative 'Monotonic

processHeapAllocated :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapAllocated =
  process (Proxy @C.HeapAllocatedMetric) M.processHeapAllocated

--------------------------------------------------------------------------------
-- HeapSize

instance KnownMetric C.HeapSizeMetric where
  type NameOf C.HeapSizeMetric = "heapSize"
  type TypeOf C.HeapSizeMetric = Word64
  type UnitOf C.HeapSizeMetric = 'Byte
  type KindOf C.HeapSizeMetric = 'Gauge

processHeapSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapSize =
  process (Proxy @C.HeapSizeMetric) M.processHeapSize

--------------------------------------------------------------------------------
-- BlocksSize

instance KnownMetric C.BlocksSizeMetric where
  type NameOf C.BlocksSizeMetric = "blocksSize"
  type TypeOf C.BlocksSizeMetric = Word64
  type UnitOf C.BlocksSizeMetric = 'Byte
  type KindOf C.BlocksSizeMetric = 'Gauge

processBlocksSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processBlocksSize =
  process (Proxy @C.BlocksSizeMetric) M.processBlocksSize

--------------------------------------------------------------------------------
-- HeapLive

instance KnownMetric C.HeapLiveMetric where
  type NameOf C.HeapLiveMetric = "heapLive"
  type TypeOf C.HeapLiveMetric = Word64
  type UnitOf C.HeapLiveMetric = 'Byte
  type KindOf C.HeapLiveMetric = 'Gauge

processHeapLive :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapLive =
  process (Proxy @C.HeapLiveMetric) M.processHeapLive

--------------------------------------------------------------------------------
-- MemReturn

instance KnownMetric C.MemCurrentMetric where
  type NameOf C.MemCurrentMetric = "memCurrent"
  type TypeOf C.MemCurrentMetric = Word32
  type UnitOf C.MemCurrentMetric = 'MegaBlock
  type KindOf C.MemCurrentMetric = 'Gauge

instance KnownMetric C.MemNeededMetric where
  type NameOf C.MemNeededMetric = "memNeeded"
  type TypeOf C.MemNeededMetric = Word32
  type UnitOf C.MemNeededMetric = 'MegaBlock
  type KindOf C.MemNeededMetric = 'Gauge

instance KnownMetric C.MemReturnedMetric where
  type NameOf C.MemReturnedMetric = "memReturned"
  type TypeOf C.MemReturnedMetric = Word32
  type UnitOf C.MemReturnedMetric = 'MegaBlock
  type KindOf C.MemReturnedMetric = 'Gauge

processMemReturn :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList SomeMetric))
processMemReturn fullConfig =
  processAllWith fullConfig M.processMemReturn $
    select (Proxy @C.MemCurrentMetric) (.current)
      :&: select (Proxy @C.MemNeededMetric) (.needed)
      :&: select (Proxy @C.MemReturnedMetric) (.returned)
      :&: End

--------------------------------------------------------------------------------
-- GcStats

instance KnownMetric C.GcCopiedMetric where
  type NameOf C.GcCopiedMetric = "gcCopied"
  type TypeOf C.GcCopiedMetric = Word64
  type UnitOf C.GcCopiedMetric = 'Byte
  type KindOf C.GcCopiedMetric = 'Gauge

instance KnownMetric C.GcSlopMetric where
  type NameOf C.GcSlopMetric = "gcSlop"
  type TypeOf C.GcSlopMetric = Word64
  type UnitOf C.GcSlopMetric = 'Byte
  type KindOf C.GcSlopMetric = 'Gauge

instance KnownMetric C.GcFragmentationMetric where
  type NameOf C.GcFragmentationMetric = "gcFragmentation"
  type TypeOf C.GcFragmentationMetric = Word64
  type UnitOf C.GcFragmentationMetric = 'Byte
  type KindOf C.GcFragmentationMetric = 'Gauge

processGcStats :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList SomeMetric))
processGcStats fullConfig =
  processAllWith fullConfig M.processGcStats $
    select (Proxy @C.GcCopiedMetric) (.copied)
      :&: select (Proxy @C.GcSlopMetric) (.slop)
      :&: select (Proxy @C.GcFragmentationMetric) (.fragmentation)
      :&: End

--------------------------------------------------------------------------------
-- HeapProfSample

instance KnownMetric C.HeapProfSampleMetric where
  type NameOf C.HeapProfSampleMetric = "heapProfSample"
  type TypeOf C.HeapProfSampleMetric = Word64
  type UnitOf C.HeapProfSampleMetric = 'Byte
  type KindOf C.HeapProfSampleMetric = 'Gauge

processHeapProfSample ::
  (MonadIO m) =>
  Logger m ->
  Maybe (DB.Table IP.InfoProvId IP.InfoProv) ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick SomeMetric)
processHeapProfSample logger maybeInfoProvTable maybeHeapProfBreakdown =
  processWith @C.HeapProfSampleMetric
    MetricProcessor
      { processor = M.processHeapProfSample logger maybeInfoProvTable maybeHeapProfBreakdown
      , aggregators = viaLast
      , ungroup = M.heapProfSamples
      }
