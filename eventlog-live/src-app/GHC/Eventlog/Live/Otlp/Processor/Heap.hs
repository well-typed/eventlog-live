{-# LANGUAGE OverloadedStrings #-}

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
import Data.Machine (Process, ProcessT, asParts, echo, mapping, (~>))
import Data.Proxy (Proxy (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Heap (GcStatsData (..), MemReturnData (..))
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otlp.Processor.Common.Core (runIf)
import GHC.Eventlog.Live.Otlp.Processor.Common.Metrics (MetricProcessor (..), asGauge, asSum, runMetricProcessor, viaLast)
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..))
import IpeDB.Database qualified as DB
import IpeDB.Types.InfoProv qualified as IP
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM

--------------------------------------------------------------------------------
-- processHeapEvents
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Logger m ->
  Maybe (DB.Table IP.InfoProvId IP.InfoProv) ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapEvents verbosity maybeInfoProvTable maybeHeapProfBreakdown fullConfig =
  M.fanoutTick
    [ processHeapAllocated fullConfig
    , processBlocksSize fullConfig
    , processHeapSize fullConfig
    , processHeapLive fullConfig
    , processMemReturn fullConfig
    , processGcStats fullConfig
    , processHeapProfSample verbosity maybeInfoProvTable maybeHeapProfBreakdown fullConfig
    ]

--------------------------------------------------------------------------------
-- HeapAllocated

processHeapAllocated :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapAllocated =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapAllocated"
      , dataProcessor = M.processHeapAllocatedData
      , aggregators = viaLast
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data =
          asSum
            [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_CUMULATIVE
            , OM.isMonotonic .~ True
            ]
      }

--------------------------------------------------------------------------------
-- HeapSize

processHeapSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapSize =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapSize"
      , dataProcessor = M.processHeapSizeData
      , aggregators = viaLast
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data = asGauge
      }

--------------------------------------------------------------------------------
-- BlocksSize

processBlocksSize :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processBlocksSize =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"blocksSize"
      , dataProcessor = M.processBlocksSizeData
      , aggregators = viaLast
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data = asGauge
      }

--------------------------------------------------------------------------------
-- HeapLive

processHeapLive :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapLive =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapLive"
      , dataProcessor = M.processHeapLiveData
      , aggregators = viaLast
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data = asGauge
      }

--------------------------------------------------------------------------------
-- MemReturn

processMemReturn :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processMemReturn fullConfig =
  runIf (shouldComputeMemReturn fullConfig) $
    M.liftTick M.processMemReturnData
      ~> M.fanoutTick
        [ runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"memCurrent"
              , dataProcessor = mapping (fmap (.current))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "{mblock}"
              , asMetric'Data = asGauge
              }
            fullConfig
        , runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"memNeeded"
              , dataProcessor = mapping (fmap (.needed))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "{mblock}"
              , asMetric'Data = asGauge
              }
            fullConfig
        , runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"memReturned"
              , dataProcessor = mapping (fmap (.returned))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "{mblock}"
              , asMetric'Data = asGauge
              }
            fullConfig
        ]

{- |
Internal helper.
Determine whether the MemReturn data should be computed.
-}
shouldComputeMemReturn :: FullConfig -> Bool
shouldComputeMemReturn fullConfig =
  C.processorEnabled (.metrics) (.memCurrent) fullConfig
    || C.processorEnabled (.metrics) (.memNeeded) fullConfig
    || C.processorEnabled (.metrics) (.memReturned) fullConfig

--------------------------------------------------------------------------------
-- GcStats

processGcStats :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processGcStats fullConfig =
  runIf (shouldComputeGcStats fullConfig) $
    M.liftTick M.processGcStatsData
      ~> M.fanoutTick
        [ runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"gcCopied"
              , dataProcessor = mapping (fmap (.copied))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "By"
              , asMetric'Data = asGauge
              }
            fullConfig
        , runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"gcSlop"
              , dataProcessor = mapping (fmap (.slop))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "By"
              , asMetric'Data = asGauge
              }
            fullConfig
        , runMetricProcessor
            MetricProcessor
              { metricProcessorProxy = Proxy @"gcFragmentation"
              , dataProcessor = mapping (fmap (.fragmentation))
              , aggregators = viaLast
              , postProcessor = echo
              , unit = "By"
              , asMetric'Data = asGauge
              }
            fullConfig
        ]

{- |
Internal helper.
Determine whether the MemReturn data should be computed.
-}
shouldComputeGcStats :: FullConfig -> Bool
shouldComputeGcStats fullConfig =
  C.processorEnabled (.metrics) (.gcCopied) fullConfig
    || C.processorEnabled (.metrics) (.gcSlop) fullConfig
    || C.processorEnabled (.metrics) (.gcFragmentation) fullConfig

--------------------------------------------------------------------------------
-- HeapProfSample

processHeapProfSample ::
  (MonadIO m) =>
  Logger m ->
  Maybe (DB.Table IP.InfoProvId IP.InfoProv) ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapProfSample logger maybeInfoProvTable maybeHeapProfBreakdown =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapProfSample"
      , dataProcessor = M.processHeapProfSampleData logger maybeInfoProvTable maybeHeapProfBreakdown
      , aggregators = viaLast
      , postProcessor = mapping M.heapProfSamples ~> asParts
      , unit = "By"
      , asMetric'Data = asGauge
      }
