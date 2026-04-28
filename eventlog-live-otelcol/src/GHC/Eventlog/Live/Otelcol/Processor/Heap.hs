{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Heap
Description : Heap Event Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Heap (
  processHeapEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.Machine (Process, ProcessT, asParts, echo, mapping, (~>))
import Data.Proxy (Proxy (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Heap (MemReturnData (..))
import GHC.Eventlog.Live.Machine.Analysis.Heap qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core (runIf)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Metrics (MetricProcessor (..), asGauge, asSum, runMetricProcessor, viaLast, viaSum)
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..))
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM

--------------------------------------------------------------------------------
-- processHeapEvents
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Logger m ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapEvents verbosity maybeHeapProfBreakdown fullConfig =
  M.fanoutTick
    [ processHeapAllocated fullConfig
    , processBlocksSize fullConfig
    , processHeapSize fullConfig
    , processHeapLive fullConfig
    , processMemReturn fullConfig
    , processHeapProfSample verbosity maybeHeapProfBreakdown fullConfig
    ]

--------------------------------------------------------------------------------
-- HeapAllocated

processHeapAllocated :: FullConfig -> Process (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapAllocated =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapAllocated"
      , dataProcessor = M.processHeapAllocatedData
      , aggregators = viaSum
      , postProcessor = echo
      , unit = "By"
      , asMetric'Data =
          asSum
            [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
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
-- HeapProfSample

processHeapProfSample ::
  (MonadIO m) =>
  Logger m ->
  Maybe HeapProfBreakdown ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList OM.Metric))
processHeapProfSample logger maybeHeapProfBreakdown =
  runMetricProcessor
    MetricProcessor
      { metricProcessorProxy = Proxy @"heapProfSample"
      , dataProcessor = M.processHeapProfSampleData logger maybeHeapProfBreakdown
      , aggregators = viaLast
      , postProcessor = mapping M.heapProfSamples ~> asParts
      , unit = "By"
      , asMetric'Data = asGauge
      }
