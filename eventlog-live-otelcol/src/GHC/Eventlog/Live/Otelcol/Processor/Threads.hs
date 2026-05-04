{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Threads
Description : Thread Event Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Threads (
  processThreadEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (ProcessT, asParts, echo, mapping, (~>))
import Data.Machine.Fanout (fanout)
import Data.Proxy (Proxy (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Capability qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core (runIf)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Metrics (MetricProcessor (..), asSum, runMetricProcessor, viaSum)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Traces (asSpan)
import GHC.RTS.Events (Event (..))
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT

data OneOf a b c = A !a | B !b | C !c

processThreadEvents ::
  (MonadIO m) =>
  Logger m ->
  FullConfig ->
  ProcessT m (Tick (WithStartTime Event)) (Tick (DList (Either OM.Metric OT.Span)))
processThreadEvents verbosity fullConfig =
  runIf (shouldProcessThreadEvents fullConfig) $
    M.sortByTicks (.value.evTime) fullConfig.eventlogFlushIntervalX
      ~> M.liftTick
        ( fanout
            [ M.validateOrder verbosity (.value.evTime)
            , runIf (shouldComputeCapabilityUsageSpan fullConfig) $
                M.processGCSpans verbosity
                  ~> mapping (D.singleton . A)
            , runIf (shouldComputeThreadStateSpan fullConfig) $
                M.processThreadStateSpans' M.getTimeUnixNano (.value) M.setWithStartTime'value verbosity
                  ~> fanout
                    [ M.asMutatorSpans' (.value) M.setWithStartTime'value
                        ~> mapping (D.singleton . B)
                    , mapping (D.singleton . C)
                    ]
            ]
        )
      ~> M.liftTick
        ( asParts
            ~> mapping repackCapabilityUsageSpanOrThreadStateSpan
        )
      ~> fanout
        [ M.liftTick
            ( mapping leftToMaybe
                ~> asParts
            )
            ~> M.fanoutTick
              [ runMetricProcessor
                  MetricProcessor
                    { metricProcessorProxy = Proxy @"capabilityUsage"
                    , dataProcessor = M.processCapabilityUsageMetrics
                    , aggregators = viaSum
                    , postProcessor = echo
                    , unit = "ns"
                    , asMetric'Data =
                        asSum
                          [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_DELTA
                          , OM.isMonotonic .~ True
                          ]
                    }
                  fullConfig
                  ~> mapping (fmap (fmap Left))
              , runIf (C.processorEnabled (.traces) (.capabilityUsage) fullConfig) $
                  M.liftTick
                    ( M.dropStartTime
                        ~> asSpan fullConfig
                        ~> mapping (D.singleton . Right)
                    )
                    ~> M.batchByTick
              ]
        , runIf (C.processorEnabled (.traces) (.threadState) fullConfig) $
            M.liftTick
              ( mapping rightToMaybe
                  ~> asParts
                  ~> asSpan fullConfig
                  ~> mapping (D.singleton . Right)
              )
              ~> M.batchByTick
        ]
 where
  repackCapabilityUsageSpanOrThreadStateSpan = \case
    A i -> Left $ fmap Left i
    B i -> Left $ fmap Right i
    C i -> Right i.value

{- |
Internal helper.
Get the `Left` value, if any.
-}
leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

{- |
Internal helper.
Get the `Right` value, if any.
-}
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

{- |
Internal helper.
Determine whether or not any thread events should be processed at all.
-}
shouldProcessThreadEvents :: FullConfig -> Bool
shouldProcessThreadEvents fullConfig =
  C.processorEnabled (.metrics) (.capabilityUsage) fullConfig
    || C.processorEnabled (.traces) (.capabilityUsage) fullConfig
    || C.processorEnabled (.traces) (.threadState) fullConfig

{- |
Internal helper.
Determine whether or not the capability usage spans should be computed.
-}
shouldComputeCapabilityUsageSpan :: FullConfig -> Bool
shouldComputeCapabilityUsageSpan fullConfig =
  C.processorEnabled (.traces) (.capabilityUsage) fullConfig
    || C.processorEnabled (.metrics) (.capabilityUsage) fullConfig

{- |
Internal helper.
Determine whether or not the thread state spans should be computed.
-}
shouldComputeThreadStateSpan :: FullConfig -> Bool
shouldComputeThreadStateSpan fullConfig =
  C.processorEnabled (.traces) (.threadState) fullConfig
    || shouldComputeCapabilityUsageSpan fullConfig
