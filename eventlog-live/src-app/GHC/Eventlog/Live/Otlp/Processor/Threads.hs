{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otlp.Processor.Threads
Description : Thread Event Processors for OTLP.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otlp.Processor.Threads (
  processThreadEvents,
)
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (ProcessT, asParts, echo, mapping, (~>))
import Data.Machine.Fanout (fanout)
import Data.Proxy (Proxy (..))
import GHC.Eventlog.Live.Data.Metric qualified as M
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Analysis.Capability qualified as M
import GHC.Eventlog.Live.Machine.Analysis.Thread qualified as M
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Machine.WithStartTime qualified as M
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Config.Types (FullConfig (..))
import GHC.Eventlog.Live.Otlp.Processor.Common.Core (runIf)
import GHC.Eventlog.Live.Otlp.Processor.Common.Metrics (MetricProcessor (..), asGauge, asSum, runMetricProcessor, viaLast)
import GHC.Eventlog.Live.Otlp.Processor.Common.Traces (asSpan)
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
                M.processThreadStateSpans' M.tryGetTimeUnixNano (.value) M.setWithStartTime'value verbosity
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
              [ M.liftTick M.processCapabilityUsageDurationData
                  ~> M.fanoutTick
                    [ runMetricProcessor
                        MetricProcessor
                          { metricProcessorProxy = Proxy @"capabilityUsage"
                          , dataProcessor = mapping M.toMetric
                          , aggregators = viaLast
                          , postProcessor = echo
                          , unit = "ns"
                          , asMetric'Data =
                              asSum
                                [ OM.aggregationTemporality .~ OM.AGGREGATION_TEMPORALITY_CUMULATIVE
                                , OM.isMonotonic .~ True
                                ]
                          }
                        fullConfig
                    , runMetricProcessor
                        MetricProcessor
                          { metricProcessorProxy = Proxy @"productivity"
                          , dataProcessor = M.processProductivity ~> mapping (fmap (* 100.0) . M.toMetric)
                          , aggregators = viaLast
                          , postProcessor = echo
                          , unit = "%"
                          , asMetric'Data = asGauge
                          }
                        fullConfig
                    ]
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
shouldProcessThreadEvents = shouldComputeThreadStateSpan

{- |
Internal helper.
Determine whether or not the capability usage spans should be computed.
-}
shouldComputeCapabilityUsageSpan :: FullConfig -> Bool
shouldComputeCapabilityUsageSpan fullConfig =
  C.processorEnabled (.metrics) (.capabilityUsage) fullConfig
    || C.processorEnabled (.metrics) (.productivity) fullConfig
    || C.processorEnabled (.traces) (.capabilityUsage) fullConfig

{- |
Internal helper.
Determine whether or not the thread state spans should be computed.
-}
shouldComputeThreadStateSpan :: FullConfig -> Bool
shouldComputeThreadStateSpan fullConfig =
  C.processorEnabled (.traces) (.threadState) fullConfig
    || shouldComputeCapabilityUsageSpan fullConfig
