{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Config.Types
Description : The implementation of @eventlog-live-otlp@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Config.Types (
  -- * Configuration type
  Config (..),
  FullConfig (..),

  -- ** Processor configuration types
  Processors (..),
  IsProcessorConfig,

  -- *** Log processor configuration types
  Logs (..),
  IsLogProcessorConfig,
  ThreadLabelLog (..),
  UserMarkerLog (..),
  UserMessageLog (..),
  InternalLogMessageLog (..),

  -- *** Metric processor configuration types
  Metrics (..),
  IsMetricProcessorConfig,
  HeapAllocatedMetric (..),
  BlocksSizeMetric (..),
  HeapSizeMetric (..),
  HeapLiveMetric (..),
  MemCurrentMetric (..),
  MemNeededMetric (..),
  MemReturnedMetric (..),
  HeapProfSampleMetric (..),
  GcCopiedMetric (..),
  GcSlopMetric (..),
  GcFragmentationMetric (..),
  CapabilityUsageMetric (..),
  ProductivityMetric (..),
  InternalEventCountMetric (..),
  InternalExportedLogsMetric (..),
  InternalRejectedLogsMetric (..),
  InternalExportedMetricsMetric (..),
  InternalRejectedMetricsMetric (..),
  InternalExportedSamplesMetric (..),
  InternalRejectedSamplesMetric (..),
  InternalExportedSpansMetric (..),
  InternalRejectedSpansMetric (..),

  -- *** Trace processor configuration types
  Traces (..),
  IsTraceProcessorConfig,
  CapabilityUsageSpan (..),
  ThreadStateSpan (..),

  -- *** Profile processor configuration types
  Profiles (..),
  IsProfileProcessorConfig,
  CallStackProfile (..),
  CostCentreStackProfile (..),

  -- ** Property types
  Duration (..),
  AggregationStrategy (..),
  toAggregationSeconds,
  ExportStrategy (..),
  toExportSeconds,
  isEnabled,
) where

import Control.Applicative (asum)
import Data.Char (isDigit)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.YAML (FromYAML (..), ToYAML, (.:?), (.=))
import Data.YAML qualified as YAML
import GHC.Records (HasField (..))
import Language.Haskell.TH.Lift.Compat (Lift)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read (readEither)

{- |
The extended configuration with derived fields.
-}
data FullConfig = FullConfig
  { batchIntervalMs :: !Int
  -- ^ The batch interval in milliseconds.
  , eventlogFlushIntervalX :: !Int
  -- ^ The @--eventlog-flush-interval@ in /batches/.
  , config :: !Config
  }

{- |
The configuration for @eventlog-live-otlp@.
-}
newtype Config = Config
  { processors :: Maybe Processors
  }
  deriving (Lift, Show)

instance FromYAML Config where
  parseYAML = YAML.withMap "Config" $ \m ->
    Config
      <$> m .:? "processors"

instance ToYAML Config where
  toYAML config =
    YAML.mapping
      [ "processors" .= config.processors
      ]

{- |
The configuration options for the processors.
-}
data Processors = Processors
  { logs :: Maybe Logs
  , metrics :: Maybe Metrics
  , traces :: Maybe Traces
  , profiles :: Maybe Profiles
  }
  deriving (Lift, Show)

instance FromYAML Processors where
  parseYAML = YAML.withMap "Processors" $ \m ->
    Processors
      <$> m .:? "logs"
      <*> m .:? "metrics"
      <*> m .:? "traces"
      <*> m .:? "profiles"

instance ToYAML Processors where
  toYAML processors =
    YAML.mapping
      [ "logs" .= processors.logs
      , "metrics" .= processors.metrics
      , "traces" .= processors.traces
      , "profiles" .= processors.profiles
      ]

{- |
The configuration options for the span processors.
-}

-- NOTE:
-- If you add a new log, search for the string...
--
--   This should be kept in sync with the list of logs.
--
-- ...and update all the relevant locations.
data Logs = Logs
  { threadLabel :: Maybe ThreadLabelLog
  , userMarker :: Maybe UserMarkerLog
  , userMessage :: Maybe UserMessageLog
  , internalLogMessage :: Maybe InternalLogMessageLog
  }
  deriving (Lift, Show)

instance FromYAML Logs where
  parseYAML =
    -- NOTE: This should be kept in sync with the list of logs.
    YAML.withMap "Logs" $ \m ->
      Logs
        <$> m .:? "thread_label"
        <*> m .:? "user_marker"
        <*> m .:? "user_message"
        <*> m .:? "internal_log_message"

instance ToYAML Logs where
  toYAML logs =
    -- NOTE: This should be kept in sync with the list of logs.
    YAML.mapping
      [ "thread_label" .= logs.threadLabel
      , "user_marker" .= logs.userMarker
      , "user_message" .= logs.userMessage
      , "internal_log_message" .= logs.internalLogMessage
      ]

{- |
The configuration options for the metric processors.
-}

-- NOTE:
-- If you add a new metric, search for the string...
--
--   This should be kept in sync with the list of metrics.
--
-- ...and update all the relevant locations.
data Metrics = Metrics
  { heapAllocated :: Maybe HeapAllocatedMetric
  , blocksSize :: Maybe BlocksSizeMetric
  , heapSize :: Maybe HeapSizeMetric
  , heapLive :: Maybe HeapLiveMetric
  , memCurrent :: Maybe MemCurrentMetric
  , memNeeded :: Maybe MemNeededMetric
  , memReturned :: Maybe MemReturnedMetric
  , gcCopied :: Maybe GcCopiedMetric
  , gcSlop :: Maybe GcSlopMetric
  , gcFragmentation :: Maybe GcFragmentationMetric
  , heapProfSample :: Maybe HeapProfSampleMetric
  , capabilityUsage :: Maybe CapabilityUsageMetric
  , productivity :: Maybe ProductivityMetric
  , internalEventCount :: Maybe InternalEventCountMetric
  , internalExportedLogs :: Maybe InternalExportedLogsMetric
  , internalRejectedLogs :: Maybe InternalRejectedLogsMetric
  , internalExportedMetrics :: Maybe InternalExportedMetricsMetric
  , internalRejectedMetrics :: Maybe InternalRejectedMetricsMetric
  , internalExportedSamples :: Maybe InternalExportedSamplesMetric
  , internalRejectedSamples :: Maybe InternalRejectedSamplesMetric
  , internalExportedSpans :: Maybe InternalExportedSpansMetric
  , internalRejectedSpans :: Maybe InternalRejectedSpansMetric
  }
  deriving (Lift, Show)

instance FromYAML Metrics where
  parseYAML =
    -- NOTE: This should be kept in sync with the list of metrics.
    YAML.withMap "Metrics" $ \m -> do
      heapAllocated <- m .:? "heap_allocated"
      blocksSize <- m .:? "blocks_size"
      heapSize <- m .:? "heap_size"
      heapLive <- m .:? "heap_live"
      memCurrent <- m .:? "mem_current"
      memNeeded <- m .:? "mem_needed"
      memReturned <- m .:? "mem_returned"
      gcCopied <- m .:? "gc_copied"
      gcSlop <- m .:? "gc_slop"
      gcFragmentation <- m .:? "gc_fragmentation"
      heapProfSample <- m .:? "heap_prof_sample"
      capabilityUsage <- m .:? "capability_usage"
      productivity <- m .:? "productivity"
      internalEventCount <- m .:? "internal_event_count"
      internalExportedLogs <- m .:? "internal_exported_logs"
      internalRejectedLogs <- m .:? "internal_rejected_logs"
      internalExportedMetrics <- m .:? "internal_exported_metrics"
      internalRejectedMetrics <- m .:? "internal_rejected_metrics"
      internalExportedSamples <- m .:? "internal_exported_samples"
      internalRejectedSamples <- m .:? "internal_rejected_samples"
      internalExportedSpans <- m .:? "internal_exported_spans"
      internalRejectedSpans <- m .:? "internal_rejected_spans"
      pure Metrics{..}

instance ToYAML Metrics where
  toYAML metrics =
    -- NOTE: This should be kept in sync with the list of metrics.
    YAML.mapping
      [ "heap_allocated" .= metrics.heapAllocated
      , "blocks_size" .= metrics.blocksSize
      , "heap_size" .= metrics.heapSize
      , "heap_live" .= metrics.heapLive
      , "mem_current" .= metrics.memCurrent
      , "mem_needed" .= metrics.memNeeded
      , "mem_returned" .= metrics.memReturned
      , "gc_copied" .= metrics.gcCopied
      , "gc_slop" .= metrics.gcSlop
      , "gc_fragmentation" .= metrics.gcFragmentation
      , "heap_prof_sample" .= metrics.heapProfSample
      , "capability_usage" .= metrics.capabilityUsage
      , "productivity" .= metrics.productivity
      , "internal_event_count" .= metrics.internalEventCount
      , "internal_exported_logs" .= metrics.internalExportedLogs
      , "internal_rejected_logs" .= metrics.internalRejectedLogs
      , "internal_exported_metrics" .= metrics.internalExportedMetrics
      , "internal_rejected_metrics" .= metrics.internalRejectedMetrics
      , "internal_exported_samples" .= metrics.internalExportedSamples
      , "internal_rejected_samples" .= metrics.internalRejectedSamples
      , "internal_exported_spans" .= metrics.internalExportedSpans
      , "internal_rejected_spans" .= metrics.internalRejectedSpans
      ]

{- |
The configuration options for the span processors.
-}

-- NOTE:
-- If you add a new trace, search for the string...
--
--   This should be kept in sync with the list of traces.
--
-- ...and update all the relevant locations.
data Traces = Traces
  { capabilityUsage :: Maybe CapabilityUsageSpan
  , threadState :: Maybe ThreadStateSpan
  }
  deriving (Lift, Show)

instance FromYAML Traces where
  parseYAML =
    -- NOTE: This should be kept in sync with the list of traces.
    YAML.withMap "Traces" $ \m ->
      Traces
        <$> m .:? "capability_usage"
        <*> m .:? "thread_state"

instance ToYAML Traces where
  toYAML traces =
    -- NOTE: This should be kept in sync with the list of traces.
    YAML.mapping
      [ "capability_usage" .= traces.capabilityUsage
      , "thread_state" .= traces.threadState
      ]

{- |
The configuration options for the profile processors.
-}
data Profiles = Profiles
  { callStackProfile :: Maybe CallStackProfile
  , costCentreStackProfile :: Maybe CostCentreStackProfile
  }
  deriving (Lift, Show)

instance FromYAML Profiles where
  parseYAML =
    -- NOTE: This should be kept in sync with the list of profiles.
    YAML.withMap "Profiles" $ \m ->
      Profiles
        <$> m .:? "call_stack_profile"
        <*> m .:? "cost_centre_stack_profile"

instance ToYAML Profiles where
  toYAML profiles =
    -- NOTE: This should be kept in sync with the list of profiles.
    YAML.mapping
      [ "call_stack_profile" .= profiles.callStackProfile
      , "cost_centre_stack_profile" .= profiles.costCentreStackProfile
      ]

-------------------------------------------------------------------------------
-- Logs
-------------------------------------------------------------------------------

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadLabelLogData`.
-}
data ThreadLabelLog = ThreadLabelLog
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML ThreadLabelLog where
  parseYAML = genericParseYAMLLogProcessorConfig "ThreadLabelLog" ThreadLabelLog

instance ToYAML ThreadLabelLog where
  toYAML = genericToYAMLLogProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Log.processUserMessageLog`.
-}
data UserMessageLog = UserMessageLog
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML UserMessageLog where
  parseYAML = genericParseYAMLLogProcessorConfig "UserMessageLog" UserMessageLog

instance ToYAML UserMessageLog where
  toYAML = genericToYAMLLogProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Log.processUserMarkerLog`.
-}
data UserMarkerLog = UserMarkerLog
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML UserMarkerLog where
  parseYAML = genericParseYAMLLogProcessorConfig "UserMarkerLog" UserMarkerLog

instance ToYAML UserMarkerLog where
  toYAML = genericToYAMLLogProcessorConfig

{- |
The configuration options for internal log messages.
-}
data InternalLogMessageLog = InternalLogMessageLog
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalLogMessageLog where
  parseYAML = genericParseYAMLLogProcessorConfig "InternalLogMessageLog" InternalLogMessageLog

instance ToYAML InternalLogMessageLog where
  toYAML = genericToYAMLLogProcessorConfig

-------------------------------------------------------------------------------
-- Metrics
-------------------------------------------------------------------------------

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapAllocatedData`.
-}
data HeapAllocatedMetric = HeapAllocatedMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML HeapAllocatedMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapAllocatedMetric" HeapAllocatedMetric

instance ToYAML HeapAllocatedMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapSizeData`.
-}
data HeapSizeMetric = HeapSizeMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML HeapSizeMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapSizeMetric" HeapSizeMetric

instance ToYAML HeapSizeMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processBlocksSizeData`.
-}
data BlocksSizeMetric = BlocksSizeMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML BlocksSizeMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "BlocksSizeMetric" BlocksSizeMetric

instance ToYAML BlocksSizeMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapLiveData`.
-}
data HeapLiveMetric = HeapLiveMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML HeapLiveMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapLiveMetric" HeapLiveMetric

instance ToYAML HeapLiveMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the @memCurrent@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemCurrentMetric = MemCurrentMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML MemCurrentMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "MemCurrentMetric" MemCurrentMetric

instance ToYAML MemCurrentMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the @memNeeded@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemNeededMetric = MemNeededMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML MemNeededMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "MemNeededMetric" MemNeededMetric

instance ToYAML MemNeededMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the @memReturned@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemReturnedMetric = MemReturnedMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML MemReturnedMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "MemReturnedMetric" MemReturnedMetric

instance ToYAML MemReturnedMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapProfSampleData`.
-}
data HeapProfSampleMetric = HeapProfSampleMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML HeapProfSampleMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapProfSampleMetric" HeapProfSampleMetric

instance ToYAML HeapProfSampleMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the @copied@ field for `GHC.Eventlog.Live.Machine.Analysis.Heap.processGcStats`.
-}
data GcCopiedMetric = GcCopiedMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML GcCopiedMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "GcCopiedMetric" GcCopiedMetric

instance ToYAML GcCopiedMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the @copied@ field for `GHC.Eventlog.Live.Machine.Analysis.Heap.processGcStats`.
-}
data GcSlopMetric = GcSlopMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML GcSlopMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "GcSlopMetric" GcSlopMetric

instance ToYAML GcSlopMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the @copied@ field for `GHC.Eventlog.Live.Machine.Analysis.Heap.processGcStats`.
-}
data GcFragmentationMetric = GcFragmentationMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML GcFragmentationMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "GcFragmentationMetric" GcFragmentationMetric

instance ToYAML GcFragmentationMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageDurationData`.
-}
data CapabilityUsageMetric = CapabilityUsageMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML CapabilityUsageMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "CapabilityUsageMetric" CapabilityUsageMetric

instance ToYAML CapabilityUsageMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processProductivityData`.
-}
data ProductivityMetric = ProductivityMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML ProductivityMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "ProductivityMetric" ProductivityMetric

instance ToYAML ProductivityMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of received events.
-}
data InternalEventCountMetric = InternalEventCountMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalEventCountMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalEventCountMetric" InternalEventCountMetric

instance ToYAML InternalEventCountMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of exported logs metric.
-}
data InternalExportedLogsMetric = InternalExportedLogsMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalExportedLogsMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalExportedLogsMetric" InternalExportedLogsMetric

instance ToYAML InternalExportedLogsMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of rejected logs.
-}
data InternalRejectedLogsMetric = InternalRejectedLogsMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalRejectedLogsMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalRejectedLogsMetric" InternalRejectedLogsMetric

instance ToYAML InternalRejectedLogsMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of exported metrics metric.
-}
data InternalExportedMetricsMetric = InternalExportedMetricsMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalExportedMetricsMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalExportedMetricsMetric" InternalExportedMetricsMetric

instance ToYAML InternalExportedMetricsMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of rejected metrics.
-}
data InternalRejectedMetricsMetric = InternalRejectedMetricsMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalRejectedMetricsMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalRejectedMetricsMetric" InternalRejectedMetricsMetric

instance ToYAML InternalRejectedMetricsMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of exported samples metric.
-}
data InternalExportedSamplesMetric = InternalExportedSamplesMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalExportedSamplesMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalExportedSamplesMetric" InternalExportedSamplesMetric

instance ToYAML InternalExportedSamplesMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of rejected samples.
-}
data InternalRejectedSamplesMetric = InternalRejectedSamplesMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalRejectedSamplesMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalRejectedSamplesMetric" InternalRejectedSamplesMetric

instance ToYAML InternalRejectedSamplesMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of exported spans metric.
-}
data InternalExportedSpansMetric = InternalExportedSpansMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalExportedSpansMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalExportedSpansMetric" InternalExportedSpansMetric

instance ToYAML InternalExportedSpansMetric where
  toYAML = genericToYAMLMetricProcessorConfig

{- |
The configuration options for the internal count of rejected spans.
-}
data InternalRejectedSpansMetric = InternalRejectedSpansMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML InternalRejectedSpansMetric where
  parseYAML = genericParseYAMLMetricProcessorConfig "InternalRejectedSpansMetric" InternalRejectedSpansMetric

instance ToYAML InternalRejectedSpansMetric where
  toYAML = genericToYAMLMetricProcessorConfig

-------------------------------------------------------------------------------
-- Traces
-------------------------------------------------------------------------------

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageTraces`.
-}
data CapabilityUsageSpan = CapabilityUsageSpan
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML CapabilityUsageSpan where
  parseYAML = genericParseYAMLTraceProcessorConfig "CapabilityUsageSpan" CapabilityUsageSpan

instance ToYAML CapabilityUsageSpan where
  toYAML = genericToYAMLTraceProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadStateSpan`.
-}
data ThreadStateSpan = ThreadStateSpan
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML ThreadStateSpan where
  parseYAML = genericParseYAMLTraceProcessorConfig "ThreadStateSpan" ThreadStateSpan

instance ToYAML ThreadStateSpan where
  toYAML = genericToYAMLTraceProcessorConfig

-------------------------------------------------------------------------------
-- Profiles
-------------------------------------------------------------------------------

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Profile.processStackProfSampleData`.
-}
data CallStackProfile = CallStackProfile
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML CallStackProfile where
  parseYAML = genericParseYAMLProfilerProcessorConfig "CallStackProfile" CallStackProfile

instance ToYAML CallStackProfile where
  toYAML = genericToYAMLProfilerProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Profile.processCostCentreProfSampleData`.
-}
data CostCentreStackProfile = CostCentreStackProfile
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYAML CostCentreStackProfile where
  parseYAML = genericParseYAMLProfilerProcessorConfig "CostCentreStackProfile" CostCentreStackProfile

instance ToYAML CostCentreStackProfile where
  toYAML = genericToYAMLProfilerProcessorConfig

-------------------------------------------------------------------------------
-- Configuration supertypes
-------------------------------------------------------------------------------

{- |
The structural type of processor configurations.
-}
type IsProcessorConfig :: Type -> Constraint
type IsProcessorConfig config =
  ( HasField "name" config (Maybe Text)
  , HasField "description" config (Maybe Text)
  , HasField "export" config (Maybe ExportStrategy)
  )

{- |
The structural type of log processor configurations.
-}
type IsLogProcessorConfig :: Type -> Constraint
type IsLogProcessorConfig config =
  (IsProcessorConfig config)

{- |
The structural type of metric processor configurations.
-}
type IsMetricProcessorConfig :: Type -> Constraint
type IsMetricProcessorConfig config =
  ( IsProcessorConfig config
  , HasField "aggregate" config (Maybe AggregationStrategy)
  )

{- |
The structural type of span processor configurations.
-}
type IsTraceProcessorConfig :: Type -> Constraint
type IsTraceProcessorConfig config =
  (IsProcessorConfig config)

{- |
The structural type of span processor configurations.
-}
type IsProfileProcessorConfig :: Type -> Constraint
type IsProfileProcessorConfig config =
  (IsProcessorConfig config)

--------------------------------------------------------------------------------
-- Duration
--------------------------------------------------------------------------------

data Duration
  = DurationByBatches {batches :: !Int}
  | DurationBySeconds {seconds :: !Double}
  deriving (Lift, Show)

{- |
Internal helper.
A `ReadP` style parser for `AggregationStrategy`.
-}
readPDuration :: ReadP Duration
readPDuration = do
  -- Parse the number
  integerPart <- P.munch1 isDigit
  maybeFractionPart <- P.option Nothing (Just <$ P.char '.' <*> P.munch1 isDigit)

  -- Make a duration by batches
  let byBatches =
        case maybeFractionPart of
          Nothing ->
            case readEither integerPart of
              Left errorMsg -> fail $ "Could not parse duration: " <> errorMsg
              Right batches -> pure DurationByBatches{..}
          Just fractionPart -> fail $ "Fractional batches are unsupported; found " <> integerPart <> "." <> fractionPart <> "x"

  -- Make a duration by seconds
  let bySeconds =
        case readEither $ integerPart <> maybe "" ('.' :) maybeFractionPart of
          Left errorMsg -> fail $ "Could not parse duration: " <> errorMsg
          Right seconds -> pure DurationBySeconds{..}

  -- Parse the unit
  asum
    [ P.char 'x' >> byBatches
    , P.char 's' >> bySeconds
    ]

{- |
Internal helper.
Pretty-print a duration.
-}
prettyDuration :: Duration -> String
prettyDuration = \case
  DurationByBatches{..} -> show batches <> "x"
  DurationBySeconds{..} -> show seconds <> "s"

--------------------------------------------------------------------------------
-- Aggregation Strategy
--------------------------------------------------------------------------------

{- |
The options for metric aggregation strategies.
-}
data AggregationStrategy
  = AggregationStrategyBool {isOn :: !Bool}
  | AggregationStrategyDuration {duration :: !Duration}
  deriving (Lift, Show)

{- |
Convert an `AggregationStrategy` to a number of seconds, if specified in seconds.
-}
toAggregationSeconds :: AggregationStrategy -> Maybe Double
toAggregationSeconds aggregationStrategy
  | AggregationStrategyDuration DurationBySeconds{..} <- aggregationStrategy = Just seconds
  | otherwise = Nothing

instance FromYAML AggregationStrategy where
  parseYAML node = YAML.withScalar "AggregationStrategy" parseYAMLScalar node
   where
    parseYAMLScalar = \case
      YAML.SBool isOn -> pure AggregationStrategyBool{..}
      YAML.SStr str
        | [(duration, "")] <- P.readP_to_S readPDuration (T.unpack str) ->
            pure AggregationStrategyDuration{..}
      _otherwise -> YAML.typeMismatch "AggregationStrategy" node

instance ToYAML AggregationStrategy where
  toYAML = \case
    AggregationStrategyBool{..} ->
      YAML.Scalar () (YAML.SBool isOn)
    AggregationStrategyDuration{..} ->
      YAML.Scalar () (YAML.SStr . T.pack . prettyDuration $ duration)

--------------------------------------------------------------------------------
-- Export Strategy
--------------------------------------------------------------------------------

{- |
The options for export strategies.
-}
data ExportStrategy
  = ExportStrategyBool {isOn :: !Bool}
  | ExportStrategyDuration {duration :: !Duration}
  deriving (Lift, Show)

{- |
Check whether or not a processor is enabled based on its export strategy.
-}
isEnabled ::
  forall processorConfig.
  (HasField "export" processorConfig (Maybe ExportStrategy)) =>
  Maybe processorConfig -> Bool
isEnabled =
  maybe False (isEnabledByExportStrategy . (.export))
 where
  isEnabledByExportStrategy :: Maybe ExportStrategy -> Bool
  isEnabledByExportStrategy = \case
    Nothing -> True
    Just ExportStrategyBool{..} -> isOn
    Just ExportStrategyDuration{..} ->
      case duration of
        DurationByBatches{..} -> batches > 0
        DurationBySeconds{..} -> seconds > 0

{- |
Convert an `ExportStrategy` to a number of seconds, if specified in seconds.
-}
toExportSeconds :: ExportStrategy -> Maybe Double
toExportSeconds exportStrategy
  | ExportStrategyDuration DurationBySeconds{..} <- exportStrategy = Just seconds
  | otherwise = Nothing

instance FromYAML ExportStrategy where
  parseYAML node = YAML.withScalar "ExportStrategy" parseYAMLScalar node
   where
    parseYAMLScalar = \case
      YAML.SBool isOn -> pure ExportStrategyBool{..}
      YAML.SStr str
        | [(duration, "")] <- P.readP_to_S readPDuration (T.unpack str) ->
            pure ExportStrategyDuration{..}
      _otherwise -> YAML.typeMismatch "ExportStrategy" node

instance ToYAML ExportStrategy where
  toYAML = \case
    ExportStrategyBool{..} ->
      YAML.Scalar () (YAML.SBool isOn)
    ExportStrategyDuration{..} ->
      YAML.Scalar () (YAML.SStr . T.pack . prettyDuration $ duration)

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper.
Generic parser for log processor configuration.
-}
genericParseYAMLLogProcessorConfig ::
  String ->
  (Maybe Text -> Maybe Text -> Maybe ExportStrategy -> logProcessorConfig) ->
  YAML.Node YAML.Pos ->
  YAML.Parser logProcessorConfig
genericParseYAMLLogProcessorConfig log_ mkLogProcessorConfig =
  YAML.withMap log_ $ \m ->
    mkLogProcessorConfig
      <$> m .:? "name"
      <*> m .:? "description"
      <*> m .:? "export"

{- |
Internal helper.
Generic conversion from log processor configuration to YAML mappings.
-}
genericToYAMLLogProcessorConfig ::
  (IsLogProcessorConfig logProcessorConfig) =>
  logProcessorConfig ->
  YAML.Node ()
genericToYAMLLogProcessorConfig logProcessorConfig =
  YAML.mapping
    [ "name" .= logProcessorConfig.name
    , "description" .= logProcessorConfig.description
    , "export" .= logProcessorConfig.export
    ]

{- |
Internal helper.
Generic parser for metric processor configuration.
-}
genericParseYAMLMetricProcessorConfig ::
  String ->
  (Maybe Text -> Maybe Text -> Maybe AggregationStrategy -> Maybe ExportStrategy -> metricProcessorConfig) ->
  YAML.Node YAML.Pos ->
  YAML.Parser metricProcessorConfig
genericParseYAMLMetricProcessorConfig metric mkMetricProcessorConfig =
  YAML.withMap metric $ \m ->
    mkMetricProcessorConfig
      <$> m .:? "name"
      <*> m .:? "description"
      <*> m .:? "aggregate"
      <*> m .:? "export"

{- |
Internal helper.
Generic conversion from metric processor configuration to YAML mappings.
-}
genericToYAMLMetricProcessorConfig ::
  (IsMetricProcessorConfig metricProcessorConfig) =>
  metricProcessorConfig ->
  YAML.Node ()
genericToYAMLMetricProcessorConfig metricProcessorConfig =
  YAML.mapping
    [ "name" .= metricProcessorConfig.name
    , "description" .= metricProcessorConfig.description
    , "aggregate" .= metricProcessorConfig.aggregate
    , "export" .= metricProcessorConfig.export
    ]

{- |
Internal helper.
Generic parser for trace processor configuration.
-}
genericParseYAMLTraceProcessorConfig ::
  String ->
  (Maybe Text -> Maybe Text -> Maybe ExportStrategy -> traceProcessorConfig) ->
  YAML.Node YAML.Pos ->
  YAML.Parser traceProcessorConfig
genericParseYAMLTraceProcessorConfig trace mkTraceProcessorConfig =
  YAML.withMap trace $ \m ->
    mkTraceProcessorConfig
      <$> m .:? "name"
      <*> m .:? "description"
      <*> m .:? "export"

{- |
Internal helper.
Generic conversion from trace processor configuration to YAML mappings.
-}
genericToYAMLTraceProcessorConfig ::
  (IsTraceProcessorConfig traceProcessorConfig) =>
  traceProcessorConfig ->
  YAML.Node ()
genericToYAMLTraceProcessorConfig traceProcessorConfig =
  YAML.mapping
    [ "name" .= traceProcessorConfig.name
    , "description" .= traceProcessorConfig.description
    , "export" .= traceProcessorConfig.export
    ]

{- |
Internal helper.
Generic parser for processor configuration.
-}
genericParseYAMLProfilerProcessorConfig ::
  String ->
  (Maybe Text -> Maybe Text -> Maybe ExportStrategy -> profilerConfig) ->
  YAML.Node YAML.Pos ->
  YAML.Parser profilerConfig
genericParseYAMLProfilerProcessorConfig trace mkProfilerConfig =
  YAML.withMap trace $ \m ->
    mkProfilerConfig
      <$> m .:? "name"
      <*> m .:? "description"
      <*> m .:? "export"

{- |
Internal helper.
Generic parser for metric configuration.
-}
genericToYAMLProfilerProcessorConfig ::
  (IsTraceProcessorConfig profilerConfig) =>
  profilerConfig ->
  YAML.Node ()
genericToYAMLProfilerProcessorConfig profilerConfig =
  YAML.mapping
    [ "name" .= profilerConfig.name
    , "description" .= profilerConfig.description
    , "export" .= profilerConfig.export
    ]
