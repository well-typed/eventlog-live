{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Config.Types
Description : The implementation of @eventlog-live@.
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
import GHC.Records (HasField (..))
import Language.Haskell.TH.Lift (Lift)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read (readEither)
import Yamlet (FromYaml (..), ToYaml (..))
import Yamlet qualified as Yaml

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
The configuration for @eventlog-live@.
-}
newtype Config = Config
  { processors :: Maybe Processors
  }
  deriving (Lift, Show)

instance FromYaml Config where
  parseYaml = Yaml.withMapping $ \o ->
    Config
      <$> Yaml.parseFieldMaybe o "processors"

instance ToYaml Config where
  toYaml config =
    Yaml.mapping
      [ "processors" Yaml..= config.processors
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

instance FromYaml Processors where
  parseYaml = Yaml.withMapping $ \o ->
    Processors
      <$> Yaml.parseFieldMaybe o "logs"
      <*> Yaml.parseFieldMaybe o "metrics"
      <*> Yaml.parseFieldMaybe o "traces"
      <*> Yaml.parseFieldMaybe o "profiles"

instance ToYaml Processors where
  toYaml processors =
    Yaml.mapping
      [ "logs" Yaml..= processors.logs
      , "metrics" Yaml..= processors.metrics
      , "traces" Yaml..= processors.traces
      , "profiles" Yaml..= processors.profiles
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

instance FromYaml Logs where
  parseYaml =
    -- NOTE: This should be kept in sync with the list of logs.
    Yaml.withMapping $ \o ->
      Logs
        <$> Yaml.parseFieldMaybe o "thread_label"
        <*> Yaml.parseFieldMaybe o "user_marker"
        <*> Yaml.parseFieldMaybe o "user_message"
        <*> Yaml.parseFieldMaybe o "internal_log_message"

instance ToYaml Logs where
  toYaml logs =
    -- NOTE: This should be kept in sync with the list of logs.
    Yaml.mapping
      [ "thread_label" Yaml..= logs.threadLabel
      , "user_marker" Yaml..= logs.userMarker
      , "user_message" Yaml..= logs.userMessage
      , "internal_log_message" Yaml..= logs.internalLogMessage
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

instance FromYaml Metrics where
  parseYaml =
    -- NOTE: This should be kept in sync with the list of metrics.
    Yaml.withMapping $ \o -> do
      heapAllocated <- Yaml.parseFieldMaybe o "heap_allocated"
      blocksSize <- Yaml.parseFieldMaybe o "blocks_size"
      heapSize <- Yaml.parseFieldMaybe o "heap_size"
      heapLive <- Yaml.parseFieldMaybe o "heap_live"
      memCurrent <- Yaml.parseFieldMaybe o "mem_current"
      memNeeded <- Yaml.parseFieldMaybe o "mem_needed"
      memReturned <- Yaml.parseFieldMaybe o "mem_returned"
      gcCopied <- Yaml.parseFieldMaybe o "gc_copied"
      gcSlop <- Yaml.parseFieldMaybe o "gc_slop"
      gcFragmentation <- Yaml.parseFieldMaybe o "gc_fragmentation"
      heapProfSample <- Yaml.parseFieldMaybe o "heap_prof_sample"
      capabilityUsage <- Yaml.parseFieldMaybe o "capability_usage"
      productivity <- Yaml.parseFieldMaybe o "productivity"
      internalEventCount <- Yaml.parseFieldMaybe o "internal_event_count"
      internalExportedLogs <- Yaml.parseFieldMaybe o "internal_exported_logs"
      internalRejectedLogs <- Yaml.parseFieldMaybe o "internal_rejected_logs"
      internalExportedMetrics <- Yaml.parseFieldMaybe o "internal_exported_metrics"
      internalRejectedMetrics <- Yaml.parseFieldMaybe o "internal_rejected_metrics"
      internalExportedSamples <- Yaml.parseFieldMaybe o "internal_exported_samples"
      internalRejectedSamples <- Yaml.parseFieldMaybe o "internal_rejected_samples"
      internalExportedSpans <- Yaml.parseFieldMaybe o "internal_exported_spans"
      internalRejectedSpans <- Yaml.parseFieldMaybe o "internal_rejected_spans"
      pure Metrics{..}

instance ToYaml Metrics where
  toYaml metrics =
    -- NOTE: This should be kept in sync with the list of metrics.
    Yaml.mapping
      [ "heap_allocated" Yaml..= metrics.heapAllocated
      , "blocks_size" Yaml..= metrics.blocksSize
      , "heap_size" Yaml..= metrics.heapSize
      , "heap_live" Yaml..= metrics.heapLive
      , "mem_current" Yaml..= metrics.memCurrent
      , "mem_needed" Yaml..= metrics.memNeeded
      , "mem_returned" Yaml..= metrics.memReturned
      , "gc_copied" Yaml..= metrics.gcCopied
      , "gc_slop" Yaml..= metrics.gcSlop
      , "gc_fragmentation" Yaml..= metrics.gcFragmentation
      , "heap_prof_sample" Yaml..= metrics.heapProfSample
      , "capability_usage" Yaml..= metrics.capabilityUsage
      , "productivity" Yaml..= metrics.productivity
      , "internal_event_count" Yaml..= metrics.internalEventCount
      , "internal_exported_logs" Yaml..= metrics.internalExportedLogs
      , "internal_rejected_logs" Yaml..= metrics.internalRejectedLogs
      , "internal_exported_metrics" Yaml..= metrics.internalExportedMetrics
      , "internal_rejected_metrics" Yaml..= metrics.internalRejectedMetrics
      , "internal_exported_samples" Yaml..= metrics.internalExportedSamples
      , "internal_rejected_samples" Yaml..= metrics.internalRejectedSamples
      , "internal_exported_spans" Yaml..= metrics.internalExportedSpans
      , "internal_rejected_spans" Yaml..= metrics.internalRejectedSpans
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

instance FromYaml Traces where
  parseYaml =
    -- NOTE: This should be kept in sync with the list of traces.
    Yaml.withMapping $ \o ->
      Traces
        <$> Yaml.parseFieldMaybe o "capability_usage"
        <*> Yaml.parseFieldMaybe o "thread_state"

instance ToYaml Traces where
  toYaml traces =
    -- NOTE: This should be kept in sync with the list of traces.
    Yaml.mapping
      [ "capability_usage" Yaml..= traces.capabilityUsage
      , "thread_state" Yaml..= traces.threadState
      ]

{- |
The configuration options for the profile processors.
-}
data Profiles = Profiles
  { callStackProfile :: Maybe CallStackProfile
  , costCentreStackProfile :: Maybe CostCentreStackProfile
  }
  deriving (Lift, Show)

instance FromYaml Profiles where
  parseYaml =
    -- NOTE: This should be kept in sync with the list of profiles.
    Yaml.withMapping $ \o ->
      Profiles
        <$> Yaml.parseFieldMaybe o "call_stack_profile"
        <*> Yaml.parseFieldMaybe o "cost_centre_stack_profile"

instance ToYaml Profiles where
  toYaml profiles =
    -- NOTE: This should be kept in sync with the list of profiles.
    Yaml.mapping
      [ "call_stack_profile" Yaml..= profiles.callStackProfile
      , "cost_centre_stack_profile" Yaml..= profiles.costCentreStackProfile
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

instance FromYaml ThreadLabelLog where
  parseYaml = genericParseYamlLogProcessorConfig ThreadLabelLog

instance ToYaml ThreadLabelLog where
  toYaml = genericToYamlLogProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Log.processUserMessageLog`.
-}
data UserMessageLog = UserMessageLog
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYaml UserMessageLog where
  parseYaml = genericParseYamlLogProcessorConfig UserMessageLog

instance ToYaml UserMessageLog where
  toYaml = genericToYamlLogProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Log.processUserMarkerLog`.
-}
data UserMarkerLog = UserMarkerLog
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYaml UserMarkerLog where
  parseYaml = genericParseYamlLogProcessorConfig UserMarkerLog

instance ToYaml UserMarkerLog where
  toYaml = genericToYamlLogProcessorConfig

{- |
The configuration options for internal log messages.
-}
data InternalLogMessageLog = InternalLogMessageLog
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYaml InternalLogMessageLog where
  parseYaml = genericParseYamlLogProcessorConfig InternalLogMessageLog

instance ToYaml InternalLogMessageLog where
  toYaml = genericToYamlLogProcessorConfig

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

instance FromYaml HeapAllocatedMetric where
  parseYaml = genericParseYamlMetricProcessorConfig HeapAllocatedMetric

instance ToYaml HeapAllocatedMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml HeapSizeMetric where
  parseYaml = genericParseYamlMetricProcessorConfig HeapSizeMetric

instance ToYaml HeapSizeMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml BlocksSizeMetric where
  parseYaml = genericParseYamlMetricProcessorConfig BlocksSizeMetric

instance ToYaml BlocksSizeMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml HeapLiveMetric where
  parseYaml = genericParseYamlMetricProcessorConfig HeapLiveMetric

instance ToYaml HeapLiveMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml MemCurrentMetric where
  parseYaml = genericParseYamlMetricProcessorConfig MemCurrentMetric

instance ToYaml MemCurrentMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml MemNeededMetric where
  parseYaml = genericParseYamlMetricProcessorConfig MemNeededMetric

instance ToYaml MemNeededMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml MemReturnedMetric where
  parseYaml = genericParseYamlMetricProcessorConfig MemReturnedMetric

instance ToYaml MemReturnedMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml HeapProfSampleMetric where
  parseYaml = genericParseYamlMetricProcessorConfig HeapProfSampleMetric

instance ToYaml HeapProfSampleMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml GcCopiedMetric where
  parseYaml = genericParseYamlMetricProcessorConfig GcCopiedMetric

instance ToYaml GcCopiedMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml GcSlopMetric where
  parseYaml = genericParseYamlMetricProcessorConfig GcSlopMetric

instance ToYaml GcSlopMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml GcFragmentationMetric where
  parseYaml = genericParseYamlMetricProcessorConfig GcFragmentationMetric

instance ToYaml GcFragmentationMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml CapabilityUsageMetric where
  parseYaml = genericParseYamlMetricProcessorConfig CapabilityUsageMetric

instance ToYaml CapabilityUsageMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml ProductivityMetric where
  parseYaml = genericParseYamlMetricProcessorConfig ProductivityMetric

instance ToYaml ProductivityMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalEventCountMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalEventCountMetric

instance ToYaml InternalEventCountMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalExportedLogsMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalExportedLogsMetric

instance ToYaml InternalExportedLogsMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalRejectedLogsMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalRejectedLogsMetric

instance ToYaml InternalRejectedLogsMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalExportedMetricsMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalExportedMetricsMetric

instance ToYaml InternalExportedMetricsMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalRejectedMetricsMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalRejectedMetricsMetric

instance ToYaml InternalRejectedMetricsMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalExportedSamplesMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalExportedSamplesMetric

instance ToYaml InternalExportedSamplesMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalRejectedSamplesMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalRejectedSamplesMetric

instance ToYaml InternalRejectedSamplesMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalExportedSpansMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalExportedSpansMetric

instance ToYaml InternalExportedSpansMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml InternalRejectedSpansMetric where
  parseYaml = genericParseYamlMetricProcessorConfig InternalRejectedSpansMetric

instance ToYaml InternalRejectedSpansMetric where
  toYaml = genericToYamlMetricProcessorConfig

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

instance FromYaml CapabilityUsageSpan where
  parseYaml = genericParseYamlTraceProcessorConfig CapabilityUsageSpan

instance ToYaml CapabilityUsageSpan where
  toYaml = genericToYamlTraceProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadStateSpan`.
-}
data ThreadStateSpan = ThreadStateSpan
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYaml ThreadStateSpan where
  parseYaml = genericParseYamlTraceProcessorConfig ThreadStateSpan

instance ToYaml ThreadStateSpan where
  toYaml = genericToYamlTraceProcessorConfig

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

instance FromYaml CallStackProfile where
  parseYaml = genericParseYamlProfilerProcessorConfig CallStackProfile

instance ToYaml CallStackProfile where
  toYaml = genericToYamlProfilerProcessorConfig

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Profile.processCostCentreProfSampleData`.
-}
data CostCentreStackProfile = CostCentreStackProfile
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift, Show)

instance FromYaml CostCentreStackProfile where
  parseYaml = genericParseYamlProfilerProcessorConfig CostCentreStackProfile

instance ToYaml CostCentreStackProfile where
  toYaml = genericToYamlProfilerProcessorConfig

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
A `ReadP` style parser for durations.
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
Parse a duration.
-}
parseDuration :: Text -> Maybe Duration
parseDuration text
  | [(duration, "")] <- P.readP_to_S readPDuration (T.unpack text) = Just duration
  | otherwise = Nothing

{- |
Internal helper.
Pretty-print a duration.
-}
prettyDuration :: Duration -> Text
prettyDuration = \case
  DurationByBatches{..} -> T.show batches <> "x"
  DurationBySeconds{..} -> T.show seconds <> "s"

instance FromYaml Duration where
  parseYaml =
    Yaml.withText $ \text ->
      maybe (fail $ "Invalid duration '" <> T.unpack text <> "'") pure $
        parseDuration text

instance ToYaml Duration where
  toYaml = toYaml . Yaml.String . prettyDuration

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

instance FromYaml AggregationStrategy where
  parseYaml node = pBool `Yaml.orElse` pDuration
   where
    pBool = Yaml.withBool (pure . AggregationStrategyBool) node
    pDuration = AggregationStrategyDuration <$> Yaml.parseYaml node

instance ToYaml AggregationStrategy where
  toYaml = \case
    AggregationStrategyBool{..} -> toYaml . Yaml.Bool $ isOn
    AggregationStrategyDuration{..} -> toYaml duration

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

instance FromYaml ExportStrategy where
  parseYaml node = pBool `Yaml.orElse` pDuration
   where
    pBool = Yaml.withBool (pure . ExportStrategyBool) node
    pDuration = ExportStrategyDuration <$> Yaml.parseYaml node

instance ToYaml ExportStrategy where
  toYaml = \case
    ExportStrategyBool{..} -> toYaml . Yaml.Bool $ isOn
    ExportStrategyDuration{..} -> toYaml duration

-------------------------------------------------------------------------------
-- Helpers for yamlet
-------------------------------------------------------------------------------

{- |
Internal helper.
Generic parser for log processor configuration.
-}
genericParseYamlLogProcessorConfig ::
  (Maybe Text -> Maybe Text -> Maybe ExportStrategy -> logProcessorConfig) ->
  Yaml.Node ->
  Yaml.Parser logProcessorConfig
genericParseYamlLogProcessorConfig mkLogProcessorConfig =
  Yaml.withMapping $ \o ->
    mkLogProcessorConfig
      <$> Yaml.parseFieldMaybe o "name"
      <*> Yaml.parseFieldMaybe o "description"
      <*> Yaml.parseFieldMaybe o "export"

{- |
Internal helper.
Generic conversion from log processor configuration to Yaml mappings.
-}
genericToYamlLogProcessorConfig ::
  (IsLogProcessorConfig logProcessorConfig) =>
  logProcessorConfig ->
  Yaml.Node
genericToYamlLogProcessorConfig logProcessorConfig =
  Yaml.mapping
    [ "name" Yaml..= logProcessorConfig.name
    , "description" Yaml..= logProcessorConfig.description
    , "export" Yaml..= logProcessorConfig.export
    ]

{- |
Internal helper.
Generic parser for metric processor configuration.
-}
genericParseYamlMetricProcessorConfig ::
  (Maybe Text -> Maybe Text -> Maybe AggregationStrategy -> Maybe ExportStrategy -> metricProcessorConfig) ->
  Yaml.Node ->
  Yaml.Parser metricProcessorConfig
genericParseYamlMetricProcessorConfig mkMetricProcessorConfig =
  Yaml.withMapping $ \o ->
    mkMetricProcessorConfig
      <$> Yaml.parseFieldMaybe o "name"
      <*> Yaml.parseFieldMaybe o "description"
      <*> Yaml.parseFieldMaybe o "aggregate"
      <*> Yaml.parseFieldMaybe o "export"

{- |
Internal helper.
Generic conversion from metric processor configuration to Yaml mappings.
-}
genericToYamlMetricProcessorConfig ::
  (IsMetricProcessorConfig metricProcessorConfig) =>
  metricProcessorConfig ->
  Yaml.Node
genericToYamlMetricProcessorConfig metricProcessorConfig =
  Yaml.mapping
    [ "name" Yaml..= metricProcessorConfig.name
    , "description" Yaml..= metricProcessorConfig.description
    , "aggregate" Yaml..= metricProcessorConfig.aggregate
    , "export" Yaml..= metricProcessorConfig.export
    ]

{- |
Internal helper.
Generic parser for trace processor configuration.
-}
genericParseYamlTraceProcessorConfig ::
  (Maybe Text -> Maybe Text -> Maybe ExportStrategy -> traceProcessorConfig) ->
  Yaml.Node ->
  Yaml.Parser traceProcessorConfig
genericParseYamlTraceProcessorConfig mkTraceProcessorConfig =
  Yaml.withMapping $ \o ->
    mkTraceProcessorConfig
      <$> Yaml.parseFieldMaybe o "name"
      <*> Yaml.parseFieldMaybe o "description"
      <*> Yaml.parseFieldMaybe o "export"

{- |
Internal helper.
Generic conversion from trace processor configuration to Yaml mappings.
-}
genericToYamlTraceProcessorConfig ::
  (IsTraceProcessorConfig traceProcessorConfig) =>
  traceProcessorConfig ->
  Yaml.Node
genericToYamlTraceProcessorConfig traceProcessorConfig =
  Yaml.mapping
    [ "name" Yaml..= traceProcessorConfig.name
    , "description" Yaml..= traceProcessorConfig.description
    , "export" Yaml..= traceProcessorConfig.export
    ]

{- |
Internal helper.
Generic parser for processor configuration.
-}
genericParseYamlProfilerProcessorConfig ::
  (Maybe Text -> Maybe Text -> Maybe ExportStrategy -> profilerConfig) ->
  Yaml.Node ->
  Yaml.Parser profilerConfig
genericParseYamlProfilerProcessorConfig mkProfilerConfig =
  Yaml.withMapping $ \o ->
    mkProfilerConfig
      <$> Yaml.parseFieldMaybe o "name"
      <*> Yaml.parseFieldMaybe o "description"
      <*> Yaml.parseFieldMaybe o "export"

{- |
Internal helper.
Generic parser for metric configuration.
-}
genericToYamlProfilerProcessorConfig ::
  (IsTraceProcessorConfig profilerConfig) =>
  profilerConfig ->
  Yaml.Node
genericToYamlProfilerProcessorConfig profilerConfig =
  Yaml.mapping
    [ "name" Yaml..= profilerConfig.name
    , "description" Yaml..= profilerConfig.description
    , "export" Yaml..= profilerConfig.export
    ]
