{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Config.Types
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config.Types (
  -- * Configuration type
  Config (..),

  -- ** Processor configuration types
  IsProcessorConfig,
  Processors (..),

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
  CapabilityUsageMetric (..),

  -- *** Trace processor configuration types
  Traces (..),
  IsTraceProcessorConfig,
  CapabilityUsageSpan (..),
  ThreadStateSpan (..),

  -- ** Property types
  AggregationStrategy (..),
  toAggregationBatches,
  ExportStrategy (..),
  toExportBatches,
) where

import Data.Char (isDigit)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.YAML (FromYAML (..), ToYAML, (.:?), (.=))
import Data.YAML qualified as YAML
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Language.Haskell.TH.Lift.Compat (Lift)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

{- |
The configuration for @eventlog-live-otelcol@.
-}
newtype Config = Config
  { processors :: Maybe Processors
  }
  deriving (Generic, Lift)

instance FromYAML Config where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser Config
  parseYAML = YAML.withMap "Config" $ \m ->
    Config
      <$> m .:? "processors"

instance ToYAML Config where
  toYAML :: Config -> YAML.Node ()
  toYAML config =
    YAML.mapping
      [ "processors" .= config.processors
      ]

{- |
The configuration options for the processors.
-}
data Processors = Processors
  { metrics :: Maybe Metrics
  , traces :: Maybe Traces
  }
  deriving (Generic, Lift)

instance FromYAML Processors where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser Processors
  parseYAML = YAML.withMap "Processors" $ \m ->
    Processors
      <$> m .:? "metrics"
      <*> m .:? "traces"

instance ToYAML Processors where
  toYAML :: Processors -> YAML.Node ()
  toYAML processors =
    YAML.mapping
      [ "metrics" .= processors.metrics
      , "traces" .= processors.traces
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
  , heapProfSample :: Maybe HeapProfSampleMetric
  , capabilityUsage :: Maybe CapabilityUsageMetric
  }
  deriving (Generic, Lift)

instance FromYAML Metrics where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser Metrics
  parseYAML =
    -- NOTE: This should be kept in sync with the list of metrics.
    YAML.withMap "Metrics" $ \m ->
      Metrics
        <$> m .:? "heap_allocated"
        <*> m .:? "blocks_size"
        <*> m .:? "heap_size"
        <*> m .:? "heap_live"
        <*> m .:? "mem_current"
        <*> m .:? "mem_needed"
        <*> m .:? "mem_returned"
        <*> m .:? "heap_prof_sample"
        <*> m .:? "capability_usage"

instance ToYAML Metrics where
  toYAML :: Metrics -> YAML.Node ()
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
      , "heap_prof_sample" .= metrics.heapProfSample
      , "capability_usage" .= metrics.capabilityUsage
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
  deriving (Generic, Lift)

instance FromYAML Traces where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser Traces
  parseYAML =
    -- NOTE: This should be kept in sync with the list of traces.
    YAML.withMap "Traces" $ \m ->
      Traces
        <$> m .:? "capability_usage"
        <*> m .:? "thread_state"

instance ToYAML Traces where
  toYAML :: Traces -> YAML.Node ()
  toYAML traces =
    -- NOTE: This should be kept in sync with the list of traces.
    YAML.mapping
      [ "capability_usage" .= traces.capabilityUsage
      , "thread_state" .= traces.threadState
      ]

{- |
The options for metric aggregation strategies.
-}
data AggregationStrategy
  = AggregationStrategyBool {isOn :: !Bool}
  | AggregationStrategyByBatches {byBatches :: !Int}
  deriving (Generic, Lift)

{- |
Convert an `ExportStrategy` to a number of batches.
-}
toAggregationBatches :: Maybe AggregationStrategy -> Int
toAggregationBatches = \case
  Nothing -> 0
  Just AggregationStrategyBool{..} -> if isOn then 1 else 0
  Just AggregationStrategyByBatches{..} -> byBatches

{- |
Internal helper.
A `ReadP` style parser for `AggregationStrategy`.
-}
readPAggregationStrategy :: ReadP AggregationStrategy
readPAggregationStrategy =
  AggregationStrategyByBatches . read <$> P.munch1 isDigit <* P.char 'x'

instance FromYAML AggregationStrategy where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser AggregationStrategy
  parseYAML node = YAML.withScalar "AggregationStrategy" parseYAMLScalar node
   where
    parseYAMLScalar = \case
      YAML.SBool isOn -> pure AggregationStrategyBool{..}
      YAML.SStr str
        | [(aggregationStrategy, "")] <- P.readP_to_S readPAggregationStrategy (T.unpack str) ->
            pure aggregationStrategy
      _otherwise -> YAML.typeMismatch "AggregationStrategy" node

instance ToYAML AggregationStrategy where
  toYAML :: AggregationStrategy -> YAML.Node ()
  toYAML = \case
    AggregationStrategyBool{..} ->
      YAML.Scalar () (YAML.SBool isOn)
    AggregationStrategyByBatches{..} ->
      YAML.Scalar () (YAML.SStr . T.pack $ show byBatches <> "x")

{- |
The options for export strategies.
-}
data ExportStrategy
  = ExportStrategyBool {isOn :: Bool}
  | ExportStrategyByBatches {byBatches :: Int}
  deriving (Generic, Lift)

{- |
Check whether or not a processor is enabled based on its export strategy.
-}
isEnabled :: Maybe ExportStrategy -> Bool
isEnabled = \case
  Nothing -> False
  Just ExportStrategyBool{..} -> isOn
  Just ExportStrategyByBatches{..} -> byBatches >= 1

{- |
Convert an `ExportStrategy` to a number of batches.
-}
toExportBatches :: Maybe ExportStrategy -> Int
toExportBatches = \case
  Nothing -> 0
  Just ExportStrategyBool{..} -> if isOn then 1 else 0
  Just ExportStrategyByBatches{..} -> byBatches

{- |
Internal helper.
A `ReadP` style parser for `ExportStrategy`.
-}
readPExportStrategy :: ReadP ExportStrategy
readPExportStrategy =
  ExportStrategyByBatches . read <$> P.munch1 isDigit <* P.char 'x'

instance FromYAML ExportStrategy where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser ExportStrategy
  parseYAML node = YAML.withScalar "ExportStrategy" parseYAMLScalar node
   where
    parseYAMLScalar = \case
      YAML.SBool isOn -> pure ExportStrategyBool{..}
      YAML.SStr str
        | [(exportStrategy, "")] <- P.readP_to_S readPExportStrategy (T.unpack str) ->
            pure exportStrategy
      _otherwise -> YAML.typeMismatch "ExportStrategy" node

instance ToYAML ExportStrategy where
  toYAML :: ExportStrategy -> YAML.Node ()
  toYAML = \case
    ExportStrategyBool{..} ->
      YAML.Scalar () (YAML.SBool isOn)
    ExportStrategyByBatches{..} ->
      YAML.Scalar () (YAML.SStr . T.pack $ show byBatches <> "x")

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapAllocatedData`.
-}
data HeapAllocatedMetric = HeapAllocatedMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML HeapAllocatedMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser HeapAllocatedMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapAllocatedMetric" HeapAllocatedMetric

instance ToYAML HeapAllocatedMetric where
  toYAML :: HeapAllocatedMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" HeapAllocatedMetric Bool where
  getField :: HeapAllocatedMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapSizeData`.
-}
data HeapSizeMetric = HeapSizeMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML HeapSizeMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser HeapSizeMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapSizeMetric" HeapSizeMetric

instance ToYAML HeapSizeMetric where
  toYAML :: HeapSizeMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" HeapSizeMetric Bool where
  getField :: HeapSizeMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processBlocksSizeData`.
-}
data BlocksSizeMetric = BlocksSizeMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML BlocksSizeMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser BlocksSizeMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "BlocksSizeMetric" BlocksSizeMetric

instance ToYAML BlocksSizeMetric where
  toYAML :: BlocksSizeMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" BlocksSizeMetric Bool where
  getField :: BlocksSizeMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapLiveData`.
-}
data HeapLiveMetric = HeapLiveMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML HeapLiveMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser HeapLiveMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapLiveMetric" HeapLiveMetric

instance ToYAML HeapLiveMetric where
  toYAML :: HeapLiveMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" HeapLiveMetric Bool where
  getField :: HeapLiveMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for the @memCurrent@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemCurrentMetric = MemCurrentMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML MemCurrentMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser MemCurrentMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "MemCurrentMetric" MemCurrentMetric

instance ToYAML MemCurrentMetric where
  toYAML :: MemCurrentMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" MemCurrentMetric Bool where
  getField :: MemCurrentMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for the @memNeeded@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemNeededMetric = MemNeededMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML MemNeededMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser MemNeededMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "MemNeededMetric" MemNeededMetric

instance ToYAML MemNeededMetric where
  toYAML :: MemNeededMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" MemNeededMetric Bool where
  getField :: MemNeededMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for the @memReturned@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemReturnedMetric = MemReturnedMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML MemReturnedMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser MemReturnedMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "MemReturnedMetric" MemReturnedMetric

instance ToYAML MemReturnedMetric where
  toYAML :: MemReturnedMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" MemReturnedMetric Bool where
  getField :: MemReturnedMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapProfSampleData`.
-}
data HeapProfSampleMetric = HeapProfSampleMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML HeapProfSampleMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser HeapProfSampleMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "HeapProfSampleMetric" HeapProfSampleMetric

instance ToYAML HeapProfSampleMetric where
  toYAML :: HeapProfSampleMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" HeapProfSampleMetric Bool where
  getField :: HeapProfSampleMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageMetrics`.
-}
data CapabilityUsageMetric = CapabilityUsageMetric
  { name :: Maybe Text
  , description :: Maybe Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML CapabilityUsageMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser CapabilityUsageMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "CapabilityUsageMetric" CapabilityUsageMetric

instance ToYAML CapabilityUsageMetric where
  toYAML :: CapabilityUsageMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" CapabilityUsageMetric Bool where
  getField :: CapabilityUsageMetric -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageTraces`.
-}
data CapabilityUsageSpan = CapabilityUsageSpan
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML CapabilityUsageSpan where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser CapabilityUsageSpan
  parseYAML = genericParseYAMLTraceProcessorConfig "CapabilityUsageSpan" CapabilityUsageSpan

instance ToYAML CapabilityUsageSpan where
  toYAML :: CapabilityUsageSpan -> YAML.Node ()
  toYAML = genericToYAMLTraceProcessorConfig

instance HasField "enabled" CapabilityUsageSpan Bool where
  getField :: CapabilityUsageSpan -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadStateTraces`.
-}
data ThreadStateSpan = ThreadStateSpan
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance FromYAML ThreadStateSpan where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser ThreadStateSpan
  parseYAML = genericParseYAMLTraceProcessorConfig "ThreadStateSpan" ThreadStateSpan

instance ToYAML ThreadStateSpan where
  toYAML :: ThreadStateSpan -> YAML.Node ()
  toYAML = genericToYAMLTraceProcessorConfig

instance HasField "enabled" ThreadStateSpan Bool where
  getField :: ThreadStateSpan -> Bool
  getField = isEnabled . (.export)

-------------------------------------------------------------------------------
-- Configuration types
-------------------------------------------------------------------------------

{- |
The structural type of processor configurations.
-}
type IsProcessorConfig :: Type -> Constraint
type IsProcessorConfig config =
  ( HasField "name" config (Maybe Text)
  , HasField "description" config (Maybe Text)
  , HasField "enabled" config Bool
  , HasField "export" config (Maybe ExportStrategy)
  )

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

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper.
Generic parser for metric configuration.
-}
genericParseYAMLMetricProcessorConfig ::
  String ->
  (Maybe Text -> Maybe Text -> Maybe AggregationStrategy -> Maybe ExportStrategy -> metricConfig) ->
  YAML.Node YAML.Pos ->
  YAML.Parser metricConfig
genericParseYAMLMetricProcessorConfig metric mkMetricConfig =
  YAML.withMap metric $ \m ->
    mkMetricConfig
      <$> m .:? "name"
      <*> m .:? "description"
      <*> m .:? "aggregate"
      <*> m .:? "export"

{- |
Internal helper.
Generic parser for metric configuration.
-}
genericToYAMLMetricProcessorConfig ::
  (IsMetricProcessorConfig metricConfig) =>
  metricConfig ->
  YAML.Node ()
genericToYAMLMetricProcessorConfig metricConfig =
  YAML.mapping
    [ "name" .= metricConfig.name
    , "description" .= metricConfig.description
    , "aggregate" .= metricConfig.aggregate
    , "export" .= metricConfig.export
    ]

{- |
Internal helper.
Generic parser for processor configuration.
-}
genericParseYAMLTraceProcessorConfig ::
  String ->
  (Maybe Text -> Maybe Text -> Maybe ExportStrategy -> traceConfig) ->
  YAML.Node YAML.Pos ->
  YAML.Parser traceConfig
genericParseYAMLTraceProcessorConfig trace mkTraceConfig =
  YAML.withMap trace $ \m ->
    mkTraceConfig
      <$> m .:? "name"
      <*> m .:? "description"
      <*> m .:? "export"

{- |
Internal helper.
Generic parser for metric configuration.
-}
genericToYAMLTraceProcessorConfig ::
  (IsTraceProcessorConfig traceConfig) =>
  traceConfig ->
  YAML.Node ()
genericToYAMLTraceProcessorConfig traceConfig =
  YAML.mapping
    [ "name" .= traceConfig.name
    , "description" .= traceConfig.description
    , "export" .= traceConfig.export
    ]
