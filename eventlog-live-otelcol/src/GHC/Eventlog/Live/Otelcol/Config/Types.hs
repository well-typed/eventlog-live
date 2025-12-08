{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Config.Types
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config.Types (
  -- * Configuration type
  Config (..),
  FullConfig (..),

  -- ** Processor configuration types
  Processors (..),
  IsProcessorConfig,

  -- *** Log processor configuration types
  Logs (..),
  IsLogProcessorConfig,
  ThreadLabel (..),
  UserMarker (..),
  UserMessage (..),
  InternalLogMessage (..),

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
The configuration for @eventlog-live-otelcol@.
-}
newtype Config = Config
  { processors :: Maybe Processors
  }
  deriving (Lift)

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
  { logs :: Maybe Logs
  , metrics :: Maybe Metrics
  , traces :: Maybe Traces
  }
  deriving (Lift)

instance FromYAML Processors where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser Processors
  parseYAML = YAML.withMap "Processors" $ \m ->
    Processors
      <$> m .:? "logs"
      <*> m .:? "metrics"
      <*> m .:? "traces"

instance ToYAML Processors where
  toYAML :: Processors -> YAML.Node ()
  toYAML processors =
    YAML.mapping
      [ "logs" .= processors.logs
      , "metrics" .= processors.metrics
      , "traces" .= processors.traces
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
  { threadLabel :: Maybe ThreadLabel
  , userMarker :: Maybe UserMarker
  , userMessage :: Maybe UserMessage
  , internalLogMessage :: Maybe InternalLogMessage
  }
  deriving (Lift)

instance FromYAML Logs where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser Logs
  parseYAML =
    -- NOTE: This should be kept in sync with the list of logs.
    YAML.withMap "Logs" $ \m ->
      Logs
        <$> m .:? "thread_label"
        <*> m .:? "user_marker"
        <*> m .:? "user_message"
        <*> m .:? "internal_log_message"

instance ToYAML Logs where
  toYAML :: Logs -> YAML.Node ()
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
  , heapProfSample :: Maybe HeapProfSampleMetric
  , capabilityUsage :: Maybe CapabilityUsageMetric
  }
  deriving (Lift)

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
  deriving (Lift)

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

-------------------------------------------------------------------------------
-- Logs
-------------------------------------------------------------------------------

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadLabelData`.
-}
data ThreadLabel = ThreadLabel
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift)

instance FromYAML ThreadLabel where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser ThreadLabel
  parseYAML = genericParseYAMLLogProcessorConfig "ThreadLabel" ThreadLabel

instance ToYAML ThreadLabel where
  toYAML :: ThreadLabel -> YAML.Node ()
  toYAML = genericToYAMLLogProcessorConfig

instance HasField "enabled" ThreadLabel Bool where
  getField :: ThreadLabel -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Log.processUserMessageData`.
-}
data UserMessage = UserMessage
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift)

instance FromYAML UserMessage where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser UserMessage
  parseYAML = genericParseYAMLLogProcessorConfig "UserMessage" UserMessage

instance ToYAML UserMessage where
  toYAML :: UserMessage -> YAML.Node ()
  toYAML = genericToYAMLLogProcessorConfig

instance HasField "enabled" UserMessage Bool where
  getField :: UserMessage -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Log.processUserMarkerData`.
-}
data UserMarker = UserMarker
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift)

instance FromYAML UserMarker where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser UserMarker
  parseYAML = genericParseYAMLLogProcessorConfig "UserMarker" UserMarker

instance ToYAML UserMarker where
  toYAML :: UserMarker -> YAML.Node ()
  toYAML = genericToYAMLLogProcessorConfig

instance HasField "enabled" UserMarker Bool where
  getField :: UserMarker -> Bool
  getField = isEnabled . (.export)

{- |
The configuration options for internal log messages.
-}
data InternalLogMessage = InternalLogMessage
  { name :: Maybe Text
  , description :: Maybe Text
  , export :: Maybe ExportStrategy
  }
  deriving (Lift)

instance FromYAML InternalLogMessage where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser InternalLogMessage
  parseYAML = genericParseYAMLLogProcessorConfig "InternalLogMessage" InternalLogMessage

instance ToYAML InternalLogMessage where
  toYAML :: InternalLogMessage -> YAML.Node ()
  toYAML = genericToYAMLLogProcessorConfig

instance HasField "enabled" InternalLogMessage Bool where
  getField :: InternalLogMessage -> Bool
  getField = isEnabled . (.export)

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
  deriving (Lift)

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
  deriving (Lift)

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
  deriving (Lift)

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
  deriving (Lift)

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
  deriving (Lift)

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
  deriving (Lift)

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
  deriving (Lift)

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
  deriving (Lift)

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
  deriving (Lift)

instance FromYAML CapabilityUsageMetric where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser CapabilityUsageMetric
  parseYAML = genericParseYAMLMetricProcessorConfig "CapabilityUsageMetric" CapabilityUsageMetric

instance ToYAML CapabilityUsageMetric where
  toYAML :: CapabilityUsageMetric -> YAML.Node ()
  toYAML = genericToYAMLMetricProcessorConfig

instance HasField "enabled" CapabilityUsageMetric Bool where
  getField :: CapabilityUsageMetric -> Bool
  getField = isEnabled . (.export)

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
  deriving (Lift)

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
  deriving (Lift)

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
-- Configuration supertypes
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

--------------------------------------------------------------------------------
-- Duration
--------------------------------------------------------------------------------

data Duration
  = DurationByBatches {batches :: !Int}
  | DurationBySeconds {seconds :: !Double}
  deriving (Lift)

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
  deriving (Lift)

{- |
Convert an `AggregationStrategy` to a number of seconds, if specified in seconds.
-}
toAggregationSeconds :: AggregationStrategy -> Maybe Double
toAggregationSeconds aggregationStrategy
  | AggregationStrategyDuration DurationBySeconds{..} <- aggregationStrategy = Just seconds
  | otherwise = Nothing

instance FromYAML AggregationStrategy where
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser AggregationStrategy
  parseYAML node = YAML.withScalar "AggregationStrategy" parseYAMLScalar node
   where
    parseYAMLScalar = \case
      YAML.SBool isOn -> pure AggregationStrategyBool{..}
      YAML.SStr str
        | [(duration, "")] <- P.readP_to_S readPDuration (T.unpack str) ->
            pure AggregationStrategyDuration{..}
      _otherwise -> YAML.typeMismatch "AggregationStrategy" node

instance ToYAML AggregationStrategy where
  toYAML :: AggregationStrategy -> YAML.Node ()
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
  deriving (Lift)

{- |
Check whether or not a processor is enabled based on its export strategy.
-}
isEnabled :: Maybe ExportStrategy -> Bool
isEnabled = \case
  Nothing -> False
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
  parseYAML :: YAML.Node YAML.Pos -> YAML.Parser ExportStrategy
  parseYAML node = YAML.withScalar "ExportStrategy" parseYAMLScalar node
   where
    parseYAMLScalar = \case
      YAML.SBool isOn -> pure ExportStrategyBool{..}
      YAML.SStr str
        | [(duration, "")] <- P.readP_to_S readPDuration (T.unpack str) ->
            pure ExportStrategyDuration{..}
      _otherwise -> YAML.typeMismatch "ExportStrategy" node

instance ToYAML ExportStrategy where
  toYAML :: ExportStrategy -> YAML.Node ()
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
genericParseYAMLLogProcessorConfig log mkLogProcessorConfig =
  YAML.withMap log $ \m ->
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
