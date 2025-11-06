{- |
Module      : GHC.Eventlog.Live.Otelcol.Config.Types
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config.Types (
  Config (..),
  Processors (..),
  Metrics (..),
  Spans (..),
  AggregationStrategy (..),
  HeapAllocatedMetric (..),
  BlocksSizeMetric (..),
  HeapSizeMetric (..),
  HeapLiveMetric (..),
  MemCurrentMetric (..),
  MemNeededMetric (..),
  MemReturnedMetric (..),
  HeapProfSampleMetric (..),
  CapabilityUsageMetric (..),
  CapabilityUsageSpan (..),
  ThreadStateSpan (..),
) where

import Data.Aeson.Types (Encoding, FromJSON (..), Options (..), Parser, SumEncoding (..), ToJSON (..), Value (..), camelTo2, defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift.Compat (Lift)

{- |
The configuration for @eventlog-live-otelcol@.
-}
newtype Config = Config
  { processors :: Maybe Processors
  }
  deriving (Generic, Lift)

instance FromJSON Config where
  parseJSON :: Value -> Parser Config
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Config where
  toJSON :: Config -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Config -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for the processors.
-}
data Processors = Processors
  { metrics :: Maybe Metrics
  , spans :: Maybe Spans
  }
  deriving (Generic, Lift)

instance FromJSON Processors where
  parseJSON :: Value -> Parser Processors
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Processors where
  toJSON :: Processors -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Processors -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for the metric processors.
-}
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

instance FromJSON Metrics where
  parseJSON :: Value -> Parser Metrics
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Metrics where
  toJSON :: Metrics -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Metrics -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for the span processors.
-}
data Spans = Spans
  { capabilityUsage :: Maybe CapabilityUsageSpan
  , threadState :: Maybe ThreadStateSpan
  }
  deriving (Generic, Lift)

instance FromJSON Spans where
  parseJSON :: Value -> Parser Spans
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Spans where
  toJSON :: Spans -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Spans -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The options for metric aggregation.
-}
data AggregationStrategy
  = AggregationStrategyByBatch
  deriving (Generic, Lift)

instance FromJSON AggregationStrategy where
  parseJSON :: Value -> Parser AggregationStrategy
  parseJSON = genericParseJSON aggregationStrategyEncodingOptions

instance ToJSON AggregationStrategy where
  toJSON :: AggregationStrategy -> Value
  toJSON = genericToJSON aggregationStrategyEncodingOptions
  toEncoding :: AggregationStrategy -> Encoding
  toEncoding = genericToEncoding aggregationStrategyEncodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapAllocatedData`.
-}
data HeapAllocatedMetric = HeapAllocatedMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON HeapAllocatedMetric where
  parseJSON :: Value -> Parser HeapAllocatedMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapAllocatedMetric where
  toJSON :: HeapAllocatedMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapAllocatedMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapSizeData`.
-}
data HeapSizeMetric = HeapSizeMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON HeapSizeMetric where
  parseJSON :: Value -> Parser HeapSizeMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapSizeMetric where
  toJSON :: HeapSizeMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapSizeMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processBlocksSizeData`.
-}
data BlocksSizeMetric = BlocksSizeMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON BlocksSizeMetric where
  parseJSON :: Value -> Parser BlocksSizeMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON BlocksSizeMetric where
  toJSON :: BlocksSizeMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: BlocksSizeMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapLiveData`.
-}
data HeapLiveMetric = HeapLiveMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON HeapLiveMetric where
  parseJSON :: Value -> Parser HeapLiveMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapLiveMetric where
  toJSON :: HeapLiveMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapLiveMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for the @memCurrent@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemCurrentMetric = MemCurrentMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON MemCurrentMetric where
  parseJSON :: Value -> Parser MemCurrentMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON MemCurrentMetric where
  toJSON :: MemCurrentMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: MemCurrentMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for the @memNeeded@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemNeededMetric = MemNeededMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON MemNeededMetric where
  parseJSON :: Value -> Parser MemNeededMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON MemNeededMetric where
  toJSON :: MemNeededMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: MemNeededMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for the @memReturned@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemReturnedMetric = MemReturnedMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON MemReturnedMetric where
  parseJSON :: Value -> Parser MemReturnedMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON MemReturnedMetric where
  toJSON :: MemReturnedMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: MemReturnedMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapProfSampleData`.
-}
data HeapProfSampleMetric = HeapProfSampleMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON HeapProfSampleMetric where
  parseJSON :: Value -> Parser HeapProfSampleMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapProfSampleMetric where
  toJSON :: HeapProfSampleMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapProfSampleMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageMetrics`.
-}
data CapabilityUsageMetric = CapabilityUsageMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  }
  deriving (Generic, Lift)

instance FromJSON CapabilityUsageMetric where
  parseJSON :: Value -> Parser CapabilityUsageMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON CapabilityUsageMetric where
  toJSON :: CapabilityUsageMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: CapabilityUsageMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageSpans`.
-}
data CapabilityUsageSpan = CapabilityUsageSpan
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic, Lift)

instance FromJSON CapabilityUsageSpan where
  parseJSON :: Value -> Parser CapabilityUsageSpan
  parseJSON = genericParseJSON encodingOptions

instance ToJSON CapabilityUsageSpan where
  toJSON :: CapabilityUsageSpan -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: CapabilityUsageSpan -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadStateSpans`.
-}
data ThreadStateSpan = ThreadStateSpan
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic, Lift)

instance FromJSON ThreadStateSpan where
  parseJSON :: Value -> Parser ThreadStateSpan
  parseJSON = genericParseJSON encodingOptions

instance ToJSON ThreadStateSpan where
  toJSON :: ThreadStateSpan -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: ThreadStateSpan -> Encoding
  toEncoding = genericToEncoding encodingOptions

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper.
The encoding options that should be used by every `FromJSON` and `ToJSON` instance for the configuration.
-}
encodingOptions :: Options
encodingOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    , constructorTagModifier = camelTo2 '_'
    , allNullaryToStringTag = True
    , omitNothingFields = False
    , sumEncoding = UntaggedValue
    , tagSingleConstructors = False
    , unwrapUnaryRecords = False
    }

{- |
Internal helper.
The encoding options that should be used by the `FromJSON` and `ToJSON` instances for `AggregationStrategy`.
-}
aggregationStrategyEncodingOptions :: Options
aggregationStrategyEncodingOptions =
  encodingOptions
    { constructorTagModifier = camelTo2 '_' . stripAggregationStrategy
    , tagSingleConstructors = True
    }
 where
  stripAggregationStrategy :: String -> String
  stripAggregationStrategy = drop (length ("AggregationStrategy" :: String))
