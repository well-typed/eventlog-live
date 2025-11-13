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
  Traces (..),
  AggregationStrategy (..),
  toAggregationBatches,
  ExportStrategy (..),
  toExportBatches,
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

import Data.Aeson.Encoding qualified as AE
import Data.Aeson.Types (Encoding, FromJSON (..), Options (..), Parser, SumEncoding (..), ToJSON (..), Value (..), camelTo2, defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Aeson.Types qualified as AT
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
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
  , traces :: Maybe Traces
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
data Traces = Traces
  { capabilityUsage :: Maybe CapabilityUsageSpan
  , threadState :: Maybe ThreadStateSpan
  }
  deriving (Generic, Lift)

instance FromJSON Traces where
  parseJSON :: Value -> Parser Traces
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Traces where
  toJSON :: Traces -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Traces -> Encoding
  toEncoding = genericToEncoding encodingOptions

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

instance FromJSON AggregationStrategy where
  parseJSON :: Value -> Parser AggregationStrategy
  parseJSON = \case
    AT.Bool isOn ->
      pure $ AggregationStrategyBool isOn
    AT.String txt
      | [(aggregationStrategy, "")] <-
          P.readP_to_S readPAggregationStrategy (T.unpack txt) ->
          pure aggregationStrategy
    value -> AT.unexpected value

instance ToJSON AggregationStrategy where
  toJSON :: AggregationStrategy -> Value
  toJSON = \case
    AggregationStrategyBool{..} ->
      Bool isOn
    AggregationStrategyByBatches{..} ->
      String (T.pack (show byBatches <> "x"))
  toEncoding :: AggregationStrategy -> Encoding
  toEncoding = \case
    AggregationStrategyBool{..} ->
      AE.bool isOn
    AggregationStrategyByBatches{..} ->
      AE.text (T.pack (show byBatches <> "x"))

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

instance FromJSON ExportStrategy where
  parseJSON :: Value -> Parser ExportStrategy
  parseJSON = \case
    AT.Bool isOn ->
      pure $ ExportStrategyBool isOn
    AT.String txt
      | [(aggregationStrategy, "")] <-
          P.readP_to_S readPExportStrategy (T.unpack txt) ->
          pure aggregationStrategy
    value -> AT.unexpected value

instance ToJSON ExportStrategy where
  toJSON :: ExportStrategy -> Value
  toJSON = \case
    ExportStrategyBool{..} ->
      Bool isOn
    ExportStrategyByBatches{..} ->
      String (T.pack (show byBatches <> "x"))
  toEncoding :: ExportStrategy -> Encoding
  toEncoding = \case
    ExportStrategyBool{..} ->
      AE.bool isOn
    ExportStrategyByBatches{..} ->
      AE.text (T.pack (show byBatches <> "x"))

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapAllocatedData`.
-}
data HeapAllocatedMetric = HeapAllocatedMetric
  { description :: Maybe Text
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" HeapAllocatedMetric Bool where
  getField :: HeapAllocatedMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" HeapSizeMetric Bool where
  getField :: HeapSizeMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" BlocksSizeMetric Bool where
  getField :: BlocksSizeMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" HeapLiveMetric Bool where
  getField :: HeapLiveMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" MemCurrentMetric Bool where
  getField :: MemCurrentMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" MemNeededMetric Bool where
  getField :: MemNeededMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" MemReturnedMetric Bool where
  getField :: MemReturnedMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" HeapProfSampleMetric Bool where
  getField :: HeapProfSampleMetric -> Bool
  getField = isEnabled . (.export)

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
  , name :: Text
  , aggregate :: Maybe AggregationStrategy
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" CapabilityUsageMetric Bool where
  getField :: CapabilityUsageMetric -> Bool
  getField = isEnabled . (.export)

instance FromJSON CapabilityUsageMetric where
  parseJSON :: Value -> Parser CapabilityUsageMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON CapabilityUsageMetric where
  toJSON :: CapabilityUsageMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: CapabilityUsageMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageTraces`.
-}
data CapabilityUsageSpan = CapabilityUsageSpan
  { description :: Maybe Text
  , name :: Text
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" CapabilityUsageSpan Bool where
  getField :: CapabilityUsageSpan -> Bool
  getField = isEnabled . (.export)

instance FromJSON CapabilityUsageSpan where
  parseJSON :: Value -> Parser CapabilityUsageSpan
  parseJSON = genericParseJSON encodingOptions

instance ToJSON CapabilityUsageSpan where
  toJSON :: CapabilityUsageSpan -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: CapabilityUsageSpan -> Encoding
  toEncoding = genericToEncoding encodingOptions

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadStateTraces`.
-}
data ThreadStateSpan = ThreadStateSpan
  { description :: Maybe Text
  , name :: Text
  , export :: Maybe ExportStrategy
  }
  deriving (Generic, Lift)

instance HasField "enabled" ThreadStateSpan Bool where
  getField :: ThreadStateSpan -> Bool
  getField = isEnabled . (.export)

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
