{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Otelcol.Config
Description : The implementation of @eventlog-live-otelcol@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Config (
  readConfig,
  Config (..),
  Processors (..),
  Metrics (..),
  Spans (..),
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
  processorEnabled,
  processorDescription,
  processorName,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson.Types (Encoding, FromJSON (..), Options (..), Parser, SumEncoding (..), ToJSON (..), Value (..), camelTo2, defaultOptions, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Any (..), First (..))
import Data.Text (Text)
import Data.Yaml qualified as Y
import GHC.Generics (Generic)
import GHC.Records (HasField)

{- |
Read a `Config` from a configuration file.
-}
readConfig :: (MonadIO m) => FilePath -> m Config
readConfig = Y.decodeFileThrow

{- |
The configuration for @eventlog-live-otelcol@.
-}
newtype Config = Config
  { processors :: Maybe Processors
  }
  deriving (Generic)

instance FromJSON Config where
  parseJSON :: Value -> Parser Config
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Config where
  toJSON :: Config -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Config -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default Config where
  def :: Config
  def =
    Config
      { processors = Just def
      }

{- |
The configuration options for the processors.
-}
data Processors = Processors
  { metrics :: Maybe Metrics
  , spans :: Maybe Spans
  }
  deriving (Generic)

instance FromJSON Processors where
  parseJSON :: Value -> Parser Processors
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Processors where
  toJSON :: Processors -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Processors -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default Processors where
  def :: Processors
  def =
    Processors
      { metrics = Just def
      , spans = Just def
      }

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
  deriving (Generic)

instance FromJSON Metrics where
  parseJSON :: Value -> Parser Metrics
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Metrics where
  toJSON :: Metrics -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Metrics -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default Metrics where
  def :: Metrics
  def =
    Metrics
      { heapAllocated = Just def
      , blocksSize = Just def
      , heapSize = Just def
      , heapLive = Just def
      , memCurrent = Just def
      , memNeeded = Just def
      , memReturned = Just def
      , heapProfSample = Just def
      , capabilityUsage = Just def
      }

{- |
The configuration options for the span processors.
-}
data Spans = Spans
  { capabilityUsage :: Maybe CapabilityUsageSpan
  , threadState :: Maybe ThreadStateSpan
  }
  deriving (Generic)

instance FromJSON Spans where
  parseJSON :: Value -> Parser Spans
  parseJSON = genericParseJSON encodingOptions

instance ToJSON Spans where
  toJSON :: Spans -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: Spans -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default Spans where
  def :: Spans
  def =
    Spans
      { capabilityUsage = Just def
      , threadState = Just def
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapAllocatedData`.
-}
data HeapAllocatedMetric = HeapAllocatedMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON HeapAllocatedMetric where
  parseJSON :: Value -> Parser HeapAllocatedMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapAllocatedMetric where
  toJSON :: HeapAllocatedMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapAllocatedMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default HeapAllocatedMetric where
  def :: HeapAllocatedMetric
  def =
    HeapAllocatedMetric
      { description = Just "The size of a newly allocated chunk of heap."
      , enabled = True
      , name = "ghc_eventlog_HeapAllocated"
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapSizeData`.
-}
data HeapSizeMetric = HeapSizeMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON HeapSizeMetric where
  parseJSON :: Value -> Parser HeapSizeMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapSizeMetric where
  toJSON :: HeapSizeMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapSizeMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default HeapSizeMetric where
  def :: HeapSizeMetric
  def =
    HeapSizeMetric
      { description = Just "The current heap size, calculated by the allocated number of megablocks."
      , enabled = True
      , name = "ghc_eventlog_HeapSize"
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processBlocksSizeData`.
-}
data BlocksSizeMetric = BlocksSizeMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON BlocksSizeMetric where
  parseJSON :: Value -> Parser BlocksSizeMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON BlocksSizeMetric where
  toJSON :: BlocksSizeMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: BlocksSizeMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default BlocksSizeMetric where
  def :: BlocksSizeMetric
  def =
    BlocksSizeMetric
      { description = Just "The current heap size, calculated by the allocated number of blocks."
      , enabled = True
      , name = "ghc_eventlog_BlocksSize"
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapLiveData`.
-}
data HeapLiveMetric = HeapLiveMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON HeapLiveMetric where
  parseJSON :: Value -> Parser HeapLiveMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapLiveMetric where
  toJSON :: HeapLiveMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapLiveMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default HeapLiveMetric where
  def :: HeapLiveMetric
  def =
    HeapLiveMetric
      { description = Just "The current heap size, calculated by the allocated number of megablocks."
      , enabled = True
      , name = "ghc_eventlog_HeapLive"
      }

{- |
The configuration options for the @memCurrent@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemCurrentMetric = MemCurrentMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON MemCurrentMetric where
  parseJSON :: Value -> Parser MemCurrentMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON MemCurrentMetric where
  toJSON :: MemCurrentMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: MemCurrentMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default MemCurrentMetric where
  def :: MemCurrentMetric
  def =
    MemCurrentMetric
      { description = Just "The number of megablocks currently allocated."
      , enabled = True
      , name = "ghc_eventlog_MemCurrent"
      }

{- |
The configuration options for the @memNeeded@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemNeededMetric = MemNeededMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON MemNeededMetric where
  parseJSON :: Value -> Parser MemNeededMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON MemNeededMetric where
  toJSON :: MemNeededMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: MemNeededMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default MemNeededMetric where
  def :: MemNeededMetric
  def =
    MemNeededMetric
      { description = Just "The number of megablocks currently needed."
      , enabled = True
      , name = "ghc_eventlog_MemNeeded"
      }

{- |
The configuration options for the @memReturned@ field `GHC.Eventlog.Live.Machine.Analysis.Heap.processMemReturnData`.
-}
data MemReturnedMetric = MemReturnedMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON MemReturnedMetric where
  parseJSON :: Value -> Parser MemReturnedMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON MemReturnedMetric where
  toJSON :: MemReturnedMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: MemReturnedMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default MemReturnedMetric where
  def :: MemReturnedMetric
  def =
    MemReturnedMetric
      { description = Just "The number of megablocks currently being returned to the OS."
      , enabled = True
      , name = "ghc_eventlog_MemReturned"
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Heap.processHeapProfSampleData`.
-}
data HeapProfSampleMetric = HeapProfSampleMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON HeapProfSampleMetric where
  parseJSON :: Value -> Parser HeapProfSampleMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON HeapProfSampleMetric where
  toJSON :: HeapProfSampleMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: HeapProfSampleMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default HeapProfSampleMetric where
  def :: HeapProfSampleMetric
  def =
    HeapProfSampleMetric
      { description = Just "A heap profile sample."
      , enabled = True
      , name = "ghc_eventlog_HeapProfSample"
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageMetrics`.
-}
data CapabilityUsageMetric = CapabilityUsageMetric
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON CapabilityUsageMetric where
  parseJSON :: Value -> Parser CapabilityUsageMetric
  parseJSON = genericParseJSON encodingOptions

instance ToJSON CapabilityUsageMetric where
  toJSON :: CapabilityUsageMetric -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: CapabilityUsageMetric -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default CapabilityUsageMetric where
  def :: CapabilityUsageMetric
  def =
    CapabilityUsageMetric
      { description = Just "The duration of each capability usage span."
      , enabled = True
      , name = "ghc_eventlog_CapabilityUsageDuration"
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Capability.processCapabilityUsageSpans`.
-}
data CapabilityUsageSpan = CapabilityUsageSpan
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON CapabilityUsageSpan where
  parseJSON :: Value -> Parser CapabilityUsageSpan
  parseJSON = genericParseJSON encodingOptions

instance ToJSON CapabilityUsageSpan where
  toJSON :: CapabilityUsageSpan -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: CapabilityUsageSpan -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default CapabilityUsageSpan where
  def :: CapabilityUsageSpan
  def =
    CapabilityUsageSpan
      { description = Just "A span of capability usage (either by a mutator thread or the garbage collector)."
      , enabled = True
      , name = "ghc_eventlog_CapabilityUsage"
      }

{- |
The configuration options for `GHC.Eventlog.Live.Machine.Analysis.Thread.processThreadStateSpans`.
-}
data ThreadStateSpan = ThreadStateSpan
  { description :: Maybe Text
  , enabled :: Bool
  , name :: Text
  }
  deriving (Generic)

instance FromJSON ThreadStateSpan where
  parseJSON :: Value -> Parser ThreadStateSpan
  parseJSON = genericParseJSON encodingOptions

instance ToJSON ThreadStateSpan where
  toJSON :: ThreadStateSpan -> Value
  toJSON = genericToJSON encodingOptions
  toEncoding :: ThreadStateSpan -> Encoding
  toEncoding = genericToEncoding encodingOptions

instance Default ThreadStateSpan where
  def :: ThreadStateSpan
  def =
    ThreadStateSpan
      { description = Just "A span of thread state changes (either running or stopped)."
      , enabled = True
      , name = "ghc_eventlog_ThreadState"
      }

-------------------------------------------------------------------------------
-- Accessors
-------------------------------------------------------------------------------

{- |
Get whether or not a processor is enabled.
-}
processorEnabled :: (HasField "enabled" b Bool) => (Processors -> Maybe a) -> (a -> Maybe b) -> Config -> Bool
processorEnabled group field = getAny . with (.processors) (with group (with field (Any . (.enabled))))

{- |
Get the description corresponding to a processor.
-}
processorDescription :: (Default b, HasField "description" b (Maybe Text)) => (Processors -> Maybe a) -> (a -> Maybe b) -> Config -> Maybe Text
processorDescription group field = (.description) . fromMaybe def . getFirst . with (.processors) (with group (First . field))

{- |
Get the name corresponding to a processor.
-}
processorName :: (Default b, HasField "name" b Text) => (Processors -> Maybe a) -> (a -> Maybe b) -> Config -> Text
processorName group field = (.name) . fromMaybe def . getFirst . with (.processors) (with group (First . field))

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
-}
with :: (Foldable f, Monoid r) => (s -> f t) -> (t -> r) -> s -> r
with = flip ((.) . foldMap)
