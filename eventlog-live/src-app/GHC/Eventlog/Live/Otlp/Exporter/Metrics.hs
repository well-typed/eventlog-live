{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Otlp.Exporter.Metrics (
  -- * Export
  ExportMetricsResult (..),
  RejectedMetricsError (..),
  exportResourceMetrics,

  -- * Conversion
  toExportMetricsServiceRequest,
  toResourceMetrics,
  toScopeMetrics,
  toMetric,
) where

import Control.Exception (Exception (..), SomeException (..), catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Machine (ProcessT, await, construct, yield)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Eventlog.Live.Data.Metric (KnownMetricKind (..), KnownMetricType (..), Metric (..), SAggregationTemporality (..), SMetricKind (..), SMetricType (..), SMonotonicity (..))
import GHC.Eventlog.Live.Logger (Logger)
import GHC.Eventlog.Live.Machine.Core (Tick (..))
import GHC.Eventlog.Live.Otlp.Config (FullConfig)
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Exporter.Core (CanExportToConsole, CanExportToOltpViaHttpProtobuf (..), Exporter (..), export)
import GHC.Eventlog.Live.Otlp.Processor.Common.Core (ifNonEmpty, messageWith, toMaybeKeyValue)
import GHC.Eventlog.Live.Otlp.Processor.Common.Metrics (KnownMetric (..), SomeMetric (..), getConfig)
import GHC.IsList (IsList (..))
import Lens.Family2 ((.~), (^.))
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Message (..), Protobuf)
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as OMS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporters
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- OpenTelemetry Exporter Result for Metrics

data ExportMetricsResult
  = ExportMetricsResult
  { exportedDataPoints :: !Int64
  , rejectedDataPoints :: !Int64
  , maybeSomeException :: Maybe SomeException
  }
  deriving (Show)

pattern ExportMetricsSuccess :: Int64 -> ExportMetricsResult
pattern ExportMetricsSuccess exportedDataPoints =
  ExportMetricsResult exportedDataPoints 0 Nothing

pattern ExportMetricsError :: Int64 -> Int64 -> SomeException -> ExportMetricsResult
pattern ExportMetricsError exportedDataPoints rejectedDataPoints someException =
  ExportMetricsResult exportedDataPoints rejectedDataPoints (Just someException)

data RejectedMetricsError
  = RejectedMetricsError
  { rejectedDataPoints :: !Int64
  , errorMessage :: !Text
  }
  deriving (Show)

instance Exception RejectedMetricsError where
  displayException :: RejectedMetricsError -> String
  displayException RejectedMetricsError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedDataPoints errorMessage

--------------------------------------------------------------------------------
-- OpenTelemetry gRPC Exporter for Metrics

exportResourceMetrics ::
  Logger IO ->
  Exporter ->
  ProcessT IO (Tick OMS.ExportMetricsServiceRequest) (Tick ExportMetricsResult)
exportResourceMetrics logger exporter = construct $ go False
 where
  go exportedResourceMetrics =
    await >>= \case
      Tick -> do
        unless exportedResourceMetrics $
          yield (Item $ ExportMetricsSuccess 0)
        yield Tick
        go False
      Item exportMetricsServiceRequest -> do
        exportMetricsResult <- liftIO (sendResourceMetrics exportMetricsServiceRequest)
        yield (Item exportMetricsResult)
        go True

  sendResourceMetrics :: OMS.ExportMetricsServiceRequest -> IO ExportMetricsResult
  sendResourceMetrics exportMetricsServiceRequest =
    doExport `catch` handleSomeException
   where
    !exportedDataPoints = countDataPointsInExportMetricsServiceRequest exportMetricsServiceRequest

    doExport :: IO ExportMetricsResult
    doExport = do
      resp <- export @OMS.MetricsService @"export" logger exporter exportMetricsServiceRequest
      if resp ^. OMS.partialSuccess . OMS.rejectedDataPoints == 0
        then
          pure $ ExportMetricsSuccess exportedDataPoints
        else do
          let !rejectedDataPoints = resp ^. OMS.partialSuccess . OMS.rejectedDataPoints
          let !rejectedMetricsError = RejectedMetricsError{errorMessage = resp ^. OMS.partialSuccess . OMS.errorMessage, ..}
          pure $ ExportMetricsError exportedDataPoints rejectedDataPoints (SomeException rejectedMetricsError)

    handleSomeException :: SomeException -> IO ExportMetricsResult
    handleSomeException someException = pure $ ExportMetricsError 0 exportedDataPoints someException

--------------------------------------------------------------------------------
-- CanExportToConsole

instance CanExportToConsole OMS.MetricsService "export"

--------------------------------------------------------------------------------
-- CanExportToOltpViaGrpc

type instance G.RequestMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf OMS.MetricsService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- CanExportToOltpViaHttpProtobuf

instance CanExportToOltpViaHttpProtobuf OMS.MetricsService "export" where
  apiPath :: String
  apiPath = "/v1/metrics"

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OMS.ExportMetricsServiceRequest`.
-}
{-# SPECIALIZE countDataPointsInExportMetricsServiceRequest :: OMS.ExportMetricsServiceRequest -> Int64 #-}
{-# SPECIALIZE countDataPointsInExportMetricsServiceRequest :: OMS.ExportMetricsServiceRequest -> Word #-}
countDataPointsInExportMetricsServiceRequest :: (Integral i) => OMS.ExportMetricsServiceRequest -> i
countDataPointsInExportMetricsServiceRequest exportMetricsServiceRequest =
  getSum $ foldMap (Sum . countDataPointsInResourceMetrics) (exportMetricsServiceRequest ^. OMS.vec'resourceMetrics)

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OM.ResourceMetrics`.
-}
{-# SPECIALIZE countDataPointsInResourceMetrics :: OM.ResourceMetrics -> Int64 #-}
{-# SPECIALIZE countDataPointsInResourceMetrics :: OM.ResourceMetrics -> Word #-}
countDataPointsInResourceMetrics :: (Integral i) => OM.ResourceMetrics -> i
countDataPointsInResourceMetrics resourceMetrics =
  getSum $ foldMap (Sum . countDataPointsInScopeMetrics) (resourceMetrics ^. OM.vec'scopeMetrics)

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OM.ScopeMetrics`.
-}
{-# SPECIALIZE countDataPointsInScopeMetrics :: OM.ScopeMetrics -> Int64 #-}
{-# SPECIALIZE countDataPointsInScopeMetrics :: OM.ScopeMetrics -> Word #-}
countDataPointsInScopeMetrics :: (Integral i) => OM.ScopeMetrics -> i
countDataPointsInScopeMetrics scopeMetrics =
  getSum $ foldMap (Sum . countDataPointsInMetric) (scopeMetrics ^. OM.vec'metrics)

{- |
Internal helper.
Count the number of `OM.NumberDataPoint` values in an `OM.Metric`.
-}
{-# SPECIALIZE countDataPointsInMetric :: OM.Metric -> Int64 #-}
{-# SPECIALIZE countDataPointsInMetric :: OM.Metric -> Word #-}
countDataPointsInMetric :: (Integral i) => OM.Metric -> i
countDataPointsInMetric metric =
  fromIntegral $
    case metric ^. OM.maybe'data' of
      Nothing -> 0
      Just (OM.Metric'Gauge gauge) ->
        V.length (gauge ^. OM.vec'dataPoints)
      Just (OM.Metric'Sum sum_) ->
        V.length (sum_ ^. OM.vec'dataPoints)
      Just (OM.Metric'Histogram histogram) ->
        V.length (histogram ^. OM.vec'dataPoints)
      Just (OM.Metric'ExponentialHistogram exponentialHistogram) ->
        V.length (exponentialHistogram ^. OM.vec'dataPoints)
      Just (OM.Metric'Summary summary) ->
        V.length (summary ^. OM.vec'dataPoints)

--------------------------------------------------------------------------------
-- Conversion to OTLP
--------------------------------------------------------------------------------

toExportMetricsServiceRequest :: [OM.ResourceMetrics] -> OMS.ExportMetricsServiceRequest
toExportMetricsServiceRequest = (defMessage &) . (OM.resourceMetrics .~)
{-# INLINE toExportMetricsServiceRequest #-}

toResourceMetrics :: OR.Resource -> [OM.ScopeMetrics] -> Maybe OM.ResourceMetrics
toResourceMetrics resource scopeMetrics =
  ifNonEmpty scopeMetrics $
    messageWith [OM.resource .~ resource, OM.scopeMetrics .~ scopeMetrics]
{-# INLINE toResourceMetrics #-}

toScopeMetrics :: OC.InstrumentationScope -> [OM.Metric] -> Maybe OM.ScopeMetrics
toScopeMetrics instrumentationScope metrics =
  ifNonEmpty metrics $
    messageWith [OM.scope .~ instrumentationScope, OM.metrics .~ metrics]
{-# INLINE toScopeMetrics #-}

toMetric ::
  FullConfig ->
  SomeMetric ->
  Maybe OM.Metric
toMetric fullConfig (SomeMetric (metric :: Proxy metric) measurements) = do
  metricData <- toMetric'Data metric (metricToNumberDataPoint metric <$> measurements)
  pure $
    messageWith $
      [ OM.name .~ C.processorName (.metrics) (getConfig @metric) fullConfig
      , maybe id (OM.description .~) $ C.processorDescription (.metrics) (getConfig @metric) fullConfig
      , OM.maybe'data' .~ Just metricData
      ]
{-# INLINE toMetric #-}

toMetric'Data ::
  (KnownMetric metric) =>
  Proxy metric ->
  [OM.NumberDataPoint] ->
  Maybe OM.Metric'Data
toMetric'Data (_metric :: Proxy metric) dataPoints =
  ifNonEmpty dataPoints $
    case metricKindSing (Proxy @(KindOf metric)) of
      SGauge ->
        OM.Metric'Gauge . messageWith $
          [ OM.dataPoints .~ dataPoints
          ]
      SSum sAggregationTemporality sMonotonicity ->
        OM.Metric'Sum . messageWith $
          [ OM.dataPoints .~ dataPoints
          , OM.aggregationTemporality .~ toAggregationTemporality sAggregationTemporality
          , OM.isMonotonic .~ isMonotonic sMonotonicity
          ]
{-# INLINE toMetric'Data #-}

metricToNumberDataPoint ::
  (KnownMetric metric) =>
  Proxy metric ->
  Metric (TypeOf metric) ->
  OM.NumberDataPoint
metricToNumberDataPoint (metric :: Proxy metric) =
  metricTypeIsNumberDataPoint'Value metric toNumberDataPoint
{-# INLINE metricToNumberDataPoint #-}

toAggregationTemporality :: SAggregationTemporality aggregationTemporality -> OM.AggregationTemporality
toAggregationTemporality = \case
  SCumulative -> OM.AGGREGATION_TEMPORALITY_CUMULATIVE
  SDelta -> OM.AGGREGATION_TEMPORALITY_DELTA
{-# INLINE toAggregationTemporality #-}

isMonotonic :: SMonotonicity monotonicity -> Bool
isMonotonic = \case
  SMonotonic -> True
  SNonMonotonic -> False
{-# INLINE isMonotonic #-}

toNumberDataPoint :: (IsNumberDataPoint'Value v) => Metric v -> OM.NumberDataPoint
toNumberDataPoint i =
  messageWith
    [ OM.maybe'value .~ Just (toNumberDataPoint'Value i.value)
    , OM.timeUnixNano .~ fromMaybe 0 i.maybeTimeUnixNano
    , OM.startTimeUnixNano .~ fromMaybe 0 i.maybeStartTimeUnixNano
    , OM.attributes .~ mapMaybe toMaybeKeyValue (toList i.attrs)
    ]

{- |
Internal helper.

Every supported metric type has an instance of `IsNumberDataPoint'Value`.
-}
metricTypeIsNumberDataPoint'Value ::
  (KnownMetric metric) =>
  Proxy metric -> ((IsNumberDataPoint'Value (TypeOf metric)) => a) -> a
metricTypeIsNumberDataPoint'Value (_proxy :: Proxy metric) x =
  case metricTypeSing (Proxy :: Proxy (TypeOf metric)) of
    SMetricTypeFloat -> x
    SMetricTypeDouble -> x
    SMetricTypeWord -> x
    SMetricTypeWord8 -> x
    SMetricTypeWord16 -> x
    SMetricTypeWord32 -> x
    SMetricTypeWord64 -> x
    SMetricTypeInt -> x
    SMetricTypeInt8 -> x
    SMetricTypeInt16 -> x
    SMetricTypeInt32 -> x
    SMetricTypeInt64 -> x
{-# INLINE metricTypeIsNumberDataPoint'Value #-}

{- |
Internal helper.

Class of types that can be converted to `OM.NumberDataPoint'Value` values.
-}
class IsNumberDataPoint'Value v where
  toNumberDataPoint'Value :: v -> OM.NumberDataPoint'Value

instance IsNumberDataPoint'Value Float where
  toNumberDataPoint'Value :: Float -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsDouble . realToFrac
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Double where
  toNumberDataPoint'Value :: Double -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsDouble
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Word8 where
  toNumberDataPoint'Value :: Word8 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Word16 where
  toNumberDataPoint'Value :: Word16 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Word32 where
  toNumberDataPoint'Value :: Word32 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

-- | __Warning__: This instance may cause overflow.
instance IsNumberDataPoint'Value Word64 where
  toNumberDataPoint'Value :: Word64 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

-- | __Warning__: This instance may cause overflow.
instance IsNumberDataPoint'Value Word where
  toNumberDataPoint'Value :: Word -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Int8 where
  toNumberDataPoint'Value :: Int8 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Int16 where
  toNumberDataPoint'Value :: Int16 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Int32 where
  toNumberDataPoint'Value :: Int32 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Int64 where
  toNumberDataPoint'Value :: Int64 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt
  {-# INLINE toNumberDataPoint'Value #-}

instance IsNumberDataPoint'Value Int where
  toNumberDataPoint'Value :: Int -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
  {-# INLINE toNumberDataPoint'Value #-}
