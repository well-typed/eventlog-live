{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Common.Metrics
Description : Common utilities for metric processors.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Common.Metrics (
  MetricProcessor (..),
  runMetricProcessor,
  MetricAggregators (..),
  viaSum,
  viaLast,
  asGauge,
  asSum,
  toScopeMetrics,
  toResourceMetrics,
  toExportMetricsServiceRequest,
)
where

import Control.Monad (unless)
import Data.Coerce (Coercible, coerce)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Default (Default)
import Data.Function ((&))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Machine (Process, ProcessT, asParts, await, echo, mapping, repeatedly, yield, (~>))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.ProtoLens (Message (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Last (..), Sum (..))
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Eventlog.Live.Data.Group (Group, GroupBy, GroupedBy)
import GHC.Eventlog.Live.Data.Group qualified as DG
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Otelcol.Config qualified as C
import GHC.Eventlog.Live.Otelcol.Config.Types (FullConfig)
import GHC.Eventlog.Live.Otelcol.Processor.Common.Core (ifNonEmpty, messageWith, runIf, toMaybeKeyValue)
import GHC.IsList (IsList (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as OMS
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR

--------------------------------------------------------------------------------
-- Generic Metric Processor

type MetricProcessor :: Symbol -> Type -> (Type -> Type) -> Type -> Type -> Type -> Type -> Type
data MetricProcessor metricProcessor metricProcessorConfig m a b c d
  = ( Monad m
    , HasField metricProcessor C.Metrics (Maybe metricProcessorConfig)
    , C.IsMetricProcessorConfig metricProcessorConfig
    , IsNumberDataPoint'Value d
    ) =>
  MetricProcessor
  { metricProcessorProxy :: !(Proxy metricProcessor)
  -- ^ The metric's field name in `C.Metrics`.
  , dataProcessor :: !(ProcessT m a b)
  -- ^ The metric's data processor
  , aggregators :: !(MetricAggregators b c)
  -- ^ The metric's aggregator
  , postProcessor :: !(ProcessT m c (Metric d))
  -- ^ The metric's post processor
  , unit :: !Text
  -- ^ The metric's unit (in UCUM format).
  , asMetric'Data :: !(ProcessT m [OM.NumberDataPoint] OM.Metric'Data)
  -- ^ A process to wrap data points as `OM.Metric'Data`.
  }

{- |
Internal helper.
Run a `MetricProcessor`.
-}
runMetricProcessor ::
  forall metricProcessor metricProcessorConfig m a b c d.
  (Default metricProcessorConfig) =>
  MetricProcessor metricProcessor metricProcessorConfig m a b c d ->
  -- | The full configuration.
  FullConfig ->
  ProcessT m (Tick a) (Tick (DList OM.Metric))
runMetricProcessor MetricProcessor{..} fullConfig =
  let metricProcessorConfig :: C.Metrics -> Maybe metricProcessorConfig
      metricProcessorConfig = getField @metricProcessor
   in runIf (C.processorEnabled (.metrics) metricProcessorConfig fullConfig) $
        M.liftTick dataProcessor
          ~> aggregate aggregators (C.processorAggregationBatches (.metrics) metricProcessorConfig fullConfig)
          ~> M.liftTick postProcessor
          ~> mapping (fmap (D.singleton . toNumberDataPoint))
          ~> M.batchByTicks (C.processorExportBatches (.metrics) metricProcessorConfig fullConfig)
          ~> M.liftTick
            ( mapping D.toList
                ~> asMetric'Data
                ~> asMetricWith fullConfig metricProcessorConfig [OM.unit .~ unit]
                ~> mapping D.singleton
            )
{-# INLINE runMetricProcessor #-}

asMetricWith ::
  (Default a, HasField "description" a (Maybe Text), HasField "name" a (Maybe Text)) =>
  FullConfig ->
  (C.Metrics -> Maybe a) ->
  [OM.Metric -> OM.Metric] ->
  Process OM.Metric'Data OM.Metric
asMetricWith fullConfig field f =
  asMetric $
    [ OM.name .~ C.processorName (.metrics) field fullConfig
    , maybe id (OM.description .~) $ C.processorDescription (.metrics) field fullConfig
    ]
      <> f

asMetric :: [OM.Metric -> OM.Metric] -> Process OM.Metric'Data OM.Metric
asMetric f = mapping $ toMetric f

toMetric :: [OM.Metric -> OM.Metric] -> OM.Metric'Data -> OM.Metric
toMetric f metric'data = messageWith ((OM.maybe'data' .~ Just metric'data) : f)

toExportMetricsServiceRequest :: [OM.ResourceMetrics] -> OMS.ExportMetricsServiceRequest
toExportMetricsServiceRequest = (defMessage &) . (OM.resourceMetrics .~)

toResourceMetrics :: OR.Resource -> [OM.ScopeMetrics] -> Maybe OM.ResourceMetrics
toResourceMetrics resource scopeMetrics =
  ifNonEmpty scopeMetrics $
    messageWith [OM.resource .~ resource, OM.scopeMetrics .~ scopeMetrics]

toScopeMetrics :: OC.InstrumentationScope -> [OM.Metric] -> Maybe OM.ScopeMetrics
toScopeMetrics instrumentationScope metrics =
  ifNonEmpty metrics $
    messageWith [OM.scope .~ instrumentationScope, OM.metrics .~ metrics]

asGauge :: Process [OM.NumberDataPoint] OM.Metric'Data
asGauge =
  repeatedly $ do
    await >>= \dataPoints ->
      unless (null dataPoints) $
        yield (toGauge dataPoints)

toGauge :: [OM.NumberDataPoint] -> OM.Metric'Data
toGauge dataPoints = OM.Metric'Gauge . messageWith $ [OM.dataPoints .~ dataPoints]

asSum :: [OM.Sum -> OM.Sum] -> Process [OM.NumberDataPoint] OM.Metric'Data
asSum f =
  repeatedly $
    await >>= \dataPoints ->
      unless (null dataPoints) $
        yield (toSum f dataPoints)

toSum :: [OM.Sum -> OM.Sum] -> [OM.NumberDataPoint] -> OM.Metric'Data
toSum f dataPoints = OM.Metric'Sum . messageWith $ (OM.dataPoints .~ dataPoints) : f

--------------------------------------------------------------------------------
-- Metric Aggregation

data MetricAggregators a b = MetricAggregators
  { nothing :: Process (Tick a) (Tick b)
  , byBatches :: Int -> Process (Tick a) (Tick b)
  }

{- |
Internal helper.
Aggregate items based on the provided aggregators and aggregation strategy.
-}
aggregate :: MetricAggregators a b -> Int -> Process (Tick a) (Tick b)
aggregate MetricAggregators{..} aggregationBatches
  | aggregationBatches >= 1 = byBatches aggregationBatches
  | otherwise = nothing

{- |
Internal helper.
Metric aggregators via the `Semigroup` instance for `Sum`.
-}
viaSum :: forall a. (Num a) => MetricAggregators (Metric a) (Metric a)
viaSum =
  MetricAggregators
    { nothing = echo
    , byBatches = \ticks ->
        -- TODO: Yield group sample counts as separate metric.
        batchByTicksVia ticks (Proxy @(Metric (Sum a)))
          ~> M.liftTick (mapping (fmap (.representative)) ~> asParts)
    }

{- |
Internal helper.
Metric aggregators via the `Semigroup` instance for `Last`.
-}
viaLast :: forall a. (GroupBy a) => MetricAggregators a a
viaLast =
  MetricAggregators
    { nothing = echo
    , byBatches = \ticks ->
        -- TODO: Yield group sample counts as separate metric.
        batchByTicksVia ticks (Proxy @(Last a))
          ~> M.liftTick (mapping (fmap (.representative)) ~> asParts)
    }

{- |
Internal helper.
This function aggregates items via a `Semigroup` instance and grouped by the `GroupBy` instance.
-}
batchByTicksVia ::
  forall a b.
  (Coercible a b, GroupBy b, Semigroup b) =>
  -- | The number of ticks per batch.
  Int ->
  Proxy b ->
  Process (Tick a) (Tick [Group a])
batchByTicksVia ticks (Proxy :: Proxy b) =
  mapping (fmap DG.singleton . coerce @(Tick a) @(Tick b))
    ~> M.batchByTicks @(GroupedBy b) ticks
    ~> M.liftTick (mapping (coerce @[Group b] @[Group a] . DG.groups))

{- |
Internal helper.
Convert a metric datapoint to an `OM.NumberDataPoint`.
-}
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
Class of types that can be converted to `OM.NumberDataPoint'Value` values.
-}
class IsNumberDataPoint'Value v where
  toNumberDataPoint'Value :: v -> OM.NumberDataPoint'Value

instance IsNumberDataPoint'Value Float where
  toNumberDataPoint'Value :: Float -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsDouble . realToFrac

instance IsNumberDataPoint'Value Double where
  toNumberDataPoint'Value :: Double -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsDouble

instance IsNumberDataPoint'Value Word8 where
  toNumberDataPoint'Value :: Word8 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

instance IsNumberDataPoint'Value Word16 where
  toNumberDataPoint'Value :: Word16 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

instance IsNumberDataPoint'Value Word32 where
  toNumberDataPoint'Value :: Word32 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

-- | __Warning__: This instance may cause overflow.
instance IsNumberDataPoint'Value Word64 where
  toNumberDataPoint'Value :: Word64 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

-- | __Warning__: This instance may cause overflow.
instance IsNumberDataPoint'Value Word where
  toNumberDataPoint'Value :: Word -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

instance IsNumberDataPoint'Value Int8 where
  toNumberDataPoint'Value :: Int8 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

instance IsNumberDataPoint'Value Int16 where
  toNumberDataPoint'Value :: Int16 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

instance IsNumberDataPoint'Value Int32 where
  toNumberDataPoint'Value :: Int32 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral

instance IsNumberDataPoint'Value Int64 where
  toNumberDataPoint'Value :: Int64 -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt

instance IsNumberDataPoint'Value Int where
  toNumberDataPoint'Value :: Int -> OM.NumberDataPoint'Value
  toNumberDataPoint'Value = OM.NumberDataPoint'AsInt . fromIntegral
