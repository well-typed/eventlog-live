{- |
Module      : GHC.Eventlog.Live.Otlp.Processor.Common.Metrics
Description : Common utilities for metric processors.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otlp.Processor.Common.Metrics (
  -- * Known Metrics
  KnownMetric (..),
  getConfig,
  SomeMetric (..),

  -- * Metric Processor
  MetricProcessor (..),
  process,
  processorFor,
  processWith,

  -- * Multi-Metric Processor
  MetricProcessors (..),
  select,
  processAllWith,

  -- * Metric Aggregatorss
  MetricAggregators (..),
  viaSum,
  viaLast,
)
where

import Data.Coerce (Coercible, coerce)
import Data.DList (DList)
import Data.DList qualified as D
import Data.Default (Default (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Constraint, Type)
import Data.Machine (Process, ProcessT, asParts, echo, mapping, (~>))
import Data.Proxy (Proxy (..))
import Data.Semigroup (Last (..), Sum (..))
import GHC.Eventlog.Live.Data.Group (Group, GroupBy, GroupedBy)
import GHC.Eventlog.Live.Data.Group qualified as DG
import GHC.Eventlog.Live.Data.Metric (KnownMetricPointKind (..), KnownMetricType (..), KnownMetricUnit, Metric (..), MetricPointKind (..), MetricUnit (..), SAggregationTemporality (..), SMetricPointKind (..))
import GHC.Eventlog.Live.Machine.Core (Tick)
import GHC.Eventlog.Live.Machine.Core qualified as M
import GHC.Eventlog.Live.Otlp.Config qualified as C
import GHC.Eventlog.Live.Otlp.Config.Types (FullConfig)
import GHC.Eventlog.Live.Otlp.Processor.Common.Core (runIf)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)

--------------------------------------------------------------------------------
-- Known Metrics
--------------------------------------------------------------------------------

type KnownMetric :: Type -> Constraint
class
  ( HasField (NameOf metric) C.Metrics (Maybe metric)
  , C.IsMetricProcessorConfig metric
  , Show metric
  , Default metric
  , KnownSymbol (NameOf metric)
  , KnownMetricType (TypeOf metric)
  , KnownMetricPointKind (PointKindOf metric)
  , KnownMetricUnit (UnitOf metric)
  ) =>
  KnownMetric metric
  where
  type NameOf metric :: Symbol
  type TypeOf metric :: Type
  type UnitOf metric :: MetricUnit
  type PointKindOf metric :: MetricPointKind

getConfig :: forall metric. (KnownMetric metric) => C.Metrics -> Maybe metric
getConfig = getField @(NameOf metric)
{-# INLINE getConfig #-}

type SomeMetric :: Type
data SomeMetric
  = forall metric. (KnownMetric metric) => SomeMetric !(Proxy metric) [Metric (TypeOf metric)]

--------------------------------------------------------------------------------
-- Metric Processor
--------------------------------------------------------------------------------

{- |
A t`MetricProcessor` holds the building blocks for the processing pipeline for a
single metric.
-}
type MetricProcessor :: Type -> (Type -> Type) -> Type -> Type -> Type -> Type
data MetricProcessor metric m a b c
  = forall f.
  (Monad m, KnownMetric metric, Foldable f) =>
  MetricProcessor
  { processor :: !(ProcessT m a b)
  -- ^ The `processor` field holds the input processor. Usually, this processes events into t`Metric`.
  , aggregators :: !(MetricAggregators b c)
  -- ^ The `aggregators` field holds the aggregation strategies.
  , ungroup :: !(c -> f (Metric (TypeOf metric)))
  -- ^ The `ungroup` function is useful when one value holds multiple metrics of the same type.
  }

process ::
  (Monad m, KnownMetric metric) =>
  Proxy metric ->
  ProcessT m i (Metric (TypeOf metric)) ->
  FullConfig ->
  ProcessT m (Tick i) (Tick SomeMetric)
process (metric :: Proxy metric) processor =
  processWith @metric (processorFor metric processor)
{-# INLINE process #-}

processorFor ::
  (Monad m, KnownMetric metric) =>
  Proxy metric ->
  ProcessT m i (Metric (TypeOf metric)) ->
  MetricProcessor metric m i (Metric (TypeOf metric)) (Metric (TypeOf metric))
processorFor (metric :: Proxy metric) processor =
  MetricProcessor{processor = processor, aggregators = aggregatorsFor metric, ungroup = Identity}
{-# INLINE processorFor #-}

processWith ::
  forall metric m a b c.
  MetricProcessor metric m a b c ->
  FullConfig ->
  ProcessT m (Tick a) (Tick SomeMetric)
processWith MetricProcessor{..} fullConfig =
  runIf (C.processorEnabled (.metrics) (getConfig @metric) fullConfig) $
    M.liftTick processor
      ~> aggregate aggregators (C.processorAggregationBatches (.metrics) (getConfig @metric) fullConfig)
      ~> M.liftTick (mapping ungroup ~> asParts ~> mapping D.singleton)
      ~> M.batchByTicks (C.processorExportBatches (.metrics) (getConfig @metric) fullConfig)
      ~> M.liftTick (mapping $ SomeMetric (Proxy @metric) . D.toList)
{-# INLINE processWith #-}

--------------------------------------------------------------------------------
-- Multi-Metric Processor
--------------------------------------------------------------------------------

infixr 6 :&:

{- |
A t`MetricProcessors` holds a series of t`MetricProcessor`s that work from the same input type.
-}
type MetricProcessors :: [Type] -> (Type -> Type) -> Type -> Type
data MetricProcessors metrics m a where
  End ::
    MetricProcessors '[] m i
  (:&:) ::
    forall metric metrics m i a b.
    (KnownMetric metric) =>
    (MetricProcessor metric m i a b) ->
    MetricProcessors metrics m i ->
    MetricProcessors (metric ': metrics) m i

{- |
Check if /any/ of the t`MetricProcessors` is enabled.
-}
anyProcessorEnabled :: FullConfig -> MetricProcessors metrics m i -> Bool
anyProcessorEnabled _fullConfig End = False
anyProcessorEnabled fullConfig ((:&:) @metric _ rest) =
  C.processorEnabled (.metrics) (getConfig @metric) fullConfig || anyProcessorEnabled fullConfig rest

select ::
  (Monad m, KnownMetric metric) =>
  Proxy metric ->
  (i -> TypeOf metric) ->
  MetricProcessor metric m (Metric i) (Metric (TypeOf metric)) (Metric (TypeOf metric))
select (metric :: Proxy metric) f =
  MetricProcessor{processor = mapping (fmap f), aggregators = aggregatorsFor metric, ungroup = Identity}
{-# INLINE select #-}

processAllWith ::
  forall metrics m i a.
  (Monad m) =>
  -- | The full configuration.
  FullConfig ->
  ProcessT m i a ->
  MetricProcessors metrics m a ->
  ProcessT m (Tick i) (Tick (DList SomeMetric))
processAllWith fullConfig preprocessor processors =
  runIf (anyProcessorEnabled fullConfig processors) $
    M.liftTick preprocessor
      ~> M.fanoutTick
        [ processor ~> M.liftTick (mapping D.singleton)
        | processor <- processAllWith' processors
        ]
 where
  processAllWith' :: MetricProcessors metrics' m a -> [ProcessT m (Tick a) (Tick SomeMetric)]
  processAllWith' End = []
  processAllWith' (p :&: ps) = processWith p fullConfig : processAllWith' ps

--------------------------------------------------------------------------------
-- Metric Aggregators
--------------------------------------------------------------------------------

data MetricAggregators a b = MetricAggregators
  { nothing :: Process (Tick a) (Tick b)
  , byBatches :: Int -> Process (Tick a) (Tick b)
  }

{- |
Internal helper.

Get the aggregator for a known metric, based on its known `MetricPointKind`.
-}
aggregatorsFor ::
  (KnownMetric metric) =>
  Proxy metric ->
  MetricAggregators (Metric (TypeOf metric)) (Metric (TypeOf metric))
aggregatorsFor (_metric :: Proxy metric) =
  case metricPointKindSing (Proxy @(PointKindOf metric)) of
    SGauge -> viaLast
    SSum SCumulative _sMonotonicity -> viaLast
    SSum SDelta _sMonotonicity -> viaSum

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
