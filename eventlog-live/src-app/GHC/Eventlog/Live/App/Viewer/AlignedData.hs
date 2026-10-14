{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.App.Viewer.AlignedData (
  AlignedData (..),
  Series (..),
  validate,
  toAlignedData,
) where

import Control.Monad.ST.Strict (runST)
import Data.Aeson.Types (Encoding, KeyValue (..), ToJSON (..), Value (..), pairs)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as U
import GHC.Eventlog.Live.Machine.WithStartTime (TimeUnixNano)
import GHC.Eventlog.Live.Types.Attribute (Attrs)
import GHC.Eventlog.Live.Types.Metrics (Metric (..))

type TimeUnix = Int

toTimeUnix :: TimeUnixNano -> TimeUnix
toTimeUnix timeUnixNano = fromIntegral (timeUnixNano `div` 1_000_000_000)

data AlignedData a = AlignedData
  { xValues :: !(U.Vector TimeUnix)
  , series :: !(Vector (Series a))
  }

alignedDataToKV :: (KeyValue e kv, Monoid kv, ToJSON a) => AlignedData a -> kv
alignedDataToKV d = mconcat ["xValues" .= d.xValues, "series" .= d.series]
{-# INLINE alignedDataToKV #-}

instance (ToJSON a) => ToJSON (AlignedData a) where
  toJSON :: AlignedData a -> Value
  toJSON = Object . alignedDataToKV

  toEncoding :: AlignedData a -> Encoding
  toEncoding = pairs . alignedDataToKV

data Series a = Series
  { attrs :: !Attrs
  , yValues :: !(Vector (Maybe a))
  }

seriesToKV :: (KeyValue e kv, Monoid kv, ToJSON a) => Series a -> kv
seriesToKV s = mconcat ["attrs" .= s.attrs, "yValues" .= s.yValues]
{-# INLINE seriesToKV #-}

instance (ToJSON a) => ToJSON (Series a) where
  toJSON :: Series a -> Value
  toJSON = Object . seriesToKV

  toEncoding :: Series a -> Encoding
  toEncoding = pairs . seriesToKV

-- | Check the invariant for `AlignedData`.
validate :: AlignedData a -> Bool
validate d = V.all (\s -> U.length d.xValues == V.length s.yValues) d.series

data DataPoint a = DataPoint {attrs :: !Attrs, timeUnix :: !TimeUnix, value :: !a}

toDataPoint :: Metric a -> Maybe (DataPoint a)
toDataPoint Metric{..} =
  maybeTimeUnixNano <&> \timeUnixNano -> DataPoint{timeUnix = toTimeUnix timeUnixNano, ..}

toAlignedData :: [Metric a] -> AlignedData a
toAlignedData metrics = AlignedData xValues series
 where
  dps = mapMaybe toDataPoint metrics
  (xValuesSet, attrsSet) = foldMap (\dp -> (IS.singleton dp.timeUnix, S.singleton dp.attrs)) dps

  xValuesCount = IS.size xValuesSet
  xValues = U.fromListN xValuesCount (IS.toAscList xValuesSet)
  xValuesMap = IM.fromAscList (zip (U.toList xValues) [(0 :: Int) ..])

  attrs = S.toAscList attrsSet
  attrsCount = S.size attrsSet

  series = runST $ do
    -- Create mutable vectors for the yValues of every series initialised to Nothing.
    seriesMut :: Vector (MVector s (Maybe a)) <- V.replicateM attrsCount (VM.replicate xValuesCount Nothing)

    -- Create an index over the series.
    let seriesMutMap = M.fromAscList . zip attrs $ (seriesMut V.!) <$> [0 .. attrsCount]

    -- Process all data points.
    for_ dps $ \dp -> do
      let yValues = seriesMutMap M.! dp.attrs
      let i = xValuesMap IM.! dp.timeUnix
      VM.write yValues i (Just dp.value)

    -- Freeze the yValues and wrap them as series.
    seriesMap <- traverse V.unsafeFreeze seriesMutMap
    pure $ V.fromListN attrsCount (uncurry Series <$> M.toAscList seriesMap)
