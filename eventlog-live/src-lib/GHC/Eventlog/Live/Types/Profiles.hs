{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Sample
Description : Representation for OTLP stack samples.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Types.Profiles (
  -- * Samples
  SomeSamples (..),
  KnownProfile (..),
  profileConfig,

  -- * Sample superclass
  IsSample,
  toSample,

  -- * Generic Sample Kind
  Sample (..),
  Location (..),
) where

import Data.Aeson.Types (Encoding, KeyValue (..), KeyValueOmit (..), ToJSON (..), Value (..), pairs)
import Data.Default (Default)
import Data.Int (Int64)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Eventlog.Live.Config (CallStackProfile (..), CostCentreStackProfile (..), IsProfileProcessorConfig, Profiles (..))
import GHC.Eventlog.Live.Types.Attribute (Attrs)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import IpeDB.Types.SrcLoc (Range (..), SrcLoc (..))

--------------------------------------------------------------------------------
-- Samples
--------------------------------------------------------------------------------

type SomeSamples :: Type
data SomeSamples
  = forall profile.
    (KnownProfile profile) =>
    SomeSamples
      !(Proxy profile)
      ![Sample (GetProfileMetricType profile)]

instance ToJSON SomeSamples where
  toJSON :: SomeSamples -> Value
  toJSON = Object . someSamplesToKV

  toEncoding :: SomeSamples -> Encoding
  toEncoding = pairs . someSamplesToKV

  omitField :: SomeSamples -> Bool
  omitField (SomeSamples _profile samples) = null samples

someSamplesToKV :: (KeyValueOmit e kv, Monoid kv) => SomeSamples -> kv
someSamplesToKV (SomeSamples (profile :: Proxy profile) samples) =
  mconcat $
    [ "name" .= symbolVal profile
    , "samples" .?= (fmap (fromIntegral @_ @Int64) <$> samples)
    -- "samples" is the unique property that identifies this type.
    ]
{-# INLINE someSamplesToKV #-}

-------------------------------------------------------------------------------
-- KnownProfile & Instances
-------------------------------------------------------------------------------

type KnownProfile :: Symbol -> Constraint
class
  ( HasField profile Profiles (Maybe (GetProfileConf profile))
  , IsProfileProcessorConfig (GetProfileConf profile)
  , Show (GetProfileConf profile)
  , Default (GetProfileConf profile)
  , KnownSymbol profile
  , KnownSymbol (GetProfileMetricName profile)
  , Integral (GetProfileMetricType profile)
  , KnownSymbol (GetProfileMetricUnit profile)
  ) =>
  KnownProfile profile
  where
  type GetProfileConf profile :: Type
  type GetProfileMetricName profile :: Symbol
  type GetProfileMetricType profile :: Type
  type GetProfileMetricUnit profile :: Symbol

profileConfig :: forall profile. (KnownProfile profile) => Proxy profile -> Profiles -> Maybe (GetProfileConf profile)
profileConfig (_profile :: Proxy profile) = getField @profile
{-# INLINE profileConfig #-}

instance KnownProfile "callStackProfile" where
  type GetProfileConf "callStackProfile" = CallStackProfile
  type GetProfileMetricName "callStackProfile" = "callStack"
  type GetProfileMetricType "callStackProfile" = Word8
  type GetProfileMetricUnit "callStackProfile" = "count"

instance KnownProfile "costCentreStackProfile" where
  type GetProfileConf "costCentreStackProfile" = CostCentreStackProfile
  type GetProfileMetricName "costCentreStackProfile" = "costCentreStack"
  type GetProfileMetricType "costCentreStackProfile" = Word8
  type GetProfileMetricUnit "costCentreStackProfile" = "count"

--------------------------------------------------------------------------------
-- Superclass for span types
--------------------------------------------------------------------------------

type IsSample s v =
  ( HasField "value" s v
  , HasField "stack" s (Vector Location)
  , HasField "maybeTimeUnixNano" s (Maybe Timestamp)
  , HasField "attrs" s Attrs
  )

toSample :: (IsSample s v) => s -> Sample v
toSample s =
  Sample
    { value = s.value
    , stack = s.stack
    , maybeTimeUnixNano = s.maybeTimeUnixNano
    , attrs = s.attrs
    }

--------------------------------------------------------------------------------
-- Generic sample type
--------------------------------------------------------------------------------

-- TODO: This sample type encodes the one-value, one-timestamp shape, but
--       the OTLP specification supports many other shapes.
--       See: https://github.com/open-telemetry/opentelemetry-proto/blob/b3f75588eb23c5fca62264edd05d382de49beb1a/opentelemetry/proto/profiles/v1development/profiles.proto#L460-L493

data Sample v = Sample
  { value :: !v
  , stack :: !(Vector Location)
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  -- ^ The time at which the sample was taken.
  , attrs :: Attrs
  -- ^ A set of attributes.
  }
  deriving (Functor)

instance (ToJSON v) => ToJSON (Sample v) where
  toJSON :: Sample v -> Value
  toJSON = Object . sampleToKV

  toEncoding :: Sample v -> Encoding
  toEncoding = pairs . sampleToKV

sampleToKV ::
  forall e kv v.
  (KeyValueOmit e kv, Monoid kv, ToJSON v) =>
  Sample v -> kv
sampleToKV s =
  mconcat
    [ "value" .= s.value
    , "stack" .?= s.stack
    , "time_unix_nano" .?= s.maybeTimeUnixNano
    , "attrs" .?= s.attrs
    ]
{-# INLINE sampleToKV #-}

data Location = Location
  { name :: !Text
  , srcLoc :: !SrcLoc
  }

instance ToJSON Location where
  toJSON :: Location -> Value
  toJSON = Object . locationToKV

  toEncoding :: Location -> Encoding
  toEncoding = pairs . locationToKV

locationToKV ::
  forall e kv.
  (KeyValueOmit e kv, Monoid kv) =>
  Location -> kv
locationToKV l =
  mconcat
    [ "name" .= l.name `onlyIf` (not . T.null)
    , srcLocToKV l.srcLoc
    ]
 where
  srcLocToKV :: SrcLoc -> kv
  srcLocToKV = \case
    UnhelpfulSrcLoc ->
      mempty
    SrcLoc{..} ->
      mconcat
        [ "file" .?= srcFilePath `onlyIf` (not . null)
        , maybe mempty rangeToKV srcRange
        ]

  rangeToKV :: Range -> kv
  rangeToKV = \case
    Range'Point{..} ->
      mconcat
        [ "line" .= line
        , "column" .= column
        ]
    Range'OneLine{..} ->
      mconcat
        [ "line" .= line
        , "column" .= column
        , "end_column" .= endColumn
        ]
    Range'MultiLine{..} ->
      mconcat
        [ "line" .= line
        , "column" .= column
        , "end_line" .= endLine
        , "end_column" .= endColumn
        ]

  onlyIf :: a -> (a -> Bool) -> Maybe a
  onlyIf a p = if p a then Just a else Nothing
{-# INLINE locationToKV #-}
