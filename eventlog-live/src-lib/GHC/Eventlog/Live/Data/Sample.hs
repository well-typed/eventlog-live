{- |
Module      : GHC.Eventlog.Live.Sample
Description : Representation for OTLP stack samples.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Sample (
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

import Data.Default (Default)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)
import GHC.Eventlog.Live.Config (CallStackProfile (..), CostCentreStackProfile (..), IsProfileProcessorConfig, Profiles (..))
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)
import IpeDB.Types.SrcLoc (SrcLoc)

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

-------------------------------------------------------------------------------
-- KnownProfile & Instances
-------------------------------------------------------------------------------

type KnownProfile :: Type -> Constraint
class
  ( HasField (GetProfileName profile) Profiles (Maybe profile)
  , IsProfileProcessorConfig profile
  , Show profile
  , Default profile
  , KnownSymbol (GetProfileName profile)
  , KnownSymbol (GetProfileMetricName profile)
  , Integral (GetProfileMetricType profile)
  , KnownSymbol (GetProfileMetricUnit profile)
  ) =>
  KnownProfile profile
  where
  type GetProfileName profile :: Symbol
  type GetProfileMetricName profile :: Symbol
  type GetProfileMetricType profile :: Type
  type GetProfileMetricUnit profile :: Symbol

profileConfig :: forall profile. (KnownProfile profile) => Profiles -> Maybe profile
profileConfig = getField @(GetProfileName profile)
{-# INLINE profileConfig #-}

instance KnownProfile CallStackProfile where
  type GetProfileName CallStackProfile = "callStackProfile"
  type GetProfileMetricName CallStackProfile = "callStack"
  type GetProfileMetricType CallStackProfile = Word8
  type GetProfileMetricUnit CallStackProfile = "count"

instance KnownProfile CostCentreStackProfile where
  type GetProfileName CostCentreStackProfile = "costCentreStackProfile"
  type GetProfileMetricName CostCentreStackProfile = "costCentreStack"
  type GetProfileMetricType CostCentreStackProfile = Word8
  type GetProfileMetricUnit CostCentreStackProfile = "count"

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

data Location = Location
  { name :: !Text
  , srcLoc :: !SrcLoc
  }
