{- |
Module      : GHC.Eventlog.Live.Sample
Description : Representation for OTLP stack samples.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Data.Sample (
  -- * Sample superclass
  IsSample,
  toSample,

  -- * Generic Sample Kind
  Sample (..),
  Location (..),
) where

import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Eventlog.Live.Data.Attribute (Attrs)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField)
import IpeDB.Types.SrcLoc (SrcLoc)

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
