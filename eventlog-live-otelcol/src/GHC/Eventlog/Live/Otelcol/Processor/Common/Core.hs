{- |
Module      : GHC.Eventlog.Live.Otelcol.Processor.Common.Core
Description : Common utilities shared across telemetry data types.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Otelcol.Processor.Common.Core (
  messageWith,
  runIf,
  ifNonEmpty,
  toMaybeKeyValue,
)
where

import Data.Functor ((<&>))
import Data.Machine (MachineT, stopped)
import Data.ProtoLens (Message (..))
import GHC.Eventlog.Live.Data.Attribute (Attr, AttrValue (..))
import Lens.Family2 ((.~))
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC

-- | Construct a message with a list of modifications applied.
messageWith :: (Message msg) => [msg -> msg] -> msg
messageWith = foldr ($) defMessage

-- | Run a machine if a boolean is @True@, otherwise stop.
runIf :: (Monad m) => Bool -> MachineT m k o -> MachineT m k o
runIf b m = if b then m else stopped

-- | Return the second argument if the first argument is non-empty.
ifNonEmpty :: [a] -> b -> Maybe b
ifNonEmpty xs r = if null xs then Nothing else Just r

--------------------------------------------------------------------------------
-- Interpret attributes

{- |
Convert an `Attr` to an OTLP `OC.KeyValue`.
-}
toMaybeKeyValue :: Attr -> Maybe OC.KeyValue
toMaybeKeyValue (k, v) =
  toMaybeAnyValue v <&> \v' ->
    messageWith
      [ OC.key .~ k
      , OC.value .~ v'
      ]

{- |
Internal helper.
Convert an `AttrValue` to an OTLP `OC.AnyValue`.
-}
toMaybeAnyValue :: AttrValue -> Maybe OC.AnyValue
toMaybeAnyValue = \case
  AttrBool v -> Just $ messageWith [OC.boolValue .~ v]
  AttrInt v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt8 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt16 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt32 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrInt64 v -> Just $ messageWith [OC.intValue .~ v]
  AttrWord v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord8 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord16 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord32 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrWord64 v -> Just $ messageWith [OC.intValue .~ fromIntegral v]
  AttrDouble v -> Just $ messageWith [OC.doubleValue .~ v]
  AttrText v -> Just $ messageWith [OC.stringValue .~ v]
  AttrNull -> Nothing
