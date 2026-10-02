{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Machine.Analysis.Log
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.Analysis.Log where

import Data.Machine (Process, await, repeatedly, yield)
import Data.Text (Text)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..), tryGetTimeUnixNano)
import GHC.Eventlog.Live.Types.Attribute (Attrs, (~=))
import GHC.Eventlog.Live.Types.Capability (CapNo, evCapNo)
import GHC.Eventlog.Live.Types.Severity (Severity (..))
import GHC.RTS.Events (Event, Timestamp)
import GHC.RTS.Events qualified as E
import GHC.Records (HasField (..))

--------------------------------------------------------------------------------
-- UserMessage

data UserMessage = UserMessage
  { value :: !Text
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  , capNo :: !(Maybe CapNo)
  }

instance HasField "maybeSeverity" UserMessage (Maybe Severity) where
  getField = const (Just TRACE)

instance HasField "attrs" UserMessage Attrs where
  getField i = ["capNo" ~= i.capNo]

{- |
This machine processes `E.UserMessage` events into logs.
-}
processUserMessage :: Process (WithStartTime Event) UserMessage
processUserMessage =
  repeatedly $
    await >>= \case
      i
        | E.UserMessage{..} <- i.value.evSpec ->
            yield
              UserMessage
                { value = msg
                , maybeTimeUnixNano = tryGetTimeUnixNano i
                , capNo = evCapNo i.value
                }
        | otherwise -> pure ()

--------------------------------------------------------------------------------
-- UserMarker

data UserMarker = UserMarker
  { value :: !Text
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  , capNo :: !(Maybe CapNo)
  }

instance HasField "maybeSeverity" UserMarker (Maybe Severity) where
  getField = const Nothing

instance HasField "attrs" UserMarker Attrs where
  getField i = ["capNo" ~= i.capNo]

{- |
This machine processes `E.UserMarker` events into logs.
-}
processUserMarker :: Process (WithStartTime Event) UserMarker
processUserMarker =
  repeatedly $
    await >>= \case
      i
        | E.UserMarker{..} <- i.value.evSpec ->
            yield
              UserMarker
                { value = markername
                , maybeTimeUnixNano = tryGetTimeUnixNano i
                , capNo = evCapNo i.value
                }
        | otherwise -> pure ()
