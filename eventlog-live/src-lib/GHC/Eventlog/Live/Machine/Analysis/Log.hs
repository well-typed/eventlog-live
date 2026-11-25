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
import GHC.Eventlog.Live.Machine.WithMetaData (WithMetaData (..))
import GHC.Eventlog.Live.Types.Attribute (Attrs, (~=))
import GHC.Eventlog.Live.Types.Capability (CapNo, evCapNo)
import GHC.Eventlog.Live.Types.Severity (Severity (..))
import GHC.Eventlog.Live.Types.Time (TimeUnixNano)
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import GHC.Records (HasField (..))

--------------------------------------------------------------------------------
-- UserMessage

data UserMessage = UserMessage
  { value :: !Text
  , timeUnixNano :: !TimeUnixNano
  , capNo :: !(Maybe CapNo)
  }

instance HasField "maybeSeverity" UserMessage (Maybe Severity) where
  getField = const (Just TRACE)

instance HasField "attrs" UserMessage Attrs where
  getField i = ["capNo" ~= i.capNo]

{- |
This machine processes `E.UserMessage` events into logs.
-}
processUserMessage :: Process (WithMetaData Event) UserMessage
processUserMessage =
  repeatedly $
    await >>= \case
      i
        | E.UserMessage{..} <- i.value.evSpec ->
            yield
              UserMessage
                { value = msg
                , timeUnixNano = i.timeUnixNano
                , capNo = evCapNo i.value
                }
        | otherwise -> pure ()

--------------------------------------------------------------------------------
-- UserMarker

data UserMarker = UserMarker
  { value :: !Text
  , timeUnixNano :: !TimeUnixNano
  , capNo :: !(Maybe CapNo)
  }

instance HasField "maybeSeverity" UserMarker (Maybe Severity) where
  getField = const Nothing

instance HasField "attrs" UserMarker Attrs where
  getField i = ["capNo" ~= i.capNo]

{- |
This machine processes `E.UserMarker` events into logs.
-}
processUserMarker :: Process (WithMetaData Event) UserMarker
processUserMarker =
  repeatedly $
    await >>= \case
      i
        | E.UserMarker{..} <- i.value.evSpec ->
            yield
              UserMarker
                { value = markername
                , timeUnixNano = i.timeUnixNano
                , capNo = evCapNo i.value
                }
        | otherwise -> pure ()
