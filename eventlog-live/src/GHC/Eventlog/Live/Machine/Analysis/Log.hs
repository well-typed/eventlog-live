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
import GHC.Eventlog.Live.Data.Attribute (Attrs, (~=))
import GHC.Eventlog.Live.Data.LogRecord (LogRecord (..))
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..), tryGetTimeUnixNano)
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E

--------------------------------------------------------------------------------
-- UserMessage

{- |
This machine processes `E.UserMessage` events into logs.
-}
processUserMessageData :: Process (WithStartTime Event) LogRecord
processUserMessageData =
  repeatedly $
    await >>= \case
      i
        | E.UserMessage{..} <- i.value.evSpec ->
            yield $
              logRecord i msg $
                [ "evCap" ~= i.value.evCap
                ]
        | otherwise -> pure ()

--------------------------------------------------------------------------------
-- UserMarker

{- |
This machine processes `E.UserMarker` events into logs.
-}
processUserMarkerData :: Process (WithStartTime Event) LogRecord
processUserMarkerData =
  repeatedly $
    await >>= \case
      i
        | E.UserMarker{..} <- i.value.evSpec ->
            yield $
              logRecord i markername $
                [ "evCap" ~= i.value.evCap
                , "marker" ~= True
                ]
        | otherwise -> pure ()

{- |
Internal helper.
Construct a t`LogRecord` from an event with a start time, a message, and any
set of attributes. This is a smart constructor that pulls the timestamps out
of the event.
-}
logRecord ::
  WithStartTime Event ->
  Text ->
  Attrs ->
  LogRecord
logRecord i body attrs =
  LogRecord
    { body = body
    , maybeTimeUnixNano = tryGetTimeUnixNano i
    , maybeSeverity = Nothing
    , attrs = attrs
    }
