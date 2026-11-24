{- |
Module      : GHC.Eventlog.Live.Machine.WithStartTime
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.WithStartTime (
  WithStartTime (..),
  setWithStartTime'value,
  withStartTime,
  withStartTime',
  dropStartTime,
) where

import Control.Monad (forever, when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (for_)
import Data.Machine (Process, ProcessT, await, construct, mapping, yield)
import Data.Text qualified as T
import GHC.Eventlog.Live.Logger (Logger, logWarn)
import GHC.Eventlog.Live.Types.Time (TimeUnixNano)
import GHC.RTS.Events (Event (..), EventInfo)
import GHC.RTS.Events qualified as E
import GHC.Records (HasField (..))

-------------------------------------------------------------------------------
-- Start time

{- |
Data decorated with a start time in nanoseconds since the Unix epoch.
-}
data WithStartTime a = WithStartTime
  { value :: !a
  , startTimeUnixNano :: !TimeUnixNano
  }
  deriving (Functor, Show)

instance HasField "timeUnixNano" (WithStartTime Event) TimeUnixNano where
  getField i = i.value.evTime + i.startTimeUnixNano

{- |
Setter for the value of a t`WithStartTime`
-}
setWithStartTime'value :: WithStartTime a -> b -> WithStartTime b
setWithStartTime'value (WithStartTime _a t) b = WithStartTime b t

{- |
Wrap every event in t`WithStartTime`. Every event after `E.WallClockTime` will
have its start time field set to `Just` the process start time.

This machine swallows the first and only `E.WallClockTime` event.
-}
withStartTime :: (Monad m) => Logger m -> ProcessT m Event (WithStartTime Event)
withStartTime logger = withStartTime' logger E.evSpec WithStartTime

{- |
Generalised version of `withStartTime` that can be adapted to work on arbitrary
types using a getter and a setter.
-}
withStartTime' :: (Monad m) => Logger m -> (a -> EventInfo) -> (a -> TimeUnixNano -> b) -> ProcessT m a b
withStartTime' logger getEventInfo setStartTime =
  construct $ start []
 where
  start acc =
    await >>= \case
      i
        | -- If this is the WallClockTime event, convert the timestamp to
          -- nanoseconds and continue.
          E.WallClockTime{..} <- getEventInfo i -> do
            -- This will start overflowing on Sunday, 21 July 2554 23:34:33, UTC.
            let !startTimeUnixNano = sec * 1_000_000_000 + fromIntegral nsec
            -- Emit all buffered events with the start time.
            --
            -- NOTE: This does not reemit the WallClockTime event.
            for_ (reverse acc) $ \j -> yield (j `setStartTime` startTimeUnixNano)
            -- From here on, simply reemit the events with their start time immediately.
            continue startTimeUnixNano
        | -- If this is not the WallClockTime event, buffer it and continue.
          otherwise -> do
            lift . when (shouldWorry (getEventInfo i)) . logWarn logger . T.pack $
              "Unexpected event received prior to WallClockTime event: " <> show (getEventInfo i)
            start (i : acc)
  continue startTimeUnixNano =
    forever (await >>= yield . (`setStartTime` startTimeUnixNano))

{- |
Drop the t`WithStartTime` wrapper.
-}
dropStartTime :: Process (WithStartTime a) a
dropStartTime = mapping (.value)

{- |
Should `withStartTime'` be worried if it sees this event before the `E.WallClockTime` event?
-}
shouldWorry :: EventInfo -> Bool
shouldWorry E.InfoTableProv{} = False
shouldWorry E.HeapInfoGHC{} = False
shouldWorry E.HeapProfCostCentre{} = False
shouldWorry E.OsProcessPid{} = False
shouldWorry E.OsProcessParentPid{} = False
shouldWorry E.RtsIdentifier{} = False
shouldWorry E.ProgramArgs{} = False
shouldWorry _ = True
