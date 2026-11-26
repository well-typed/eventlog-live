{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.Machine.WithMetaData
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine.WithMetaData (
  WithMetaData (..),
  setWithMetaData'value,
  withMetaData,
  dropMetaData,
) where

import Control.Applicative (Alternative (..))
import Control.Monad (forever, when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Foldable (for_)
import Data.Machine (Is, Process, ProcessT, await, construct, mapping, yield)
import Data.Machine.Plan (PlanT)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Eventlog.Live.Logger (Logger, logWarn)
import GHC.Eventlog.Live.Types.Time (TimeUnixNano)
import GHC.RTS.Events (Event (..), EventInfo, PID)
import GHC.RTS.Events qualified as E
import GHC.Records (HasField (..))

-------------------------------------------------------------------------------
-- Start time

{- |
Program metadata.
-}
data MetaData = MetaData
  { startTimeUnixNano :: {-# UNPACK #-} !TimeUnixNano
  -- ^ The program start time in nanoseconds since the Unix epoch
  , processId :: {-# UNPACK #-} !PID
  -- ^ The program's process ID.
  , parentProcessId :: {-# UNPACK #-} !(Maybe PID)
  {- ^ The program's parent process ID.

  This event is not emitted on Windows, which doesn't have process hierarchy.
  -}
  , programArgs :: ![Text]
  -- ^ The program's command-line arguments.
  , programEnv :: !(Maybe [Text])
  -- ^ The program's environment.
  , rtsIdentifier :: !Text
  -- ^ The identifier for the runtime system.
  }
  deriving (Show)

{- |
Values decorated with program meta data.
-}
data WithMetaData a = WithMetaData
  { value :: !a
  , meta :: !MetaData
  }
  deriving (Functor, Show)

instance HasField "timeUnixNano" (WithMetaData Event) TimeUnixNano where
  getField i = i.value.evTime + i.meta.startTimeUnixNano

instance HasField "startTimeUnixNano" (WithMetaData a) TimeUnixNano where
  getField = (.startTimeUnixNano) . (.meta)

instance HasField "processId" (WithMetaData a) PID where
  getField = (.processId) . (.meta)

instance HasField "parentProcessId" (WithMetaData a) (Maybe PID) where
  getField = (.parentProcessId) . (.meta)

instance HasField "programArgs" (WithMetaData a) [Text] where
  getField = (.programArgs) . (.meta)

instance HasField "programEnv" (WithMetaData a) (Maybe [Text]) where
  getField = (.programEnv) . (.meta)

instance HasField "rtsIdentifier" (WithMetaData a) Text where
  getField = (.rtsIdentifier) . (.meta)

{- |
Setter for the value of a t`WithMetaData`
-}
setWithMetaData'value :: WithMetaData a -> b -> WithMetaData b
setWithMetaData'value (WithMetaData _a t) b = WithMetaData b t

{- |
Wrap every event in t`WithMetaData`. Every event after `E.WallClockTime` will
have its start time field set to `Just` the process start time.

This machine swallows the first and only `E.WallClockTime` event.
-}
withMetaData :: forall m. (Monad m) => Logger m -> ProcessT m Event (WithMetaData Event)
withMetaData logger = construct $ start mempty 0 []
 where
  start :: PartialMetaData -> Word -> [Event] -> PlanT (Is Event) (WithMetaData Event) m Void
  start partial nonInitCount buffer =
    await >>= \case
      ev
        | -- If the event is a WallClockTime event...
          E.WallClockTime{..} <- E.evSpec ev -> do
            -- ...construct the singleton partial metadata for start time...
            --
            -- NOTE: This will start overflowing on Sunday, 21 July 2554
            --       23:34:33, UTC. Hey! What's your life like there?
            let !startTimeUnixNano = sec * 1_000_000_000 + fromIntegral nsec
            let !wallClockTime = mempty{maybeStartTimeUnixNano = Just startTimeUnixNano}
            -- ...and combine it with the buffered partial metadata...
            start (partial <> wallClockTime) nonInitCount buffer
        | E.OsProcessPid{..} <- E.evSpec ev -> do
            let processId = mempty{maybeProcessId = Just pid}
            start (partial <> processId) nonInitCount buffer
        | E.OsProcessParentPid{..} <- E.evSpec ev -> do
            let parentProcessId = mempty{maybeParentProcessId = Just ppid}
            start (partial <> parentProcessId) nonInitCount buffer
        | E.RtsIdentifier{..} <- E.evSpec ev -> do
            let rtsIdentifier = mempty{maybeRtsIdentifier = Just rtsident}
            start (partial <> rtsIdentifier) nonInitCount buffer
        | E.ProgramArgs{..} <- E.evSpec ev -> do
            let programArgs = mempty{maybeProgramArgs = Just args}
            start (partial <> programArgs) nonInitCount buffer
        | E.ProgramEnv{..} <- E.evSpec ev -> do
            let programEnv = mempty{maybeProgramEnv = Just env}
            start (partial <> programEnv) nonInitCount buffer
        | -- If the event is any other init event...
          isInitEvent ev -> do
            -- ...buffer it and continue...
            start partial nonInitCount (ev : buffer)
        | -- If the event is some non-init event and the metadata is complete...
          Just metadata <- toMetaData partial -> do
            -- ...yield all the buffered events...[
            for_ (reverse buffer) $ \ev' ->
              yield (WithMetaData ev' metadata)
            -- ..then continue with the complete metadata.
            continue metadata
        | -- If the event is some non-init event, but the metadata is incomplete...
          otherwise -> do
            -- ...mention that you're worried about this state of affairs...
            -- (But only every 1,000 events.)
            when (nonInitCount `mod` 1_000 == 0) . lift . logWarn logger $
              "Received " <> T.show nonInitCount <> "unexpected non-init event(s) before complete set of events: " <> T.show ev
            -- ...and wait for the missing init event...
            start partial (nonInitCount + 1) (ev : buffer)

  -- Add the completed metadata to each future event...
  continue :: MetaData -> PlanT (Is Event) (WithMetaData Event) m Void
  continue metadata =
    forever $
      await >>= \ev -> yield (WithMetaData ev metadata)

{- |
Drop the t`WithMetaData` wrapper.
-}
dropMetaData :: Process (WithMetaData a) a
dropMetaData = mapping (.value)


--------------------------------------------------------------------------------
-- Init Events
--------------------------------------------------------------------------------

{- |
Is this event an init event?

__Warning:__ This function returns @True@ for `E.UserBinaryMessage`, which is
not an init event, but may be used as such by @eventlog-socket@ plugins.
-}
isInitEvent :: Event -> Bool
isInitEvent = isInitEventInfo . E.evSpec
 where
  isInitEventInfo :: EventInfo -> Bool
  isInitEventInfo E.InfoTableProv{} = True
  isInitEventInfo E.HeapInfoGHC{} = True
  isInitEventInfo E.HeapProfCostCentre{} = True
  isInitEventInfo E.OsProcessPid{} = True
  isInitEventInfo E.OsProcessParentPid{} = True
  isInitEventInfo E.RtsIdentifier{} = True
  isInitEventInfo E.ProgramArgs{} = True
  isInitEventInfo E.ProgramEnv{} = True
  isInitEventInfo E.UserBinaryMessage{} = True
  isInitEventInfo _ = False


--------------------------------------------------------------------------------
-- Partial Program Metadata
--------------------------------------------------------------------------------

{- |
Partial program metadata.
-}
data PartialMetaData = PartialMetaData
  { maybeStartTimeUnixNano :: Maybe TimeUnixNano
  -- ^ The program start time in nanoseconds since the Unix epoch
  , maybeProcessId :: Maybe PID
  -- ^ The program's process ID.
  , maybeParentProcessId :: Maybe PID
  -- ^ The program's parent process ID.
  , maybeProgramArgs :: Maybe [Text]
  -- ^ The program's command-line arguments.
  , maybeProgramEnv :: Maybe [Text]
  {- ^ The program's environment.

  This event is not emitted on any recent GHC build, but may be emitted by the user.
  -}
  , maybeRtsIdentifier :: Maybe Text
  -- ^ The identifier for the runtime system.
  }

instance Semigroup PartialMetaData where
  p1 <> p2 =
    PartialMetaData
      { maybeStartTimeUnixNano =
          forceMaybe $ p1.maybeStartTimeUnixNano <|> p2.maybeStartTimeUnixNano
      , maybeProcessId =
          forceMaybe $ p1.maybeProcessId <|> p2.maybeProcessId
      , maybeParentProcessId =
          forceMaybe $ p1.maybeParentProcessId <|> p2.maybeParentProcessId
      , maybeProgramArgs =
          forceMaybe $ p1.maybeProgramArgs <|> p2.maybeProgramArgs
      , maybeProgramEnv =
          forceMaybe $ p1.maybeProgramEnv <|> p2.maybeProgramEnv
      , maybeRtsIdentifier =
          forceMaybe $ p1.maybeRtsIdentifier <|> p2.maybeRtsIdentifier
      }
   where
    -- NOTE: Without this function, `min` creates a space leak.
    forceMaybe :: Maybe a -> Maybe a
    forceMaybe Nothing = Nothing
    forceMaybe (Just a) = a `seq` Just a

instance Monoid PartialMetaData where
  mempty =
    PartialMetaData
      { maybeStartTimeUnixNano = Nothing
      , maybeProcessId = Nothing
      , maybeParentProcessId = Nothing
      , maybeProgramArgs = Nothing
      , maybeProgramEnv = Nothing
      , maybeRtsIdentifier = Nothing
      }

{- |
Convert `PartialMetaData` to `MetaData`.
-}
toMetaData :: PartialMetaData -> Maybe MetaData
toMetaData PartialMetaData{..} = do
  startTimeUnixNano <- maybeStartTimeUnixNano
  processId <- maybeProcessId
  let parentProcessId = maybeParentProcessId
  programArgs <- maybeProgramArgs
  let programEnv = maybeProgramEnv
  rtsIdentifier <- maybeRtsIdentifier
  pure MetaData{..}
