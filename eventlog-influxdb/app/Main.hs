{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((^.))
import Control.Lens.Setter (set)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Int (Int64)
import Data.Machine.Is (Is)
import Data.Machine.Plan (PlanT (..), await, yield)
import Data.Machine.Process (ProcessT, (~>))
import Data.Machine.Runner (runT_)
import Data.Machine.Type (construct, repeatedly)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32, Word64)
import Database.InfluxDB.Line qualified as I (Line (..))
import Database.InfluxDB.Types qualified as I
import Database.InfluxDB.Write qualified as I
import GHC.Eventlog.Machines (Tick (..), batchByTick, decodeEventsTick, sourceHandleInterval)
import GHC.RTS.Events (Event (..), EventInfo (..))
import Network.Socket qualified as S
import Options.Applicative qualified as O
import System.Clock (TimeSpec)
import System.Clock qualified as Clock
import System.IO (Handle)
import System.IO qualified as IO
import Data.Kind (Type, Constraint)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main :: IO ()
main = do
  Options{..} <- O.execParser optionsInfo
  let BatchOptions{..} = batchOptions
  eventlogHandle <- connect eventlogSocket
  runT_ $
    sourceHandleInterval batchIntervalMs defaultChunkSizeBytes eventlogHandle
      ~> decodeEventsTick
      ~> processEventsTick
      ~> batchByTick
      ~> influxDBWriter influxDBWriteParams

{- |
Eventlog chunk size in bytes.
This should be equal to the page size.
-}
defaultChunkSizeBytes :: Int
defaultChunkSizeBytes = 4096

--------------------------------------------------------------------------------
-- Writing to InfluxDB
--------------------------------------------------------------------------------

type Line = I.Line Clock.TimeSpec

data TagOrField = Tag | Field

class Tag t where
  toTag :: t -> I.Key

class Field f where
  toField :: f -> I.Field 'I.NonNullable

instance (Integral i) => Field i where
  toField :: (Integral i) => i -> I.Field 'I.NonNullable
  toField = I.FieldInt . fromIntegral

data Metric (tagOrField :: Type -> Constraint) = forall a. (tagOrField a) => I.Key := a

toTagSet :: [Metric Tag] -> Map I.Key I.Key
toTagSet kts = Map.fromList (fmap (\(k := t) -> (k, toTag t)) kts)

toFieldSet :: [Metric Field] -> Map I.Key (I.Field 'I.NonNullable)
toFieldSet kvs = Map.fromList (fmap (\(k := v) -> (k, toField v)) kvs)

line :: I.Measurement -> [Metric Tag] -> [Metric Field] -> Maybe TimeSpec -> Line
line measurement tagSet fieldSet = I.Line measurement (toTagSet tagSet) (toFieldSet fieldSet)

--------------------------------------------------------------------------------
-- Measurements

newtype EventProcessorState = EventProcessorState
  { startRealtime :: Maybe Clock.TimeSpec
  }

newEventProcessorState :: EventProcessorState
newEventProcessorState =
  EventProcessorState
    { startRealtime = Nothing
    }

processEventsTick :: forall m. (MonadIO m) => ProcessT m (Tick Event) (Tick Line)
processEventsTick = construct (go newEventProcessorState)
 where
  go :: (MonadIO m) => EventProcessorState -> PlanT (Is (Tick Event)) (Tick Line) m ()
  go st@EventProcessorState{..} =
    await >>= \case
      Tick -> yield Tick >> go st
      Item Event{..} -> do
        let timestamp = (Clock.fromNanoSecs (toInteger evTime) +) <$> startRealtime
        let emit measurement tagSet fieldSet =
              yield (Item $ line measurement tagSet fieldSet timestamp) >> go st
        case evSpec of
          --------------------------------------------------------------------
          -- RTS Initialisation
          --
          -- This event announces the time at which the process was started.
          WallClockTime{..} -> do
            -- If this coercion overflows, you're running this in the year ~292B.
            -- That's not my problem.
            let secInt = fromIntegral @Word64 @Int64 sec
            let nsecInt = fromIntegral @Word32 @Int64 nsec
            go st{startRealtime = Just (Clock.TimeSpec secInt nsecInt)}
          --------------------------------------------------------------------
          -- Heap Profiling
          --
          -- This event announces the current size of the heap, in bytes,
          -- calculated by how many megablocks are allocated.
          HeapSize{..} -> do
            emit "HeapSize" [] $
              [ "heapCapset" := heapCapset
              , "sizeBytes" := sizeBytes
              ]
          -- This event announces the current size of the heap, in bytes,
          -- calculated by how many blocks are allocated.
          BlocksSize{..} -> do
            emit "BlocksSize" [] $
              [ "heapCapset" := heapCapset
              , "blocksSize" := blocksSize
              ]
          -- This event announces that a new chunk of heap has been allocated.
          HeapAllocated{..} -> do
            emit "HeapAllocated" [] $
              [ "heapCapset" := heapCapset
              , "allocBytes" := allocBytes
              ]
          -- This event announces the current size of the live on-heap data.
          HeapLive{..} -> do
            emit "HeapLive" [] $
              [ "heapCapset" := heapCapset
              , "liveBytes" := liveBytes
              ]
          -- This event announces the currently allocated megablocks and
          -- attempts made to return them to the operating system.
          -- If your heap is fragmented then the current value will be greater
          -- than needed value but returned will be less than the difference
          -- between the two.
          MemReturn{..} -> do
            emit "MemReturn" [] $
              [ "heapCapset" := heapCapset
              , "current" := current
              , "needed" := needed
              , "returned" := returned
              ]
          -- This event announces a heap profiling sample.
          HeapProfSampleString{..} -> do
            liftIO . print $ (heapProfId, heapProfResidency, heapProfLabel)
            go st

          --------------------------------------------------------------------
          -- The remaining events are ignored.
          _ignored -> go st

--------------------------------------------------------------------------------
-- InfluxDB Batch Writer

influxDBWriter :: I.WriteParams -> ProcessT IO [Line] ()
influxDBWriter writeParams = repeatedly go
 where
  go =
    await >>= \batch ->
      unless (null batch) $ do
        liftIO (I.writeBatch writeParams batch)

--------------------------------------------------------------------------------
-- Connecting to the eventlog socket
--------------------------------------------------------------------------------

connect :: EventlogSocket -> IO Handle
connect = \case
  EventlogSocketUnix socketName -> do
    socket <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
    S.connect socket (S.SockAddrUnix socketName)
    handle <- S.socketToHandle socket IO.ReadMode
    IO.hSetBuffering handle IO.NoBuffering
    pure handle

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

optionsInfo :: O.ParserInfo Options
optionsInfo = O.info (optionsParser O.<**> O.helper) O.idm

data Options = Options
  { eventlogSocket :: EventlogSocket
  , batchOptions :: BatchOptions
  , influxDBWriteParams :: I.WriteParams
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSocketParser
    <*> batchOptionsParser
    <*> influxDBWriteParamsParser

newtype EventlogSocket
  = EventlogSocketUnix FilePath

eventlogSocketParser :: O.Parser EventlogSocket
eventlogSocketParser = socketUnixParser
 where
  socketUnixParser =
    EventlogSocketUnix
      <$> O.strOption
        ( O.long "eventlog-socket"
            <> O.metavar "SOCKET"
            <> O.help "Eventlog source Unix socket."
        )

newtype BatchOptions = BatchOptions
  { batchIntervalMs :: Int
  }

defaultBatchIntervalMs :: Int
defaultBatchIntervalMs = 1_000

batchOptionsParser :: O.Parser BatchOptions
batchOptionsParser =
  BatchOptions
    <$> O.option
      O.auto
      ( O.long "batch-interval"
          <> O.metavar "BATCH_INTERVAL"
          <> O.help "Batch interval in microseconds."
          <> O.value defaultBatchIntervalMs
      )

influxDBWriteParamsParser :: O.Parser I.WriteParams
influxDBWriteParamsParser = params4
 where
  params0 = I.writeParams <$> influxDBDatabaseParser
  params1 = set I.server <$> influxDBServerParser <*> params0
  params2 = set I.retentionPolicy <$> influxDBRetentionPolicyParser <*> params1
  params3 = set I.authentication <$> influxDBCredentialsParser <*> params2
  params4 = set I.precision I.Nanosecond <$> params3

influxDBDatabaseParser :: O.Parser I.Database
influxDBDatabaseParser =
  O.strOption
    ( O.long "influxdb-database"
        <> O.metavar "DATABASE"
        <> O.help "InfluxDB database name"
    )

influxDBServerParser :: O.Parser I.Server
influxDBServerParser =
  I.Server
    <$> ( O.strOption
            ( O.long "influxdb-host"
                <> O.metavar "HOST"
                <> O.help "InfluxDB server host"
            )
            <|> pure (I.defaultServer ^. I.host)
        )
    <*> ( O.option
            O.auto
            ( O.long "influxdb-port"
                <> O.metavar "HOST"
                <> O.help "InfluxDB server host"
            )
            <|> pure (I.defaultServer ^. I.port)
        )
    <*> ( O.flag
            False
            True
            ( O.long "influxdb-ssl"
                <> O.help "InfluxDB server SSL"
            )
            <|> pure (I.defaultServer ^. I.ssl)
        )

influxDBRetentionPolicyParser :: O.Parser (Maybe I.Key)
influxDBRetentionPolicyParser =
  O.optional
    ( O.strOption
        ( O.long "influxdb-retention-policy"
            <> O.metavar "RETENTION_POLICY"
            <> O.help "InfluxDB retention policy"
        )
    )

influxDBCredentialsParser :: O.Parser (Maybe I.Credentials)
influxDBCredentialsParser =
  O.optional
    ( I.Credentials
        <$> O.strOption
          ( O.long "influxdb-username"
              <> O.metavar "USERNAME"
              <> O.help "InfluxDB username"
          )
        <*> O.strOption
          ( O.long "influxdb-password"
              <> O.metavar "PASSWORD"
              <> O.help "InfluxDB password"
          )
    )
