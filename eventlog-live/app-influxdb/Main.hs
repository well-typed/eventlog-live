module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((^.))
import Control.Lens.Setter (set)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Kind (Constraint, Type)
import Data.List (uncons)
import Data.Machine.Is (Is)
import Data.Machine.Plan (PlanT (..), await, yield)
import Data.Machine.Process (ProcessT, (~>))
import Data.Machine.Type (construct, repeatedly)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Word (Word32, Word64, Word8)
import Database.InfluxDB.Line qualified as I (Line (..))
import Database.InfluxDB.Types qualified as I
import Database.InfluxDB.Write qualified as I
import GHC.Eventlog.Live
import GHC.Eventlog.Live.Machines (batchByTick)
import GHC.Eventlog.Live.Options
import GHC.RTS.Events (Event (..), EventInfo (..), HeapProfBreakdown (..), ThreadStopStatus (..))
import GHC.RTS.Events.Analysis qualified as Analysis
import GHC.RTS.Events.Analysis.Thread (ThreadState (..))
import GHC.RTS.Events.Analysis.Thread qualified as Analysis.Thread
import GHC.Stack (HasCallStack)
import Options.Applicative qualified as O
import System.Clock (TimeSpec)
import System.Clock qualified as Clock
import System.IO qualified as IO
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main :: IO ()
main = do
  Options{..} <- O.execParser optionsInfo
  let toInfluxDB =
        processEventsTick maybeHeapProfBreakdown
          ~> batchByTick
          ~> influxDBWriter influxDBWriteParams
  runWithEventlogSocket
    batchInterval
    Nothing -- chunk size (bytes)
    eventlogSocket
    maybeEventlogLogFile
    toInfluxDB

--------------------------------------------------------------------------------
-- Writing to InfluxDB
--------------------------------------------------------------------------------

type Line = I.Line Clock.TimeSpec

data Metric (tagOrField :: Type -> Constraint)
  = forall a. (tagOrField a) => I.Key := a

class Tag t where
  toTag :: t -> Maybe I.Key

instance (Tag t) => Tag (Maybe t) where
  toTag :: Maybe t -> Maybe I.Key
  toTag = (toTag =<<)

instance Tag I.Key where
  toTag :: I.Key -> Maybe I.Key
  toTag = Just

instance Tag Int where
  toTag :: Int -> Maybe I.Key
  toTag = toTag . show

instance Tag Word8 where
  toTag :: Word8 -> Maybe I.Key
  toTag = toTag . show

instance Tag Word32 where
  toTag :: Word32 -> Maybe I.Key
  toTag = toTag . show

instance Tag Text where
  toTag :: Text -> Maybe I.Key
  toTag = toTag . I.Key

instance Tag String where
  toTag :: String -> Maybe I.Key
  toTag = toTag . T.pack

toTagSet :: [Metric Tag] -> Map I.Key I.Key
toTagSet kts = M.fromList (mapMaybe (\(k := t) -> (k,) <$> toTag t) kts)

class Field f where
  toField :: f -> Maybe (I.Field 'I.NonNullable)

instance (Field f) => Field (Maybe f) where
  toField :: Maybe f -> Maybe (I.Field 'I.NonNullable)
  toField = (toField =<<)

integralToField :: (Integral i) => i -> I.Field 'I.NonNullable
integralToField = I.FieldInt . fromIntegral

instance Field Int where
  toField :: Int -> Maybe (I.Field 'I.NonNullable)
  toField = Just . integralToField

instance Field Word32 where
  toField :: Word32 -> Maybe (I.Field 'I.NonNullable)
  toField = Just . integralToField

instance Field Word64 where
  toField :: Word64 -> Maybe (I.Field 'I.NonNullable)
  toField = Just . integralToField

instance Field String where
  toField :: String -> Maybe (I.Field 'I.NonNullable)
  toField = Just . I.FieldString . T.pack

instance Field Text where
  toField :: Text -> Maybe (I.Field 'I.NonNullable)
  toField = Just . I.FieldString

instance Field ThreadState where
  toField :: ThreadState -> Maybe (I.Field 'I.NonNullable)
  toField = toField . show

toFieldSet :: [Metric Field] -> Map I.Key (I.Field 'I.NonNullable)
toFieldSet kvs = M.fromList (mapMaybe (\(k := v) -> (k,) <$> toField v) kvs)

line :: I.Measurement -> [Metric Tag] -> [Metric Field] -> Maybe TimeSpec -> Line
line measurement tagSet fieldSet = I.Line measurement (toTagSet tagSet) (toFieldSet fieldSet)

--------------------------------------------------------------------------------
-- Measurements

data EventProcessorState = EventProcessorState
  { startRealtime :: Maybe Clock.TimeSpec
  , currentHeapProfBreakdown :: Maybe HeapProfBreakdown
  , currentHeapProfSampleEraStack :: [Word64]
  , warnIfMissingHeapProfBreakdown :: Bool
  , threadLabels :: IntMap Text
  }

newEventProcessorState :: Maybe HeapProfBreakdown -> EventProcessorState
newEventProcessorState maybeHeapProfBreakdown =
  EventProcessorState
    { startRealtime = Nothing
    , currentHeapProfBreakdown = maybeHeapProfBreakdown
    , currentHeapProfSampleEraStack = []
    , warnIfMissingHeapProfBreakdown = True
    , threadLabels = IM.empty
    }

-- | Get whether or not an event is a thread event.
isThreadEvent :: EventInfo -> Bool
isThreadEvent = Analysis.alpha Analysis.Thread.threadMachine

{- | Determine the thread state after a thread event.

This function should satisfy the following properties:

prop> isThreadEvent evSpec ==> isJust (getThreadState evSpec)
prop> Analysis.delta threadMachine threadState evSpec == getThreadState evSpec
-}
getThreadState :: (HasCallStack) => EventInfo -> Maybe Analysis.Thread.ThreadState
getThreadState = \case
  CreateThread{} -> Just ThreadQueued
  RunThread{} -> Just ThreadRunning
  WakeupThread{} -> Just ThreadQueued
  StopThread{status = StackOverflow} -> Just ThreadQueued
  StopThread{status = HeapOverflow} -> Just ThreadQueued
  StopThread{status = ForeignCall} -> Just ThreadQueued
  StopThread{status = ThreadFinished} -> Just ThreadFinal
  StopThread{} -> Just ThreadStopped
  _ -> Nothing

processEventsTick :: forall m. (MonadIO m) => Maybe HeapProfBreakdown -> ProcessT m (Tick Event) (Tick Line)
processEventsTick = construct . go . newEventProcessorState
 where
  go :: (MonadIO m) => EventProcessorState -> PlanT (Is (Tick Event)) (Tick Line) m ()
  go st@EventProcessorState{..} =
    await >>= \case
      Tick -> yield Tick >> go st
      Item Event{..} -> do
        let emit measurement timestamp tagSet fieldSet =
              yield (Item $ line measurement tagSet fieldSet timestamp) >> go st
        let timestamp = (Clock.fromNanoSecs (toInteger evTime) +) <$> startRealtime
        case evSpec of
          --------------------------------------------------------------------
          -- RTS Initialisation
          --
          -- This event announces the time at which the process was started.
          WallClockTime{..} -> do
            -- If this coercion overflows, you're running this in the year ~292B.
            -- That's not my problem.
            let secInt = fromIntegral sec :: Int64
            let nsecInt = fromIntegral nsec :: Int64
            go st{startRealtime = Just (Clock.TimeSpec secInt nsecInt)}
          -- This event announces the heap profiling configuration for the process.
          HeapProfBegin{..} -> do
            go st{currentHeapProfBreakdown = Just heapProfBreakdown}

          --------------------------------------------------------------------
          -- Heap Profiling
          --
          -- This event announces the current size of the heap, in bytes,
          -- calculated by how many megablocks are allocated.
          HeapSize{..} -> do
            emit
              "HeapSize"
              timestamp
              [ "heapCapset" := heapCapset
              ]
              [ "sizeBytes" := sizeBytes
              ]
          -- This event announces the current size of the heap, in bytes,
          -- calculated by how many blocks are allocated.
          BlocksSize{..} -> do
            emit
              "BlocksSize"
              timestamp
              [ "heapCapset" := heapCapset
              ]
              [ "blocksSize" := blocksSize
              ]
          -- This event announces that a new chunk of heap has been allocated.
          HeapAllocated{..} -> do
            emit
              "HeapAllocated"
              timestamp
              [ "heapCapset" := heapCapset
              ]
              [ "allocBytes" := allocBytes
              ]
          -- This event announces the current size of the live on-heap data.
          HeapLive{..} -> do
            emit
              "HeapLive"
              timestamp
              [ "heapCapset" := heapCapset
              ]
              [ "liveBytes" := liveBytes
              ]
          -- This event announces the currently allocated megablocks and
          -- attempts made to return them to the operating system.
          -- If your heap is fragmented then the current value will be greater
          -- than needed value but returned will be less than the difference
          -- between the two.
          MemReturn{..} -> do
            emit
              "MemReturn"
              timestamp
              [ "heapCapset" := heapCapset
              ]
              [ "current" := current
              , "needed" := needed
              , "returned" := returned
              ]
          -- This event announces the beginning of a series of heap profiling samples.
          HeapProfSampleBegin{..} -> do
            go st{currentHeapProfSampleEraStack = heapProfSampleEra : currentHeapProfSampleEraStack}
          -- This event announces the end of a series of heap profiling samples.
          HeapProfSampleEnd{..} -> do
            -- Try and remove the current era from the era stack
            case currentHeapProfSampleEraStack of
              -- If there is none...
              [] -> do
                -- ...issue a warning
                liftIO $ warnMismatchedHeapProfSampleEras heapProfSampleEra Nothing
                -- ...and continue
                go st
              -- If there is one...
              (currentHeapProfSampleEra : currentHeapProfSampleEraStack') -> do
                -- ...and it is a *different* era...
                unless (heapProfSampleEra == currentHeapProfSampleEra) . liftIO $
                  -- ...issue a warning
                  warnMismatchedHeapProfSampleEras heapProfSampleEra (Just currentHeapProfSampleEra)
                -- ...and continue regardless
                go st{currentHeapProfSampleEraStack = currentHeapProfSampleEraStack'}
          -- This event announces a heap profiling sample.
          HeapProfSampleString{..} -> do
            -- If the heap profile breakdown is not known:
            if isNothing currentHeapProfBreakdown
              then do
                -- ...issue a warning when the first heap profile sample is encountered
                when warnIfMissingHeapProfBreakdown . liftIO $
                  warnMissingHeapProfBreakdown
                -- ...then ignore heap profile samples going forwards
                go st{warnIfMissingHeapProfBreakdown = False}
              else do
                case currentHeapProfBreakdown of
                  Just HeapProfBreakdownClosureType ->
                    emit
                      "HeapProfSample"
                      timestamp
                      [ "heapProfId" := heapProfId
                      , "heapProfLabel" := heapProfLabel
                      ]
                      [ "heapProfResidency" := heapProfResidency
                      , "heapProfSampleEra" := (fst <$> uncons currentHeapProfSampleEraStack)
                      ]
                  _ -> pure ()
                -- liftIO . print $ (heapProfId, heapProfResidency, heapProfLabel)
                go st

          --------------------------------------------------------------------
          -- Thread Profiling
          --
          -- These events announce thread state changes, which we handle all at
          -- once by reducing them to a thread ID and the resulting thread state.
          _ | isThreadEvent evSpec -> do
            for_ (getThreadState evSpec) $ \threadState -> do
              -- NOTE:
              -- The `thread` field selector is a partial function, so this
              -- is only safe because `getThreadState` returned a `Just`.
              let threadId = fromIntegral (thread evSpec) :: Int
              emit
                "Capability"
                timestamp
                [ "threadId" := threadId
                , "threadLabel" := IM.lookup threadId threadLabels
                , "evCap" := evCap
                ]
                [ "state" := threadState
                ]
            go st

          -- This event announces a thread name.
          ThreadLabel{..} -> do
            let threadId = fromIntegral thread :: Int
            go st{threadLabels = IM.insert threadId threadlabel threadLabels}

          -- This event announces the start of a GC.
          StartGC{} -> do
            emit
              "Capability"
              timestamp
              [ "evCap" := evCap
              ]
              [ "state" := ("GCRunning" :: Text)
              ]
            go st

          -- This event announces the end of a GC.
          EndGC{} -> do
            emit
              "Capability"
              timestamp
              [ "evCap" := evCap
              ]
              [ "state" := ("GCStopped" :: Text)
              ]
            go st

          --------------------------------------------------------------------
          -- The remaining events are ignored.
          _ignored -> go st

--------------------------------------------------------------------------------
-- Warnings

warnMismatchedHeapProfSampleEras :: Word64 -> Maybe Word64 -> IO ()
warnMismatchedHeapProfSampleEras endEra = \case
  Nothing ->
    warn $ printf "Warning: Eventlog closed era %d, but there is no current era." endEra
  Just currentEra ->
    warn $ printf "Warning: Eventlog closed era %d, but the current era is era %d." endEra currentEra

warn :: String -> IO ()
warn = IO.hPutStrLn IO.stderr

--------------------------------------------------------------------------------
-- InfluxDB Batch Writer

influxDBWriter :: I.WriteParams -> ProcessT IO [Line] Void
influxDBWriter writeParams = repeatedly go
 where
  go =
    await >>= \batch ->
      unless (null batch) $ do
        liftIO (I.writeBatch writeParams batch)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

optionsInfo :: O.ParserInfo Options
optionsInfo = O.info (optionsParser O.<**> O.helper) O.idm

data Options = Options
  { eventlogSocket :: EventlogSocket
  , batchInterval :: Int
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , influxDBWriteParams :: I.WriteParams
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSocketParser
    <*> batchIntervalParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> influxDBWriteParamsParser

--------------------------------------------------------------------------------
-- InfluxDB Configuration

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
