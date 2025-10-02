{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : GHC.Eventlog.Live.InfluxDB
Description : The implementation of @eventlog-live-influxdb@.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.InfluxDB (
  main,
) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (asParts, mapping, (<~))
import Data.Machine.Fanout (fanout)
import Data.Machine.Plan (await)
import Data.Machine.Process (ProcessT, (~>))
import Data.Machine.Type (repeatedly)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Data.Void (Void)
import Data.Word (Word32, Word64)
import Database.InfluxDB.Line qualified as I (Line (..))
import Database.InfluxDB.Types qualified as I
import Database.InfluxDB.Write qualified as I
import GHC.Eventlog.Live.Data.Attribute
import GHC.Eventlog.Live.Data.Metric
import GHC.Eventlog.Live.Data.Span
import GHC.Eventlog.Live.Machine
import GHC.Eventlog.Live.Machine.Core
import GHC.Eventlog.Live.Options
import GHC.Eventlog.Live.Socket
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..))
import Lens.Family2 (set, (^.))
import Options.Applicative qualified as O
import Options.Applicative.Extra qualified as O (helperWith)
import Paths_eventlog_live_influxdb qualified as EventlogLive
import System.Clock (TimeSpec, fromNanoSecs)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

{- |
The main function for @eventlog-live-influxdb@.
-}
main :: IO ()
main = do
  Options{..} <- O.execParser optionsInfo
  let toInfluxDB =
        liftTick withStartTime
          ~> sortByBatchTick (.value.evTime)
          ~> liftTick
            ( fanout
                [ processThreadEvents verbosity
                , processHeapEvents verbosity maybeHeapProfBreakdown
                ]
            )
          ~> batchByTick
          ~> mapping D.toList
          ~> influxDBWriter influxDBWriteParams
  runWithEventlogSource
    verbosity
    eventlogSource
    eventlogSocketTimeout
    eventlogSocketTimeoutExponent
    batchInterval
    Nothing -- chunk size (bytes)
    maybeEventlogLogFile
    toInfluxDB

--------------------------------------------------------------------------------
-- Thread events
--------------------------------------------------------------------------------

data OneOf a b c = A !a | B !b | C !c

processThreadEvents ::
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (WithStartTime Event) (DList (I.Line TimeSpec))
processThreadEvents verbosity =
  fanout
    [ fanout
        [ -- GCSpan
          processGCSpans verbosity
            ~> mapping (D.singleton . A)
        , processThreadStateSpans' tryGetTimeUnixNano (.value) setWithStartTime'value verbosity
            ~> fanout
              [ -- MutatorSpan
                asMutatorSpans' (.value) setWithStartTime'value
                  ~> mapping (D.singleton . B)
              , -- ThreadStateSpan
                mapping (D.singleton . C)
              ]
        ]
        ~> asParts
        ~> mapping repackCapabilityUsageSpanOrThreadStateSpan
        ~> fanout
          [ mapping leftToMaybe
              ~> asParts
              ~> fanout
                [ -- CapabilityUsageMetric
                  processCapabilityUsageMetrics
                    ~> mapping (D.singleton . fromMetric "CapabilityUsage")
                , -- CapabilityUsageSpan
                  mapping (D.singleton . fromSpan . (.value))
                ]
          , -- ThreadStateSpan
            mapping rightToMaybe
              ~> asParts
              ~> mapping (D.singleton . fromSpan . (.value))
          ]
    , -- ThreadLabel
      processThreadLabels
        ~> mapping (D.singleton . fromThreadLabel)
    ]
 where
  repackCapabilityUsageSpanOrThreadStateSpan = \case
    A i -> Left $ fmap Left i
    B i -> Left $ fmap Right i
    C i -> Right i

{- |
Internal helper.
Get the `Left` value, if any.
-}
leftToMaybe :: Either a b -> Maybe a
leftToMaybe = either Just (const Nothing)

{- |
Internal helper.
Get the `Right` value, if any.
-}
rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

--------------------------------------------------------------------------------
-- Heap events
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Verbosity ->
  Maybe HeapProfBreakdown ->
  ProcessT m (WithStartTime Event) (DList (I.Line TimeSpec))
processHeapEvents verbosity maybeHeapProfBreakdown =
  fanout
    [ mapping (D.singleton . fromMetric "HeapAllocated")
        <~ processHeapAllocatedData
    , mapping (D.singleton . fromMetric "HeapSize")
        <~ processHeapSizeData
    , mapping (D.singleton . fromMetric "BlocksSize")
        <~ processBlocksSizeData
    , mapping (D.singleton . fromMetric "HeapLive")
        <~ processHeapLiveData
    , mapping
        ( \i ->
            D.fromList
              [ fromMetric "MemCurrent" ((.current) <$> i)
              , fromMetric "MemNeeded" ((.needed) <$> i)
              , fromMetric "MemReturned" ((.returned) <$> i)
              ]
        )
        <~ processMemReturnData
    , mapping (D.singleton . fromMetric "HeapProfSample")
        <~ processHeapProfSampleData verbosity maybeHeapProfBreakdown
    ]

--------------------------------------------------------------------------------
-- Interpreting metadata
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Interpreting spans

class FromSpan v where
  fromSpan :: v -> I.Line TimeSpec

toTag :: (Show a) => a -> I.Key
toTag = I.Key . T.pack . show

--------------------------------------------------------------------------------
-- Interpret capability usage spans

instance FromSpan CapabilityUsageSpan where
  fromSpan :: CapabilityUsageSpan -> I.Line TimeSpec
  fromSpan i =
    I.Line "CapabilityUsageSpan" tagSet fieldSet timestamp
   where
    tagSet =
      M.fromList
        [ ("capability", toTag i.cap)
        ]
    fieldSet =
      M.fromList
        [ ("duration", toField $ duration i)
        , ("category", toField $ showCapabilityUserCategory user)
        , ("user", toField $ show user)
        ]
     where
      user = capabilityUser i
    timestamp = Just . fromNanoSecs . toInteger $ i.startTimeUnixNano

--------------------------------------------------------------------------------
-- Interpret thread state spans

instance FromSpan ThreadStateSpan where
  fromSpan :: ThreadStateSpan -> I.Line TimeSpec
  fromSpan i =
    I.Line "ThreadStateSpan" tagSet fieldSet timestamp
   where
    tagSet =
      M.fromList
        [ ("thread", toTag i.thread)
        ]
    fieldSet =
      M.fromList . catMaybes $
        [ Just ("duration", toField $ duration i)
        , Just ("category", toField $ showThreadStateCategory i.threadState)
        , ("capability",) . toField <$> threadStateCap i.threadState
        , ("status",) . toField . show <$> threadStateStatus i.threadState
        ]
    timestamp = Just . fromNanoSecs . toInteger $ i.startTimeUnixNano

--------------------------------------------------------------------------------
-- Interpreting attributes

class IsField v where
  toField :: v -> I.Field 'I.NonNullable

instance IsField String where
  toField :: String -> I.Field 'I.NonNullable
  toField = toField . T.pack

instance IsField Text where
  toField :: Text -> I.Field 'I.NonNullable
  toField = I.FieldString

instance IsField Int where
  toField :: Int -> I.Field 'I.NonNullable
  toField = I.FieldInt . fromIntegral

instance IsField Double where
  toField :: Double -> I.Field 'I.NonNullable
  toField = I.FieldFloat

instance IsField Word32 where
  toField :: Word32 -> I.Field 'I.NonNullable
  toField = I.FieldInt . fromIntegral

-- | __Warning__: This instance may cause overflow.
instance IsField Word64 where
  toField :: Word64 -> I.Field 'I.NonNullable
  toField = I.FieldInt . fromIntegral

fromThreadLabel :: ThreadLabel -> I.Line TimeSpec
fromThreadLabel i =
  I.Line "ThreadLabel" tagSet fieldSet timestamp
 where
  thread = I.Key . T.pack . show $ i.thread
  tagSet = M.singleton "thread" thread
  fieldSet = M.singleton "label" (toField . show $ i.threadlabel)
  timestamp = Just . fromNanoSecs . toInteger $ i.startTimeUnixNano

fromMetric :: (IsField v) => I.Measurement -> Metric v -> I.Line TimeSpec
fromMetric measurement@(I.Measurement measurementName) i =
  I.Line measurement tagSet fieldSet timestamp
 where
  tagSet = M.fromList (mapMaybe (\(k, v) -> (I.Key k,) <$> fromAttrValue v) i.attr)
  fieldSet = M.singleton (I.Key measurementName) (toField i.value)
  timestamp = fromNanoSecs . toInteger <$> i.maybeTimeUnixNano

fromAttrValue :: AttrValue -> Maybe I.Key
fromAttrValue = \case
  AttrInt v -> Just . fromString . show $ v
  AttrInt8 v -> Just . fromString . show $ v
  AttrInt16 v -> Just . fromString . show $ v
  AttrInt32 v -> Just . fromString . show $ v
  AttrInt64 v -> Just . fromString . show $ v
  AttrWord v -> Just . fromString . show $ v
  AttrWord8 v -> Just . fromString . show $ v
  AttrWord16 v -> Just . fromString . show $ v
  AttrWord32 v -> Just . fromString . show $ v
  AttrWord64 v -> Just . fromString . show $ v
  AttrDouble v -> Just . fromString . show $ v
  AttrText v -> Just . I.Key $ v
  AttrNull -> Nothing

--------------------------------------------------------------------------------
-- InfluxDB Batch Writer
--------------------------------------------------------------------------------

influxDBWriter :: I.WriteParams -> ProcessT IO [I.Line TimeSpec] Void
influxDBWriter writeParams = repeatedly go
 where
  go =
    await >>= \batch -> do
      unless (null batch) $ do
        liftIO (I.writeBatch writeParams batch)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

optionsInfo :: O.ParserInfo Options
optionsInfo =
  O.info
    ( optionsParser
        O.<**> O.helperWith (O.long "help" <> O.help "Show this help text.")
        O.<**> O.simpleVersioner (showVersion EventlogLive.version)
    )
    O.idm

data Options = Options
  { eventlogSource :: EventlogSource
  , eventlogSocketTimeout :: Double
  , eventlogSocketTimeoutExponent :: Double
  , batchInterval :: Int
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , verbosity :: Verbosity
  , influxDBWriteParams :: I.WriteParams
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> eventlogSourceParser
    <*> eventlogSocketTimeoutParser
    <*> eventlogSocketTimeoutExponentParser
    <*> batchIntervalParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> verbosityParser
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
