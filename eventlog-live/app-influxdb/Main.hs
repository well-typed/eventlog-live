module Main where

import Control.Applicative (Alternative ((<|>)))
import Control.Lens ((^.))
import Control.Lens.Setter (set)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Machine (mapping, (<~))
import Data.Machine.Fanout (fanout)
import Data.Machine.Plan (await)
import Data.Machine.Process (ProcessT, (~>))
import Data.Machine.Type (repeatedly)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Void (Void)
import Data.Word (Word32, Word64)
import Database.InfluxDB.Line qualified as I (Line (..))
import Database.InfluxDB.Types qualified as I
import Database.InfluxDB.Write qualified as I
import GHC.Eventlog.Live
import GHC.Eventlog.Live.Machines
import GHC.Eventlog.Live.Options
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..))
import Options.Applicative qualified as O
import System.Clock (TimeSpec, fromNanoSecs)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

main :: IO ()
main = do
  Options{..} <- O.execParser optionsInfo
  let toInfluxDB =
        liftTick withStartTime
          ~> liftTick (processHeapEvents maybeHeapProfBreakdown)
          ~> batchByTick
          ~> mapping D.toList
          ~> influxDBWriter influxDBWriteParams
  runWithEventlogSocket
    batchInterval
    Nothing -- chunk size (bytes)
    eventlogSocket
    maybeEventlogLogFile
    toInfluxDB

--------------------------------------------------------------------------------
-- Heap events
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Maybe HeapProfBreakdown ->
  ProcessT m (WithStartTime Event) (DList (I.Line TimeSpec))
processHeapEvents maybeHeapProfBreakdown =
  fanout
    [ mapping (D.singleton . toLine "HeapAllocated")
        <~ processHeapAllocatedData
    , mapping (D.singleton . toLine "HeapSize")
        <~ processHeapSizeData
    , mapping (D.singleton . toLine "BlocksSize")
        <~ processBlocksSizeData
    , mapping (D.singleton . toLine "HeapLive")
        <~ processHeapLiveData
    , mapping
        ( \i ->
            D.fromList
              [ toLine "MemCurrent" ((.current) <$> i)
              , toLine "MemNeeded" ((.needed) <$> i)
              , toLine "MemReturned" ((.returned) <$> i)
              ]
        )
        <~ processMemReturnData
    , mapping (D.singleton . toLine "HeapProfSample")
        <~ processHeapProfSampleData maybeHeapProfBreakdown
    ]

--------------------------------------------------------------------------------
-- Interpreting metadata
--------------------------------------------------------------------------------

class IsField v where
  toField :: v -> I.Field 'I.NonNullable

instance IsField Word32 where
  toField :: Word32 -> I.Field 'I.NonNullable
  toField = I.FieldInt . fromIntegral

-- | __Warning__: This instance may cause overflow.
instance IsField Word64 where
  toField :: Word64 -> I.Field 'I.NonNullable
  toField = I.FieldInt . fromIntegral

toLine :: (IsField v) => I.Measurement -> (WithMeta v) -> I.Line TimeSpec
toLine measurement@(I.Measurement measurementName) i =
  I.Line measurement tagSet fieldSet timestamp
 where
  tagSet = M.fromList (mapMaybe (\(k, v) -> (I.Key k,) <$> toMaybeTag v) i.attr)
  fieldSet = M.singleton (I.Key measurementName) (toField i.value)
  timestamp = fromNanoSecs . toInteger <$> i.maybeTimeUnixNano

  toMaybeTag :: AttrValue -> Maybe I.Key
  toMaybeTag = \case
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
