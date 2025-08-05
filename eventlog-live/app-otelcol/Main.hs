{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (asum)
import Control.Exception (Exception (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.DList (DList)
import Data.DList qualified as D
import Data.Either (isLeft)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List qualified as L
import Data.Machine (Process, ProcessT, await, construct, mapping, repeatedly, yield, (~>))
import Data.Machine.Fanout (fanout)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Data.Void (Void)
import Data.Word (Word32, Word64, Word8)
import GHC.Eventlog.Live (EventlogSocket, runWithEventlogSocket)
import GHC.Eventlog.Live.Lens qualified as EF
import GHC.Eventlog.Live.Machines (Tick, batchByTick, liftTick, setStartTime)
import GHC.Eventlog.Live.Options
import GHC.RTS.Events (Event (..), EventInfo (..), HeapProfBreakdown (..), Timestamp)
import Lens.Family2 ((&), (.~), (^.))
import Lens.Family2.Stock
import Lens.Family2.Unchecked (lens)
import Network.GRPC.Client qualified as G
import Network.GRPC.Client.StreamType.IO qualified as G
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf (Protobuf)
import Network.GRPC.Common.Protobuf qualified as G
import Numeric (showHex)
import Options.Applicative qualified as O
import Options.Applicative.Extra qualified as O
import PackageInfo_eventlog_live qualified as EventlogLive
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService (ExportMetricsServiceRequest, MetricsService)
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Common.V1.Common (AnyValue, InstrumentationScope, KeyValue)
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as C
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics (AggregationTemporality (..), Gauge, Metric, Metric'Data (..), NumberDataPoint, ResourceMetrics, ScopeMetrics, Sum)
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as MF
import System.IO qualified as IO
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
import Text.Printf (HPrintfType, hPrintf, printf)
import Text.Read (readMaybe)
import Text.Read.Lex (readHexP)

main :: IO ()
main = do
  Options{..} <- O.execParser options
  let OpenTelemetryCollectorOptions{..} = openTelemetryCollectorOptions
  G.withConnection G.def openTelemetryCollectorServer $ \conn -> do
    runWithEventlogSocket
      batchInterval
      Nothing
      eventlogSocket
      maybeEventlogLogFile
      $ liftTick (setStartTime evSpec WithContext)
        ~> fanout
          [ processHeapEvents maybeHeapProfBreakdown
          ]
        ~> mapping D.toList
        ~> asScopeMetrics
          [ MF.scope .~ eventlogLiveScope
          ]
        ~> asResourceMetric []
        ~> asExportMetricServiceRequest
        ~> otelcolResourceMetricsExporter conn
        ~> displayExceptions

displayExceptions :: (MonadIO m, Exception e) => ProcessT m e Void
displayExceptions = repeatedly $ await >>= liftIO . IO.hPutStrLn IO.stderr . displayException

data ExportMetricsError
  = ExportMetricsError
  { rejectedDataPoints :: Int64
  , errorMessage :: Text
  }
  deriving (Show)

instance Exception ExportMetricsError where
  displayException :: ExportMetricsError -> String
  displayException ExportMetricsError{..} =
    printf "Error: OpenTelemetry Collector rejected %d data points with message: %s" rejectedDataPoints errorMessage

otelcolResourceMetricsExporter :: G.Connection -> ProcessT IO ExportMetricsServiceRequest ExportMetricsError
otelcolResourceMetricsExporter conn =
  repeatedly $
    await >>= \exportMetricsServiceRequest -> do
      G.Proto resp <- liftIO (G.nonStreaming conn (G.rpc @(Protobuf MetricsService "export")) (G.Proto exportMetricsServiceRequest))
      unless (resp ^. MSF.partialSuccess . MSF.rejectedDataPoints == 0) $ do
        yield
          ExportMetricsError
            { rejectedDataPoints = resp ^. MSF.partialSuccess . MSF.rejectedDataPoints
            , errorMessage = resp ^. MSF.partialSuccess . MSF.errorMessage
            }

type instance G.RequestMetadata (Protobuf MetricsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (Protobuf MetricsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (Protobuf MetricsService meth) = G.NoMetadata

--------------------------------------------------------------------------------
-- Heap Events
--------------------------------------------------------------------------------

processHeapEvents ::
  (MonadIO m) =>
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithContext Event)) (DList Metric)
processHeapEvents maybeHeapProfBreakdown =
  fanout
    [ process ~> mapping D.singleton
    | process <-
        [ processHeapAllocated
        , processBlocksSize
        , processHeapSize
        , processHeapLive
        , processMemCurrent
        , processMemNeeded
        , processMemReturned
        , processHeapProfSample maybeHeapProfBreakdown
        ]
    ]

--------------------------------------------------------------------------------
-- HeapAllocated

processHeapAllocated :: Process (Tick (WithContext Event)) Metric
processHeapAllocated =
  liftTick processHeapAllocatedData
    ~> batchByTick
    ~> asSum
      [ MF.aggregationTemporality .~ AGGREGATION_TEMPORALITY_DELTA
      , MF.isMonotonic .~ True
      ]
    ~> asMetric
      [ MF.name .~ "HeapAllocated"
      , MF.description .~ "Report when a new chunk of heap has been allocated by the indicated capability set."
      , MF.unit .~ "By"
      ]
 where
  processHeapAllocatedData :: Process (WithContext Event) NumberDataPoint
  processHeapAllocatedData =
    repeatedly $
      await >>= \case
        ev
          | HeapAllocated{..} <- ev ^. value . EF.evSpec ->
              yield $
                intDataPointWith ev allocBytes $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapCapset" ~= heapCapset
                  ]
          | otherwise -> pure ()

--------------------------------------------------------------------------------
-- HeapSize

processHeapSize :: Process (Tick (WithContext Event)) Metric
processHeapSize =
  liftTick processHeapSizeData
    ~> batchByTick
    ~> asGauge
      []
    ~> asMetric
      [ MF.name .~ "HeapSize"
      , MF.description .~ "Report the current heap size, calculated by the allocated number of megablocks."
      , MF.unit .~ "By"
      ]
 where
  processHeapSizeData :: Process (WithContext Event) NumberDataPoint
  processHeapSizeData =
    repeatedly $
      await >>= \case
        ev
          | HeapSize{..} <- ev ^. value . EF.evSpec -> do
              yield $
                intDataPointWith ev sizeBytes $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapCapset" ~= heapCapset
                  ]
          | otherwise -> pure ()

--------------------------------------------------------------------------------
-- BlocksSize

processBlocksSize :: Process (Tick (WithContext Event)) Metric
processBlocksSize =
  liftTick processBlocksSizeData
    ~> batchByTick
    ~> asGauge
      []
    ~> asMetric
      [ MF.name .~ "BlocksSize"
      , MF.description .~ "Report the current heap size, calculated by the allocated number of blocks."
      , MF.unit .~ "By"
      ]
 where
  processBlocksSizeData :: Process (WithContext Event) NumberDataPoint
  processBlocksSizeData =
    repeatedly $
      await >>= \case
        ev
          | BlocksSize{..} <- ev ^. value . EF.evSpec -> do
              yield $
                intDataPointWith ev blocksSize $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapCapset" ~= heapCapset
                  ]
          | otherwise -> pure ()

--------------------------------------------------------------------------------
-- HeapLive

processHeapLive :: Process (Tick (WithContext Event)) Metric
processHeapLive =
  liftTick processHeapLiveData
    ~> batchByTick
    ~> asGauge
      []
    ~> asMetric
      [ MF.name .~ "HeapLive"
      , MF.description .~ "Report the current heap size, calculated by the allocated number of megablocks."
      , MF.unit .~ "By"
      ]
 where
  processHeapLiveData :: Process (WithContext Event) NumberDataPoint
  processHeapLiveData =
    repeatedly $
      await >>= \case
        ev
          | HeapLive{..} <- ev ^. value . EF.evSpec -> do
              yield $
                intDataPointWith ev liveBytes $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapCapset" ~= heapCapset
                  ]
          | otherwise -> pure ()

--------------------------------------------------------------------------------
-- MemReturn

processMemCurrent :: Process (Tick (WithContext Event)) Metric
processMemCurrent =
  liftTick processMemCurrentData
    ~> batchByTick
    ~> asGauge
      []
    ~> asMetric
      [ MF.name .~ "MemCurrent"
      , MF.description .~ "Report the number of megablocks currently allocated."
      , MF.unit .~ "{mblock}"
      ]
 where
  processMemCurrentData :: Process (WithContext Event) NumberDataPoint
  processMemCurrentData =
    repeatedly $
      await >>= \case
        ev
          | MemReturn{..} <- ev ^. value . EF.evSpec -> do
              yield $
                intDataPointWith ev current $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapCapset" ~= heapCapset
                  ]
          | otherwise -> pure ()

processMemNeeded :: Process (Tick (WithContext Event)) Metric
processMemNeeded =
  liftTick processMemNeededData
    ~> batchByTick
    ~> asGauge
      []
    ~> asMetric
      [ MF.name .~ "MemNeeded"
      , MF.description .~ "Report the number of megablocks currently needed."
      , MF.unit .~ "{mblock}"
      ]
 where
  processMemNeededData :: Process (WithContext Event) NumberDataPoint
  processMemNeededData =
    repeatedly $
      await >>= \case
        ev
          | MemReturn{..} <- ev ^. value . EF.evSpec -> do
              yield $
                intDataPointWith ev needed $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapCapset" ~= heapCapset
                  ]
          | otherwise -> pure ()

processMemReturned :: Process (Tick (WithContext Event)) Metric
processMemReturned =
  liftTick processMemReturnedData
    ~> batchByTick
    ~> asGauge
      []
    ~> asMetric
      [ MF.name .~ "MemReturned"
      , MF.description .~ "Report the number of megablocks currently being returned to the OS."
      , MF.unit .~ "{mblock}"
      ]
 where
  processMemReturnedData :: Process (WithContext Event) NumberDataPoint
  processMemReturnedData =
    repeatedly $
      await >>= \case
        ev
          | MemReturn{..} <- ev ^. value . EF.evSpec -> do
              yield $
                intDataPointWith ev returned $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapCapset" ~= heapCapset
                  ]
          | otherwise -> pure ()

--------------------------------------------------------------------------------
-- HeapProfSample

newtype InfoTablePtr = InfoTablePtr Word64
  deriving newtype (Eq, Hashable, Ord)

instance Show InfoTablePtr where
  showsPrec :: Int -> InfoTablePtr -> ShowS
  showsPrec _ (InfoTablePtr ptr) =
    showString "0x" . showHex ptr

readInfoTablePtr :: Text -> Maybe InfoTablePtr
readInfoTablePtr = readMaybe . T.unpack

readInfoTablePtrP :: ReadP InfoTablePtr
readInfoTablePtrP = InfoTablePtr <$> (P.string "0x" *> readHexP)

instance Read InfoTablePtr where
  readsPrec :: Int -> ReadS InfoTablePtr
  readsPrec _ = readP_to_S readInfoTablePtrP

data InfoTable = InfoTable
  { infoTablePtr :: InfoTablePtr
  , infoTableName :: Text
  , infoTableClosureDesc :: Int
  , infoTableTyDesc :: Text
  , infoTableLabel :: Text
  , infoTableModule :: Text
  , infoTableSrcLoc :: Text
  }
  deriving (Show)

data HeapProfSampleState = HeapProfSampleState
  { eitherShouldWarnOrHeapProfBreakdown :: Either Bool HeapProfBreakdown
  , infoTableMap :: HashMap InfoTablePtr InfoTable
  , heapProfSampleEraStack :: [Word64]
  }

shouldTrackInfoTableMap :: Either Bool HeapProfBreakdown -> Bool
shouldTrackInfoTableMap (Left _shouldWarn) = True
shouldTrackInfoTableMap (Right HeapProfBreakdownInfoTable) = True
shouldTrackInfoTableMap _ = False

isHeapProfBreakdownInfoTable :: HeapProfBreakdown -> Bool
isHeapProfBreakdownInfoTable HeapProfBreakdownInfoTable = True
isHeapProfBreakdownInfoTable _ = False

processHeapProfSample ::
  (MonadIO m) =>
  Maybe HeapProfBreakdown ->
  ProcessT m (Tick (WithContext Event)) Metric
processHeapProfSample maybeHeapProfBreakdown =
  liftTick (processHeapProfSampleData maybeHeapProfBreakdown)
    ~> batchByTick
    ~> asGauge
      []
    ~> asMetric
      [ MF.name .~ "HeapProfSample"
      , MF.description .~ "Report a heap profile sample."
      , MF.unit .~ "By"
      ]
 where
  processHeapProfSampleData ::
    (MonadIO m) =>
    Maybe HeapProfBreakdown ->
    ProcessT m (WithContext Event) NumberDataPoint
  processHeapProfSampleData maybeHeapProfBreakdown =
    construct $
      go
        HeapProfSampleState
          { eitherShouldWarnOrHeapProfBreakdown = maybe (Left True) Right maybeHeapProfBreakdown
          , infoTableMap = mempty
          , heapProfSampleEraStack = mempty
          }
   where
    go st@HeapProfSampleState{..} = do
      await >>= \ev -> case ev ^. value . EF.evSpec of
        -- Announces the heap profile breakdown, amongst other things.
        -- This event is only emitted for code compiled with GHC >=9.14.
        HeapProfBegin{..}
          | isLeft eitherShouldWarnOrHeapProfBreakdown ->
              go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
        -- Announces the arguments with which the program was called.
        -- This *may* include RTS options, which can be used to determine the
        -- heap profile breakdown for code compiled with GHC <9.14.
        ProgramArgs{..}
          | isLeft eitherShouldWarnOrHeapProfBreakdown
          , Just heapProfBreakdown <- findHeapProfBreakdown args ->
              go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
        -- Announces an info table entry.
        InfoTableProv{..}
          | shouldTrackInfoTableMap eitherShouldWarnOrHeapProfBreakdown -> do
              let infoTablePtr = InfoTablePtr itInfo
                  infoTable =
                    InfoTable
                      { infoTablePtr = infoTablePtr
                      , infoTableName = itTableName
                      , infoTableClosureDesc = itClosureDesc
                      , infoTableTyDesc = itTyDesc
                      , infoTableLabel = itLabel
                      , infoTableModule = itModule
                      , infoTableSrcLoc = itSrcLoc
                      }
              go st{infoTableMap = HM.insert infoTablePtr infoTable infoTableMap}
        -- Announces the beginning of a heap profile sample.
        HeapProfSampleBegin{..} ->
          go st{heapProfSampleEraStack = heapProfSampleEra : heapProfSampleEraStack}
        -- Announces the end of a heap profile sample.
        HeapProfSampleEnd{..} ->
          case L.uncons heapProfSampleEraStack of
            Nothing -> do
              liftIO $
                warnEmptyHeapProfSampleEraStack heapProfSampleEra
              go st
            Just (currentEra, heapProfSampleEraStack') -> do
              unless (currentEra == heapProfSampleEra) . liftIO $
                warnMismatchedHeapProfSampleEraStack heapProfSampleEra currentEra
              go st{heapProfSampleEraStack = heapProfSampleEraStack'}
        -- Announces a heap profile sample.
        HeapProfSampleString{..}
          -- If there is no heap profile breakdown, issue a warning, then disable warnings.
          | Left True <- eitherShouldWarnOrHeapProfBreakdown -> do
              liftIO warnMissingHeapProfBreakdown
              go st{eitherShouldWarnOrHeapProfBreakdown = Left False, infoTableMap = mempty}
          -- If the heap profile breakdown is biographical, issue a warning, then disable warnings.
          | Right HeapProfBreakdownBiography <- eitherShouldWarnOrHeapProfBreakdown -> do
              liftIO $ warnUnsupportedHeapProfBreakdown HeapProfBreakdownBiography
              go st{eitherShouldWarnOrHeapProfBreakdown = Left False, infoTableMap = mempty}
          -- If there is a heap profile breakdown, handle it appropriately.
          | Right heapProfBreakdown <- eitherShouldWarnOrHeapProfBreakdown -> do
              -- If the heap profile breakdown is by info table, add the info table.
              let maybeInfoTable
                    | isHeapProfBreakdownInfoTable heapProfBreakdown = do
                        !infoTablePtr <- readInfoTablePtr heapProfLabel
                        HM.lookup infoTablePtr infoTableMap
                    | otherwise = Nothing
              yield $
                intDataPointWith ev heapProfResidency $
                  [ "evCap" ~= ev ^. value . EF.evCap
                  , "heapProfBreakdown" ~= (heapProfBreakdownShow heapProfBreakdown)
                  , "heapProfId" ~= heapProfId
                  , "heapProfLabel" ~= heapProfLabel
                  , "heapProfSampleEra" ~= (fst <$> L.uncons heapProfSampleEraStack)
                  , "infoTableName" ~= (maybeInfoTable <&> infoTableName)
                  , "infoTableClosureDesc" ~= (maybeInfoTable <&> infoTableClosureDesc)
                  , "infoTableTyDesc" ~= (maybeInfoTable <&> infoTableTyDesc)
                  , "infoTableLabel" ~= (maybeInfoTable <&> infoTableLabel)
                  , "infoTableModule" ~= (maybeInfoTable <&> infoTableModule)
                  , "infoTableSrcLoc" ~= (maybeInfoTable <&> infoTableSrcLoc)
                  ]
              go $ if isHeapProfBreakdownInfoTable heapProfBreakdown then st else st{infoTableMap = mempty}
        _otherwise -> go st

  -- NOTE: This scan is currently flawed, as it does not handle @-with-rtsopts@,
  --       nor does it restrict its search to between @+RTS@ and @-RTS@ tags.
  findHeapProfBreakdown :: [Text] -> Maybe HeapProfBreakdown
  findHeapProfBreakdown = listToMaybe . mapMaybe parseHeapProfBreakdown
   where
    parseHeapProfBreakdown :: Text -> Maybe HeapProfBreakdown
    parseHeapProfBreakdown arg
      | "-h" `T.isPrefixOf` arg =
          either (const Nothing) Just
            . heapProfBreakdownEitherReader
            . T.unpack
            . T.drop 2
            $ arg
      | otherwise = Nothing

  warnEmptyHeapProfSampleEraStack :: Word64 -> IO ()
  warnEmptyHeapProfSampleEraStack =
    warnf
      "Warning: Eventlog closed era %d, but there is no current era."

  warnMismatchedHeapProfSampleEraStack :: Word64 -> Word64 -> IO ()
  warnMismatchedHeapProfSampleEraStack =
    warnf
      "Warning: Eventlog closed era %d, but the current era is era %d."

  warnUnsupportedHeapProfBreakdown :: HeapProfBreakdown -> IO ()
  warnUnsupportedHeapProfBreakdown heapProfBreakdown =
    warnf
      "Warning: Unsupported heap profile breakdown %s"
      (heapProfBreakdownShow heapProfBreakdown)

  warnMissingHeapProfBreakdown :: IO ()
  warnMissingHeapProfBreakdown =
    warnf
      "Warning: Cannot infer heap profile breakdown.\n\
      \         If your binary was compiled with a GHC version prior to 9.14,\n\
      \         you must also pass the heap profile type to this executable.\n\
      \         See: https://gitlab.haskell.org/ghc/ghc/-/commit/76d392a"

--------------------------------------------------------------------------------
-- Machines
--------------------------------------------------------------------------------

eventlogLiveScope :: InstrumentationScope
eventlogLiveScope =
  messageWith
    [ C.name .~ T.pack EventlogLive.name
    , C.version .~ T.pack (showVersion EventlogLive.version)
    ]

asExportMetricsServiceRequest :: Process [ResourceMetrics] ExportMetricsServiceRequest
asExportMetricsServiceRequest = mapping $ (defMessage &) . (MF.resourceMetrics .~)

asExportMetricServiceRequest :: Process ResourceMetrics ExportMetricsServiceRequest
asExportMetricServiceRequest = mapping (: []) ~> asExportMetricsServiceRequest

asResourceMetrics :: [ResourceMetrics -> ResourceMetrics] -> Process [ScopeMetrics] ResourceMetrics
asResourceMetrics mod = mapping $ \metrics ->
  messageWith ((MF.scopeMetrics .~ metrics) : mod)

asResourceMetric :: [ResourceMetrics -> ResourceMetrics] -> Process ScopeMetrics ResourceMetrics
asResourceMetric mod = mapping (: []) ~> asResourceMetrics mod

asScopeMetrics :: [ScopeMetrics -> ScopeMetrics] -> Process [Metric] ScopeMetrics
asScopeMetrics mod = mapping $ \metrics ->
  messageWith ((MF.metrics .~ metrics) : mod)

asMetric :: [Metric -> Metric] -> Process Metric'Data Metric
asMetric mod = mapping $ \metric'data ->
  messageWith ((MF.maybe'data' .~ Just metric'data) : mod)

asGauge :: [Gauge -> Gauge] -> Process [NumberDataPoint] Metric'Data
asGauge mod =
  repeatedly $
    await >>= \case
      dataPoints
        | null dataPoints -> pure ()
        | otherwise -> yield . Metric'Gauge . messageWith $ (MF.dataPoints .~ dataPoints) : mod

asSum :: [Sum -> Sum] -> Process [NumberDataPoint] Metric'Data
asSum mod =
  repeatedly $
    await >>= \case
      dataPoints
        | null dataPoints -> pure ()
        | otherwise -> yield . Metric'Sum . messageWith $ (MF.dataPoints .~ dataPoints) : mod

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

warnf :: (HPrintfType r) => String -> r
warnf = hPrintf IO.stderr

--------------------------------------------------------------------------------
-- Events with Context

data WithContext a = WithContext
  { _value :: !a
  , _startTimeUnixNano :: !(Maybe Timestamp)
  }
  deriving (Show)

value :: Lens' (WithContext a) a
value = lens get set
 where
  get WithContext{..} = _value
  set aXst st = aXst{_value = st}

maybeStartTimeUnixNano :: Lens' (WithContext a) (Maybe Timestamp)
maybeStartTimeUnixNano = lens get set
 where
  get WithContext{..} = _startTimeUnixNano
  set aXst st = aXst{_startTimeUnixNano = st}

startTimeUnixNano :: Lens' (WithContext a) Timestamp
startTimeUnixNano = lens get set
 where
  -- TODO: issue a warning when 0 is returned.
  get aXst = fromMaybe 0 (aXst ^. maybeStartTimeUnixNano)
  set aXst st = aXst & maybeStartTimeUnixNano .~ Just st

timeUnixNano :: Lens' (WithContext Event) Timestamp
timeUnixNano = lens get set
 where
  get evXst = evXst ^. value . EF.evTime + evXst ^. startTimeUnixNano
  set evXst t = evXst & value . EF.evTime .~ t - evXst ^. startTimeUnixNano

--------------------------------------------------------------------------------
-- DSL for writing messages

-- | Construct a message with a list of modifications applied.
messageWith :: (Message msg) => [msg -> msg] -> msg
messageWith = foldr ($) defMessage

-- | Construct a message with a list of modifications applied.
intDataPointWith ::
  (Integral i) =>
  -- | The event with context.
  WithContext Event ->
  -- | The integral data point.
  i ->
  -- | A list of attributes.
  [Maybe KeyValue] ->
  NumberDataPoint
intDataPointWith ev i attributes =
  messageWith $
    [ MF.asInt .~ fromIntegral i
    , MF.timeUnixNano .~ ev ^. timeUnixNano
    , MF.attributes .~ catMaybes attributes
    , maybe id (MF.startTimeUnixNano .~) (ev ^. maybeStartTimeUnixNano)
    ]

--------------------------------------------------------------------------------
-- DSL for writing attributes

infix 7 ~=

(~=) :: (ToAnyValue v) => Text -> v -> Maybe KeyValue
k ~= v = toAnyValue v <&> \v -> defMessage & C.key .~ k & C.value .~ v

class ToAnyValue v where
  toAnyValue :: v -> Maybe AnyValue

instance ToAnyValue Int64 where
  toAnyValue :: Int64 -> Maybe AnyValue
  toAnyValue v = Just (defMessage & C.intValue .~ v)

instance ToAnyValue Int where
  toAnyValue :: Int -> Maybe AnyValue
  toAnyValue v = toAnyValue (fromIntegral v :: Int64)

instance ToAnyValue Word8 where
  toAnyValue :: Word8 -> Maybe AnyValue
  toAnyValue v = toAnyValue (fromIntegral v :: Int64)

instance ToAnyValue Word32 where
  toAnyValue :: Word32 -> Maybe AnyValue
  toAnyValue v = toAnyValue (fromIntegral v :: Int64)

-- | __Warning__: This instance may overflow.
instance ToAnyValue Word64 where
  toAnyValue :: Word64 -> Maybe AnyValue
  toAnyValue v = toAnyValue (fromIntegral v :: Int64)

instance ToAnyValue String where
  toAnyValue :: String -> Maybe AnyValue
  toAnyValue = toAnyValue . T.pack

instance ToAnyValue Char where
  toAnyValue :: Char -> Maybe AnyValue
  toAnyValue = toAnyValue . T.singleton

instance ToAnyValue Text where
  toAnyValue :: Text -> Maybe AnyValue
  toAnyValue v = Just (defMessage & C.stringValue .~ v)

instance (ToAnyValue v) => ToAnyValue (Maybe v) where
  toAnyValue :: Maybe v -> Maybe AnyValue
  toAnyValue = (toAnyValue =<<)

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

options :: O.ParserInfo Options
options =
  O.info
    ( optionsParser
        O.<**> O.helperWith (O.long "help" <> O.help "Show this help text.")
        O.<**> O.simpleVersioner (showVersion EventlogLive.version)
    )
    O.idm

data Options = Options
  { batchInterval :: Int
  , maybeEventlogLogFile :: Maybe FilePath
  , maybeHeapProfBreakdown :: Maybe HeapProfBreakdown
  , eventlogSocket :: EventlogSocket
  , openTelemetryCollectorOptions :: OpenTelemetryCollectorOptions
  }

optionsParser :: O.Parser Options
optionsParser =
  Options
    <$> batchIntervalParser
    <*> O.optional eventlogLogFileParser
    <*> O.optional heapProfBreakdownParser
    <*> eventlogSocketParser
    <*> openTelemetryCollectorOptionsParser

--------------------------------------------------------------------------------
-- OpenTelemetry Collector Configuration

data OpenTelemetryCollectorOptions = OpenTelemetryCollectorOptions
  { openTelemetryCollectorServer :: G.Server
  }

openTelemetryCollectorOptionsParser :: O.Parser OpenTelemetryCollectorOptions
openTelemetryCollectorOptionsParser =
  O.parserOptionGroup "OpenTelemetry Collector Server Options" $
    OpenTelemetryCollectorOptions
      <$> otelcolServerParser

otelcolServerParser :: O.Parser G.Server
otelcolServerParser =
  makeServer
    <$> otelcolAddressParser
    <*> O.switch (O.long "otelcol-ssl" <> O.help "Use SSL.")
    <*> otelcolServerValidationParser
    <*> otelcolSslKeyLogParser
 where
  makeServer :: G.Address -> Bool -> G.ServerValidation -> G.SslKeyLog -> G.Server
  makeServer address ssl serverValidation sslKeyLog
    | ssl = G.ServerSecure serverValidation sslKeyLog address
    | otherwise = G.ServerInsecure address

otelcolAddressParser :: O.Parser G.Address
otelcolAddressParser =
  G.Address
    <$> ( O.strOption
            ( O.long "otelcol-host"
                <> O.metavar "HOST"
                <> O.help "Server hostname."
            )
        )
    <*> ( O.option
            O.auto
            ( O.long "otelcol-port"
                <> O.metavar "PORT"
                <> O.help "Server TCP port."
                <> O.value 4317
            )
        )
    <*> ( O.optional
            ( O.strOption
                ( O.long "otelcol-authority"
                    <> O.metavar "HOST"
                    <> O.help "Server authority."
                )
            )
        )

otelcolServerValidationParser :: O.Parser G.ServerValidation
otelcolServerValidationParser =
  asum
    [ G.ValidateServer <$> otelcolCertificateStoreSpecParser
    , pure G.NoServerValidation
    ]
 where
  otelcolCertificateStoreSpecParser :: O.Parser G.CertificateStoreSpec
  otelcolCertificateStoreSpecParser =
    makeCertificateStoreSpec
      <$> ( O.optional
              ( O.strOption
                  ( O.long "otelcol-certificate-store"
                      <> O.metavar "FILE"
                      <> O.help "Store for certificate validation."
                  )
              )
          )
   where
    makeCertificateStoreSpec :: Maybe FilePath -> G.CertificateStoreSpec
    makeCertificateStoreSpec = maybe G.certStoreFromSystem G.certStoreFromPath

otelcolSslKeyLogParser :: O.Parser G.SslKeyLog
otelcolSslKeyLogParser =
  asum
    [ G.SslKeyLogPath
        <$> O.strOption
          ( O.long "otelcol-ssl-key-log"
              <> O.metavar "FILE"
              <> O.help "Use file to log SSL keys."
          )
    , O.flag
        G.SslKeyLogNone
        G.SslKeyLogFromEnv
        ( O.long "otelcol-ssl-key-log-from-env"
            <> O.help "Use SSLKEYLOGFILE to log SSL keys."
        )
    ]
