{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : GHC.Eventlog.Live.Machine
Description : Machines for processing eventlog data.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Machine (
  -- * Heap events
  processHeapAllocatedData,
  processHeapSizeData,
  processBlocksSizeData,
  processHeapLiveData,
  MemReturnData (..),
  processMemReturnData,
  processHeapProfSampleData,

  -- * Heap profile breakdown
  heapProfBreakdownEitherReader,
  heapProfBreakdownShow,
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (isLeft)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable (..))
import Data.List qualified as L
import Data.Machine (Process, ProcessT, await, construct, repeatedly, yield)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import GHC.Eventlog.Live.Data.Attribute (Attr, (~=))
import GHC.Eventlog.Live.Data.Metric (Metric (..))
import GHC.Eventlog.Live.Internal.Logger (logWarning)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..), tryGetTimeUnixNano)
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.RTS.Events (Event (..), HeapProfBreakdown (..))
import GHC.RTS.Events qualified as E
import Numeric (showHex)
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.ParserCombinators.ReadP qualified as P
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Read.Lex (readHexP)

-------------------------------------------------------------------------------
-- Heap events
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- HeapAllocated

{- |
This machine processes `E.HeapAllocated` events into metrics.
-}
processHeapAllocatedData :: Process (WithStartTime Event) (Metric Word64)
processHeapAllocatedData =
  repeatedly $
    await >>= \case
      i
        | E.HeapAllocated{..} <- i.value.evSpec ->
            yield $
              metric i allocBytes $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- HeapSize

{- |
This machine processes `E.HeapSize` events into metrics.
-}
processHeapSizeData :: Process (WithStartTime Event) (Metric Word64)
processHeapSizeData = repeatedly go
 where
  go =
    await >>= \case
      i
        | E.HeapSize{..} <- i.value.evSpec -> do
            yield $
              metric i sizeBytes $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- BlocksSize

{- |
This machine processes `E.BlocksSize` events into metrics.
-}
processBlocksSizeData :: Process (WithStartTime Event) (Metric Word64)
processBlocksSizeData =
  repeatedly $
    await >>= \case
      i
        | E.BlocksSize{..} <- i.value.evSpec -> do
            yield $
              metric i blocksSize $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- HeapLive

{- |
This machine processes `E.HeapLive` events into metrics.
-}
processHeapLiveData :: Process (WithStartTime Event) (Metric Word64)
processHeapLiveData =
  repeatedly $
    await >>= \case
      i
        | E.HeapLive{..} <- i.value.evSpec -> do
            yield $
              metric i liveBytes $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- MemReturn

{- |
The type of data associated with a `E.MemReturn` event.
-}
data MemReturnData = MemReturnData
  { current :: !Word32
  -- ^ The number of megablocks currently allocated.
  , needed :: !Word32
  -- ^ The number of megablocks currently needed.
  , returned :: !Word32
  -- ^ The number of megablocks currently being returned to the OS.
  }

{- |
This machine processes `E.MemReturn` events into metrics.
-}
processMemReturnData :: Process (WithStartTime Event) (Metric MemReturnData)
processMemReturnData =
  repeatedly $
    await >>= \case
      i
        | E.MemReturn{..} <- i.value.evSpec -> do
            yield $
              metric i MemReturnData{..} $
                [ "evCap" ~= i.value.evCap
                , "heapCapset" ~= heapCapset
                ]
        | otherwise -> pure ()

-------------------------------------------------------------------------------
-- HeapProfSample

{- |
Internal helper.
The type of info table pointers.
-}
newtype InfoTablePtr = InfoTablePtr Word64
  deriving newtype (Eq, Hashable, Ord)

instance Show InfoTablePtr where
  showsPrec :: Int -> InfoTablePtr -> ShowS
  showsPrec _ (InfoTablePtr ptr) =
    showString "0x" . showHex ptr

instance Read InfoTablePtr where
  readsPrec :: Int -> ReadS InfoTablePtr
  readsPrec _ = readP_to_S (InfoTablePtr <$> (P.string "0x" *> readHexP))

{- |
Internal helper.
The type of an info table entry, as produced by the `E.InfoTableProv` event.
-}
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

{- |
Internal helper.
The type of the state kept by `processHeapProfSampleData`.
-}
data HeapProfSampleState = HeapProfSampleState
  { eitherShouldWarnOrHeapProfBreakdown :: Either Bool HeapProfBreakdown
  , infoTableMap :: HashMap InfoTablePtr InfoTable
  , heapProfSampleEraStack :: [Word64]
  }
  deriving (Show)

{- |
Internal helper.
Decides whether or not `processHeapProfSampleData` should track info tables.
We track info tables until (1) we learn that the RTS is not run with @-hi@,
or (2) we see the first heap profiling sample and don't yet know for sure
that the RTS is run with @-hi@.
-}
shouldTrackInfoTableMap :: Either Bool HeapProfBreakdown -> Bool
shouldTrackInfoTableMap (Left _shouldWarn) = True
shouldTrackInfoTableMap (Right HeapProfBreakdownInfoTable) = True
shouldTrackInfoTableMap _ = False

{- |
Internal helper.
Checks whether a `HeapProfBreakdown` is `HeapProfBreakdownInfoTable`.
This is needed because the ghc-events package does not define an `Eq`
instance for the `HeapProfBreakdown` type.
-}
isHeapProfBreakdownInfoTable :: HeapProfBreakdown -> Bool
isHeapProfBreakdownInfoTable HeapProfBreakdownInfoTable = True
isHeapProfBreakdownInfoTable _ = False

{- |
This machine processes `E.HeapProfSampleString` events into metrics.
Furthermore, it processes the `E.HeapProfBegin` and `E.ProgramArgs` events
to determine the heap profile breakdown, processes `E.InfoTableProv` events to
build an info table map, if necessary, and processes `E.HeapProfSampleBegin`
and `E.HeapProfSampleEnd` events to maintain an era stack.
-}
processHeapProfSampleData ::
  (MonadIO m) =>
  Verbosity ->
  Maybe HeapProfBreakdown ->
  ProcessT m (WithStartTime Event) (Metric Word64)
processHeapProfSampleData verbosityThreshold maybeHeapProfBreakdown =
  construct $
    go
      HeapProfSampleState
        { eitherShouldWarnOrHeapProfBreakdown = maybe (Left True) Right maybeHeapProfBreakdown
        , infoTableMap = mempty
        , heapProfSampleEraStack = mempty
        }
 where
  -- go :: HeapProfSampleState -> PlanT (Is (WithStartTime Event)) (Metric Word64) m Void
  go st@HeapProfSampleState{..} = do
    await >>= \i -> case i.value.evSpec of
      -- Announces the heap profile breakdown, amongst other things.
      -- This event is only emitted for code compiled with GHC >=9.14.
      E.HeapProfBegin{..}
        | isLeft eitherShouldWarnOrHeapProfBreakdown ->
            go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
      -- Announces the arguments with which the program was called.
      -- This *may* include RTS options, which can be used to determine the
      -- heap profile breakdown for code compiled with GHC <9.14.
      E.ProgramArgs{..}
        | isLeft eitherShouldWarnOrHeapProfBreakdown
        , Just heapProfBreakdown <- findHeapProfBreakdown args ->
            go st{eitherShouldWarnOrHeapProfBreakdown = Right heapProfBreakdown}
      -- Announces an info table entry.
      E.InfoTableProv{..}
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
            go st{infoTableMap = M.insert infoTablePtr infoTable infoTableMap}
      -- Announces the beginning of a heap profile sample.
      E.HeapProfSampleBegin{..} ->
        go st{heapProfSampleEraStack = heapProfSampleEra : heapProfSampleEraStack}
      -- Announces the end of a heap profile sample.
      E.HeapProfSampleEnd{..} ->
        case L.uncons heapProfSampleEraStack of
          Nothing -> do
            logWarning verbosityThreshold "processHeapProfSampleData" . T.pack $
              printf
                "Eventlog closed era %d, but there is no current era."
                heapProfSampleEra
            go st
          Just (currentEra, heapProfSampleEraStack') -> do
            unless (currentEra == heapProfSampleEra) $
              logWarning verbosityThreshold "processHeapProfSampleData" . T.pack $
                printf
                  "Eventlog closed era %d, but the current era is era %d."
                  heapProfSampleEra
                  currentEra
            go st{heapProfSampleEraStack = heapProfSampleEraStack'}
      -- Announces a heap profile sample.
      E.HeapProfSampleString{..}
        -- If there is no heap profile breakdown, issue a warning, then disable warnings.
        | Left True <- eitherShouldWarnOrHeapProfBreakdown -> do
            logWarning verbosityThreshold "processHeapProfSampleData" $
              "Cannot infer heap profile breakdown.\n\
              \         If your binary was compiled with a GHC version prior to 9.14,\n\
              \         you must also pass the heap profile type to this executable.\n\
              \         See: https://gitlab.haskell.org/ghc/ghc/-/commit/76d392a"
            go st{eitherShouldWarnOrHeapProfBreakdown = Left False, infoTableMap = mempty}
        -- If the heap profile breakdown is biographical, issue a warning, then disable warnings.
        | Right HeapProfBreakdownBiography <- eitherShouldWarnOrHeapProfBreakdown -> do
            logWarning verbosityThreshold "processHeapProfSampleData" . T.pack $
              printf
                "Unsupported heap profile breakdown %s"
                (heapProfBreakdownShow HeapProfBreakdownBiography)
            go st{eitherShouldWarnOrHeapProfBreakdown = Left False, infoTableMap = mempty}
        -- If there is a heap profile breakdown, handle it appropriately.
        | Right heapProfBreakdown <- eitherShouldWarnOrHeapProfBreakdown -> do
            -- If the heap profile breakdown is by info table, add the info table.
            let maybeInfoTable
                  | isHeapProfBreakdownInfoTable heapProfBreakdown = do
                      !infoTablePtr <- readMaybe (T.unpack heapProfLabel)
                      M.lookup infoTablePtr infoTableMap
                  | otherwise = Nothing
            yield $
              metric i heapProfResidency $
                [ "evCap" ~= i.value.evCap
                , "heapProfBreakdown" ~= heapProfBreakdownShow heapProfBreakdown
                , "heapProfId" ~= heapProfId
                , "heapProfLabel" ~= heapProfLabel
                , "heapProfSampleEra" ~= (fst <$> L.uncons heapProfSampleEraStack)
                , "infoTableName" ~= fmap (.infoTableName) maybeInfoTable
                , "infoTableClosureDesc" ~= fmap (.infoTableClosureDesc) maybeInfoTable
                , "infoTableTyDesc" ~= fmap (.infoTableTyDesc) maybeInfoTable
                , "infoTableLabel" ~= fmap (.infoTableLabel) maybeInfoTable
                , "infoTableModule" ~= fmap (.infoTableModule) maybeInfoTable
                , "infoTableSrcLoc" ~= fmap (.infoTableSrcLoc) maybeInfoTable
                ]
            go $ if isHeapProfBreakdownInfoTable heapProfBreakdown then st else st{infoTableMap = mempty}
      _otherwise -> go st

{- |
Parses the `HeapProfBreakdown` command-line arguments:

> heapProfBreakdownEitherReader "T" == Left HeapProfBreakdownClosureType
> heapProfBreakdownEitherReader "c" == Left HeapProfBreakdownCostCentre
> heapProfBreakdownEitherReader "m" == Left HeapProfBreakdownModule
> heapProfBreakdownEitherReader "d" == Left HeapProfBreakdownClosureDescr
> heapProfBreakdownEitherReader "y" == Left HeapProfBreakdownTypeDescr
> heapProfBreakdownEitherReader "e" == Left HeapProfBreakdownEra
> heapProfBreakdownEitherReader "r" == Left HeapProfBreakdownRetainer
> heapProfBreakdownEitherReader "b" == Left HeapProfBreakdownBiography
> heapProfBreakdownEitherReader "i" == Left HeapProfBreakdownInfoTable
-}
heapProfBreakdownEitherReader :: String -> Either String HeapProfBreakdown
heapProfBreakdownEitherReader =
  \case
    "T" -> Right HeapProfBreakdownClosureType
    "c" -> Right HeapProfBreakdownCostCentre
    "m" -> Right HeapProfBreakdownModule
    "d" -> Right HeapProfBreakdownClosureDescr
    "y" -> Right HeapProfBreakdownTypeDescr
    "e" -> Right HeapProfBreakdownEra
    "r" -> Right HeapProfBreakdownRetainer
    "b" -> Right HeapProfBreakdownBiography
    "i" -> Right HeapProfBreakdownInfoTable
    str -> Left $ "Unsupported heap profile breakdown -h" <> str

{- |
Shows a `HeapProfBreakdown` as its corresponding command-line flag:

> heapProfBreakdownShow HeapProfBreakdownClosureType == "-hT"
> heapProfBreakdownShow HeapProfBreakdownCostCentre == "-hc"
> heapProfBreakdownShow HeapProfBreakdownModule == "-hm"
> heapProfBreakdownShow HeapProfBreakdownClosureDescr == "-hd"
> heapProfBreakdownShow HeapProfBreakdownTypeDescr == "-hy"
> heapProfBreakdownShow HeapProfBreakdownEra == "-he"
> heapProfBreakdownShow HeapProfBreakdownRetainer == "-hr"
> heapProfBreakdownShow HeapProfBreakdownBiography == "-hb"
> heapProfBreakdownShow HeapProfBreakdownInfoTable == "-hi"
-}
heapProfBreakdownShow :: HeapProfBreakdown -> String
heapProfBreakdownShow =
  ("-h" <>) . \case
    HeapProfBreakdownClosureType -> "T"
    HeapProfBreakdownCostCentre -> "c"
    HeapProfBreakdownModule -> "m"
    HeapProfBreakdownClosureDescr -> "d"
    HeapProfBreakdownTypeDescr -> "y"
    HeapProfBreakdownEra -> "e"
    HeapProfBreakdownRetainer -> "r"
    HeapProfBreakdownBiography -> "b"
    HeapProfBreakdownInfoTable -> "i"

{- |
Internal helper.
Determine the `HeapProfBreakdown` from the list of program arguments.

__Warning__: This scan is not fully correct. It merely scans for the presence
of arguments that, as a whole, parse with `heapProfBreakdownEitherReader`.
It does not handle @-with-rtsopts@ and does not restrict its search to those
arguments between @+RTS@ and @-RTS@ tags.
-}
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

-------------------------------------------------------------------------------
-- Internal Helpers
-------------------------------------------------------------------------------

{- |
Internal helper. Construct a t`Metric` from an event with a start time
(t`WithStartTime` t`Event`), together with the measurement and any attributes.
This is a smart constructor that pulls the various timestamps out of the event.
-}
metric ::
  WithStartTime Event ->
  v ->
  [Attr] ->
  Metric v
metric i v attr =
  Metric
    { value = v
    , maybeTimeUnixNano = tryGetTimeUnixNano i
    , maybeStartTimeUnixNano = i.maybeStartTimeUnixNano
    , attr = attr
    }
