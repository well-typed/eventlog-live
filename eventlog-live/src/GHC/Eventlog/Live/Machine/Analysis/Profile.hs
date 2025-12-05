module GHC.Eventlog.Live.Machine.Analysis.Profile (
  StackProfSampleData (..),
  ThreadId (..),
  CapabilityId (..),
  CallStackData (..),
  StackItemData (..),
  CostCentreId (..),
  CostCentre (..),
  processStackProfSampleData,
  processCosterCentreProfSampleData,
  stackProfSamples,
)
where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict qualified as M
import Data.Hashable qualified as Hashable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Machine (ProcessT, await, construct, yield)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector.Unboxed qualified as UVector
import Data.Word
import GHC.Eventlog.Live.Data.Attribute qualified as Attrs
import GHC.Eventlog.Live.Data.Metric
import GHC.Eventlog.Live.Machine.Analysis.Heap (InfoTable (..), InfoTablePtr (..), metric)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.Generics (Generic)
import GHC.RTS.Events (Event (..))
import GHC.RTS.Events qualified as E
import GHC.Stack.Profiler.Core.Eventlog qualified as SPCE
import GHC.Stack.Profiler.Core.SymbolTable qualified as SPCS
import GHC.Stack.Profiler.Core.ThreadSample qualified as SPCT

data StackProfSampleState = StackProfSampleState
  { infoTableMap :: !(HashMap InfoTablePtr InfoTable)
  , -- TODO: this should probably be a maybe?
    -- We could report when interleaved messages are present
    stackProfSampleChunk :: ![SPCE.BinaryCallStackMessage]
  , stackProfSymbolTableReader :: !SPCS.IntMapTable
  , maybeStackProfSampleData :: !(Maybe StackProfSampleData)
  }
  deriving (Generic)

newtype CostCentreProfSampleState = CostCentreProfSampleState
  { costCentreMap :: HashMap CostCentreId CostCentre
  }
  deriving (Generic)

newtype StackProfSampleData = StackProfSampleData
  { stackProfSample :: Metric CallStackData
  }
  deriving (Show, Generic)

newtype ThreadId = ThreadId {id :: Word64} deriving (Show, Eq, Ord, Generic)

newtype CapabilityId = CapabilityId {id :: Word64} deriving (Show, Eq, Ord, Generic)

data CallStackData = CallStackData
  { threadId :: !(Maybe ThreadId)
  , capabilityId :: !CapabilityId
  , stack :: [StackItemData]
  }
  deriving (Show, Eq, Ord, Generic)

newtype CostCentreId = CostCentreId
  { id :: Word64
  }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable.Hashable)

data CostCentre = CostCentre
  { costCentreId :: !CostCentreId
  , costCentreLabel :: !Text
  , costCentreModule :: !Text
  , costCentreSrcLoc :: !Text
  -- , heapProfFlags :: !HeapProfFlags
  }
  deriving (Show, Eq, Ord, Generic)

data StackItemData
  = IpeData !InfoTable
  | UserMessageData !Text
  | SourceLocationData !SPCT.SourceLocation
  | CostCentreData !CostCentre
  deriving (Show, Eq, Ord, Generic)

shouldTrackInfoTableMap :: Bool
shouldTrackInfoTableMap = True

shouldTrackCostCentreMap :: Bool
shouldTrackCostCentreMap = True

-- ----------------------------------------------------------------------------
-- `cost centre stack` processor
-- ----------------------------------------------------------------------------

{- |
This machine processes `E.UserBinaryMessage` events into metrics.
Furthermore, it processes the `E.InfoTableProv` events to
-}
processCosterCentreProfSampleData ::
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (WithStartTime Event) StackProfSampleData
processCosterCentreProfSampleData _verbosityThreshold =
  construct $
    go
      CostCentreProfSampleState
        { costCentreMap = mempty
        }
 where
  go st = do
    await >>= \i -> case i.value.evSpec of
      -- Announces an info table entry.
      E.HeapProfCostCentre{..}
        | shouldTrackCostCentreMap -> do
            let costCentreId = CostCentreId $ fromIntegral heapProfCostCentreId
                costCentre =
                  CostCentre
                    { costCentreId = costCentreId
                    , costCentreLabel = heapProfLabel
                    , costCentreModule = heapProfModule
                    , costCentreSrcLoc = heapProfSrcLoc
                    }
            go st{costCentreMap = M.insert costCentreId costCentre st.costCentreMap}
      E.ProfSampleCostCentre{..} -> do
        let lookupCostCentreStackById :: Word32 -> Maybe StackItemData
            lookupCostCentreStackById costCentreId32 =
              CostCentreData <$> HashMap.lookup (CostCentreId $ fromIntegral costCentreId32) st.costCentreMap

            callStackMessage =
              CallStackData
                { threadId = Nothing
                , capabilityId = CapabilityId $ fromIntegral profCap
                , stack =
                    -- TODO: log if we are encountering unknown cost centre ids
                    mapMaybe lookupCostCentreStackById (UVector.toList profCcsStack)
                }

            stackProfSample =
              metric i callStackMessage Attrs.empty

        yield $ StackProfSampleData stackProfSample
        go st
      _otherwise -> go st

-- ----------------------------------------------------------------------------
-- `ghc-stack-profiler` processor
-- ----------------------------------------------------------------------------

{- |
This machine processes `E.UserBinaryMessage` events into metrics.
Furthermore, it processes the `E.InfoTableProv` events to
-}
processStackProfSampleData ::
  (MonadIO m) =>
  Verbosity ->
  ProcessT m (WithStartTime Event) StackProfSampleData
processStackProfSampleData _verbosityThreshold =
  construct $
    go
      StackProfSampleState
        { infoTableMap = mempty
        , stackProfSampleChunk = mempty
        , stackProfSymbolTableReader = SPCS.emptyIntMapTable
        , maybeStackProfSampleData = Nothing
        }
 where
  go st = do
    await >>= \i -> case i.value.evSpec of
      -- Announces an info table entry.
      E.InfoTableProv{..}
        | shouldTrackInfoTableMap -> do
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
            go st{infoTableMap = M.insert infoTablePtr infoTable st.infoTableMap}
      E.UserBinaryMessage{payload} ->
        case SPCT.deserializeEventlogMessage $ LBS.fromStrict payload of
          Left _err ->
            go st
          Right evMsg -> case evMsg of
            SPCE.CallStackFinal msg -> do
              let (callStackMessage, st1) =
                    hydrateBinaryEventlog st msg

                  stackProfSample =
                    metric i callStackMessage Attrs.empty

              yield $ StackProfSampleData stackProfSample
              go st1
            SPCE.CallStackChunk msg ->
              go st{stackProfSampleChunk = msg : st.stackProfSampleChunk}
            SPCE.StringDef msg ->
              go st{stackProfSymbolTableReader = SPCS.insertTextMessage msg st.stackProfSymbolTableReader}
            SPCE.SourceLocationDef msg ->
              go st{stackProfSymbolTableReader = SPCS.insertSourceLocationMessage msg st.stackProfSymbolTableReader}
      _otherwise -> go st

hydrateBinaryEventlog :: StackProfSampleState -> SPCE.BinaryCallStackMessage -> (CallStackData, StackProfSampleState)
hydrateBinaryEventlog spst msg =
  let chunks = spst.stackProfSampleChunk
      -- Why reverse?
      -- When decoding the stack, we walk the stack from the top down.
      -- Afterwards, the stack is chunked to fit into a single eventlog line,
      -- and the chunks are written in ascending order to the eventlog.
      -- When we pick up these messages one after another, they are prepended to
      -- 'stackProfSampleChunk', thus we are essentially storing the chunks in reverse
      -- order, as the first chunk we encounter is the top of the stack, etc...
      --
      -- Concrete example, assuming a stack @[1,2,3,4,5,6]@ and chunk size of 2:
      --
      -- 1. Chunk it: @[1,2] [3,4] [5,6]@
      -- 2. Write it to the eventlog in this order, so the messages are:
      --    [1,2]
      --    [3,4]
      --    [5,6]
      -- 3. When reading the eventlog, we store prepend later messages, resulting in:
      --    [5,6] [3,4] [1,2]
      -- 4. One reverse later: @[1,2] [3,4] [5,6]@
      -- 5. Now we can finally concat the stack frame chunks.
      orderedChunks = NonEmpty.reverse $ msg :| chunks
      fullBinaryCallStackMessage = SPCT.catCallStackMessage orderedChunks
      callStackMessage =
        SPCT.hydrateEventlogCallStackMessage
          (SPCS.mkIntMapSymbolTableReader spst.stackProfSymbolTableReader)
          fullBinaryCallStackMessage

      callStackData =
        CallStackData
          { threadId = Just $ ThreadId $ SPCT.callThreadId callStackMessage
          , capabilityId = CapabilityId $ SPCE.getCapabilityId $ SPCT.callCapabilityId callStackMessage
          , stack =
              -- TODO: log if we are encountering unknown ipe ids
              mapMaybe (toStackItemData spst.infoTableMap) $ SPCT.callStack callStackMessage
          }
   in ( callStackData
      , spst{stackProfSampleChunk = []}
      )

toStackItemData :: HashMap InfoTablePtr InfoTable -> SPCT.StackItem -> Maybe StackItemData
toStackItemData tbl = \case
  SPCT.IpeId iid -> IpeData <$> HashMap.lookup (InfoTablePtr $ SPCE.getIpeId iid) tbl
  SPCT.UserMessage msg -> Just $ UserMessageData $ Text.pack msg
  SPCT.SourceLocation srcLoc -> Just $ SourceLocationData srcLoc

{- |
Get the elements of a heap profile sample collection.
-}
stackProfSamples :: StackProfSampleData -> [Metric CallStackData]
stackProfSamples = List.singleton . (.stackProfSample)
