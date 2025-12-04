module GHC.Eventlog.Live.Machine.Analysis.Profile (
  StackProfSampleData (..),
  InfoTableIndex (..),
  CallStackData (..),
  StackItemData (..),
  processStackProfSampleData,
  stackProfSamples,
)
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict qualified as M
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Machine (ProcessT, await, construct, yield)
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Eventlog.Index qualified as IpeDb
import GHC.Eventlog.Live.Data.Attribute qualified as Attrs
import GHC.Eventlog.Live.Data.Metric
import GHC.Eventlog.Live.Machine.Analysis.Heap (InfoTable (..), InfoTablePtr (..), metric)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.Eventlog.Live.Verbosity (Verbosity)
import GHC.Generics (Generic)
import GHC.RTS.Events (Event (..))
import GHC.RTS.Events qualified as E
import GHC.Stack.Profiler.Core.Eventlog as SPCE
import GHC.Stack.Profiler.Core.SymbolTable qualified as SPCT
import GHC.Stack.Profiler.Core.ThreadSample as SPCT

data StackProfSampleState = StackProfSampleState
  { infoTableMap :: !(HashMap InfoTablePtr InfoTable)
  , -- TODO: this should probably be a maybe?
    -- We could report when interleaved messages are present
    stackProfSampleChunk :: ![BinaryCallStackMessage]
  , stackProfSymbolTableReader :: !SPCT.IntMapTable
  , infoTableIndex :: Maybe InfoTableIndex
  }
  deriving (Generic)

newtype StackProfSampleData = StackProfSampleData
  { stackProfSample :: Metric CallStackData
  }
  deriving (Show, Generic)

data CallStackData = CallStackData
  { threadId :: !Word64
  , capabilityId :: !CapabilityId
  , stack :: [StackItemData]
  }
  deriving (Show, Generic)

data StackItemData
  = IpeData !InfoTable
  | UserMessageData !Text
  | SourceLocationData !SourceLocation
  deriving (Show, Generic)

shouldTrackInfoTableMap :: Maybe InfoTableIndex -> Bool
shouldTrackInfoTableMap Nothing = True
shouldTrackInfoTableMap (Just _index) = False

newtype InfoTableIndex = InfoTableIndex
  { db :: IpeDb.InfoProvDb
  }
  deriving (Generic)

{- |
This machine processes `E.UserBinaryMessage` events into metrics.
Furthermore, it processes the `E.InfoTableProv` events to
-}
processStackProfSampleData ::
  (MonadIO m) =>
  Verbosity ->
  Maybe InfoTableIndex ->
  ProcessT m (WithStartTime Event) StackProfSampleData
processStackProfSampleData _verbosityThreshold index =
  construct $
    go
      StackProfSampleState
        { infoTableMap = mempty
        , stackProfSampleChunk = mempty
        , stackProfSymbolTableReader = SPCT.emptyIntMapTable
        , infoTableIndex = index
        }
 where
  go st = do
    await >>= \i -> case i.value.evSpec of
      -- Announces an info table entry.
      E.InfoTableProv{..}
        | shouldTrackInfoTableMap st.infoTableIndex -> do
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
        case deserializeEventlogMessage $ LBS.fromStrict payload of
          Left _err ->
            go st
          Right evMsg -> case evMsg of
            CallStackFinal msg -> do
              (callStackMessage, st1) <-
                liftIO $ hydrateBinaryEventlog st msg
              let
                stackProfSample =
                  metric i callStackMessage Attrs.empty

              yield $ StackProfSampleData stackProfSample
              go st1
            CallStackChunk msg ->
              go st{stackProfSampleChunk = msg : st.stackProfSampleChunk}
            StringDef msg ->
              go st{stackProfSymbolTableReader = SPCT.insertTextMessage msg st.stackProfSymbolTableReader}
            SourceLocationDef msg ->
              go st{stackProfSymbolTableReader = SPCT.insertSourceLocationMessage msg st.stackProfSymbolTableReader}
      _otherwise -> go st

hydrateBinaryEventlog :: StackProfSampleState -> BinaryCallStackMessage -> IO (CallStackData, StackProfSampleState)
hydrateBinaryEventlog spst msg = do
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
      fullBinaryCallStackMessage = catCallStackMessage orderedChunks
      callStackMessage =
        hydrateEventlogCallStackMessage
          (SPCT.mkIntMapSymbolTableReader spst.stackProfSymbolTableReader)
          fullBinaryCallStackMessage

  let lookupInfoTable = getLookupInfoTableFunc spst
  stack <- traverse (toStackItemData lookupInfoTable) (callStack callStackMessage)

  let
    callStackData =
      CallStackData
        { threadId = callThreadId callStackMessage
        , capabilityId = callCapabilityId callStackMessage
        , stack =
            -- TODO: log if we are encountering unknown ipe ids
            Maybe.catMaybes stack
        }

  pure
    ( callStackData
    , spst{stackProfSampleChunk = []}
    )

toStackItemData :: (IpeId -> IO (Maybe InfoTable)) -> StackItem -> IO (Maybe StackItemData)
toStackItemData lookupInfoTable = \case
  IpeId iid -> fmap IpeData <$> lookupInfoTable iid
  UserMessage msg -> pure $ Just $ UserMessageData $ Text.pack msg
  SourceLocation srcLoc -> pure $ Just $ SourceLocationData srcLoc

getLookupInfoTableFunc :: StackProfSampleState -> (IpeId -> IO (Maybe InfoTable))
getLookupInfoTableFunc spst = case spst.infoTableIndex of
  Nothing -> \iid -> pure (HashMap.lookup (InfoTablePtr $ getIpeId iid) spst.infoTableMap)
  Just ipeIndex -> \iid -> do
    mInfoProv <- IpeDb.lookupInfoProv ipeIndex.db (IpeDb.IpeId $ getIpeId iid)
    pure $ fmap fromInfoProv mInfoProv
 where
  fromInfoProv :: IpeDb.InfoProv -> InfoTable
  fromInfoProv prov =
    InfoTable
      { infoTablePtr = InfoTablePtr prov.infoId.id
      , infoTableName = prov.tableName
      , infoTableClosureDesc = fromIntegral prov.closureDesc
      , infoTableTyDesc = prov.typeDesc
      , infoTableLabel = prov.label
      , infoTableModule = prov.moduleName
      , infoTableSrcLoc = prov.srcLoc
      }

{- |
Get the elements of a heap profile sample collection.
-}
stackProfSamples :: StackProfSampleData -> [Metric CallStackData]
stackProfSamples = List.singleton . (.stackProfSample)
