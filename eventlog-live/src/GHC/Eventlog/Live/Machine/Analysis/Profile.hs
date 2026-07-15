module GHC.Eventlog.Live.Machine.Analysis.Profile (
  ThreadId (..),
  CallStack (..),
  StackFrame (..),

  -- * Cost-centre profiling
  processProfSampleCostCentre,

  -- * @ghc-stack-profiler@ profiling
  processGhcStackProfilerData,
)
where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Machine (Is, PlanT, ProcessT, await, construct, repeatedly, yield)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import GHC.Eventlog.Live.Data.Capability (CapNo (..), fromCapabilityId)
import GHC.Eventlog.Live.Data.Thread (ThreadId (..))
import GHC.Eventlog.Live.Logger (Logger, writeException)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..))
import GHC.RTS.Events (Event (..))
import GHC.RTS.Events qualified as E
import GHC.Stack.Profiler.Core.Eventlog qualified as GSP
import GHC.Stack.Profiler.Core.SymbolTable qualified as GSP
import GHC.Stack.Profiler.Core.ThreadSample qualified as GSP
import IpeDB.Database (Table)
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre (CostCentre (..), CostCentreId (..))
import IpeDB.Types.InfoProv (InfoProv (..), InfoProvId (..))
import IpeDB.Types.SrcLoc (Range (..), SrcLoc (..))

data CallStack = CallStack
  { threadId :: !(Maybe ThreadId)
  , capNo :: !CapNo
  , stack :: [StackFrame]
  }
  deriving (Show, Eq)

data StackFrame
  = StackFrame'InfoProv !InfoProv
  | StackFrame'Message !Text !SrcLoc
  | StackFrame'CostCentre !CostCentre
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Processor for cost-centre stack samples
--------------------------------------------------------------------------------

{- |
This machine processes `E.UserBinaryMessage` events into metrics.
Furthermore, it processes the `E.InfoTableProv` events to
-}
processProfSampleCostCentre ::
  (MonadIO m) =>
  Logger m ->
  DB.Table CostCentreId CostCentre ->
  ProcessT m (WithStartTime Event) CallStack
processProfSampleCostCentre _logger costCentreTable =
  repeatedly $
    await >>= \i -> case i.value.evSpec of
      E.ProfSampleCostCentre{..} -> do
        let !costCentreIds = CostCentreId <$> V.convert profCcsStack
        !maybeCostCentres <- liftIO $ DB.lookups costCentreTable costCentreIds
        let !callStackMessage =
              CallStack
                { threadId = Nothing
                , capNo = CapNo profCap
                , stack = mapMaybe (fmap StackFrame'CostCentre) . V.toList $ maybeCostCentres
                }
        yield $ callStackMessage
      _otherwise -> pure ()

--------------------------------------------------------------------------------
-- Processor for `ghc-stack-profiler` call-stack samples
--------------------------------------------------------------------------------

data GspProcessorState = GspProcessorState
  { -- TODO: this should probably be a maybe?
    -- We could report when interleaved messages are present
    stackProfSampleChunk :: ![GSP.BinaryCallStackMessage]
  , stackProfSymbolTableReader :: !GSP.IntMapTable
  , maybeStackProfSampleData :: !(Maybe CallStack)
  }

{- |
This machine processes the `E.UserBinaryMessage` events produced by
@ghc-stack-profiler@ into `CallStack`.
-}
processGhcStackProfilerData ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  Table InfoProvId InfoProv ->
  ProcessT m (WithStartTime Event) CallStack
processGhcStackProfilerData logger infoProvTable =
  construct $
    go
      GspProcessorState
        { stackProfSampleChunk = mempty
        , stackProfSymbolTableReader = GSP.emptyIntMapTable
        , maybeStackProfSampleData = Nothing
        }
 where
  go :: GspProcessorState -> PlanT (Is (WithStartTime Event)) CallStack m ()
  go st = do
    await >>= \i -> case i.value.evSpec of
      E.UserBinaryMessage{payload} ->
        case GSP.deserializeEventlogMessage $ LBS.fromStrict payload of
          Left _err ->
            go st
          Right evMsg -> case evMsg of
            GSP.CallStackFinal msg -> do
              (callStackMessage, st', callStackDecodeErrors) <-
                liftIO $
                  hydrateGspBinaryCallStackMessage infoProvTable st msg
              for_ callStackDecodeErrors (lift . writeException logger)
              yield callStackMessage
              go st'
            GSP.CallStackChunk msg ->
              go st{stackProfSampleChunk = msg : st.stackProfSampleChunk}
            GSP.StringDef msg ->
              go st{stackProfSymbolTableReader = GSP.insertTextMessage msg st.stackProfSymbolTableReader}
            GSP.SourceLocationDef msg -> do
              let old = st.stackProfSymbolTableReader
              let errOrnew = GSP.insertSourceLocationMessage msg old
              new <- either (\err -> lift $ writeException logger err >> pure old) pure errOrnew
              go st{stackProfSymbolTableReader = new}
      _otherwise -> go st

hydrateGspBinaryCallStackMessage ::
  Table InfoProvId InfoProv ->
  GspProcessorState ->
  GSP.BinaryCallStackMessage ->
  IO (CallStack, GspProcessorState, [GSP.BinaryCallStackDecodeError])
hydrateGspBinaryCallStackMessage infoProvTable spst msg = do
  let !chunks = spst.stackProfSampleChunk

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
  let !orderedChunks = NonEmpty.reverse $ msg :| chunks
  let !fullBinaryCallStackMessage = GSP.catCallStackMessage orderedChunks
  let !(callStackMessage, callStackDecodeErrors) =
        GSP.hydrateEventlogCallStackMessage
          (GSP.mkIntMapSymbolTableReader spst.stackProfSymbolTableReader)
          fullBinaryCallStackMessage
  let !callStack = GSP.callStack callStackMessage

  -- Extract the IPE IDs and look all of them up in a single database query,
  -- relying on the fact that IPT.lookups preserves the order.
  let !infoProvPtrs = V.fromList . flip mapMaybe callStack $ \case
        GSP.IpeId iid -> Just $! InfoProvId (GSP.getIpeId iid)
        _otherwise -> Nothing
  !maybeInfoProvs <- V.toList <$> DB.lookups infoProvTable infoProvPtrs

  -- Merge the maybeInfoProvs results into the StackFrame.
  -- TODO: There's probably a less explicit and more optimiseable way to do this.
  let toStackFrame :: [Maybe InfoProv] -> [GSP.StackItem] -> [Maybe StackFrame]
      toStackFrame infoProvAcc [] =
        assert (null infoProvAcc) []
      toStackFrame [] (GSP.IpeId _iid : stackItems) = do
        Nothing : toStackFrame [] stackItems
      toStackFrame (Nothing : infoProvAcc) (GSP.IpeId _iid : stackItems) =
        Nothing : toStackFrame infoProvAcc stackItems
      toStackFrame (Just infoProv : infoProvAcc) (GSP.IpeId _iid : stackItems) =
        -- Resolve an IPE ID against the top result in the InfoProv stack.
        let !stackItemData = StackFrame'InfoProv infoProv
         in Just stackItemData : toStackFrame infoProvAcc stackItems
      toStackFrame infoProvAcc (GSP.UserAnnotation userMessage maybeSourceLocation : stackItems) =
        -- Repackage a user annotation.
        let !stackItemData = StackFrame'Message (Text.pack userMessage) (toSrcLoc maybeSourceLocation)
         in Just stackItemData : toStackFrame infoProvAcc stackItems

  let !callStackData =
        CallStack
          { threadId = Just $ ThreadId $ GSP.callThreadId callStackMessage
          , capNo = fromCapabilityId (GSP.callCapabilityId callStackMessage)
          , stack = catMaybes (toStackFrame maybeInfoProvs callStack)
          }
  pure (callStackData, spst{stackProfSampleChunk = []}, callStackDecodeErrors)

{- |
Internal helper.

Convert a `GSP.SourceLocation` to a `SrcLoc`.
-}
toSrcLoc :: Maybe GSP.SourceLocation -> SrcLoc
toSrcLoc = \case
  Nothing ->
    UnhelpfulSrcLoc
  Just GSP.MkSourceLocation{fileName, column, line} ->
    SrcLoc (Text.unpack fileName) (Just $! Range'Point column line)
