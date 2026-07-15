{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.Machine.Analysis.Profile (
  -- * Call-stack profiling
  CallStack (..),
  CallStackFrame (..),
  processGhcStackProfilerData,

  -- * Cost-centre profiling
  CostCentreStack (..),
  processProfSampleCostCentreData,
)
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception (..))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString.Lazy qualified as BSL
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Machine (Is, PlanT, ProcessT, await, construct, repeatedly, yield)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Traversable (mapAccumM)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Eventlog.Live.Data.Capability (CapNo (..), fromCapabilityId)
import GHC.Eventlog.Live.Data.Severity (Severity (..))
import GHC.Eventlog.Live.Data.Thread (ThreadId (..))
import GHC.Eventlog.Live.Logger (Logger, writeLog)
import GHC.Eventlog.Live.Machine.WithStartTime (WithStartTime (..), tryGetTimeUnixNano)
import GHC.RTS.Events (Event (..), Timestamp)
import GHC.RTS.Events qualified as E
import GHC.Stack.Profiler.Core.Eventlog qualified as GSP
import GHC.Stack.Profiler.Core.SymbolTable qualified as GSP
import GHC.Stack.Profiler.Core.ThreadSample qualified as GSP
import IpeDB.Database qualified as DB
import IpeDB.Types.CostCentre (CostCentre (..), CostCentreId (..))
import IpeDB.Types.InfoProv (InfoProv (..), InfoProvId (..))
import IpeDB.Types.SrcLoc (Range (..), SrcLoc (..))

--------------------------------------------------------------------------------
-- Processor for `ghc-stack-profiler` call-stack samples
--------------------------------------------------------------------------------

{- |
A GHC call-stack as produced by @ghc-stack-profiler@.
-}
data CallStack = CallStack
  { capNo :: !CapNo
  , threadId :: !ThreadId
  , callStack :: !(Vector CallStackFrame)
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  }

{- |
A GHC call-stack frame as produced by @ghc-stack-profiler@.
-}
data CallStackFrame
  = CallStackFrame !InfoProv
  | CallStackMessage !Text !SrcLoc

{- |
Internal helper.

The internal state for `processGhcStackProfilerData`.
-}
data GhcStackProfilerState = GhcStackProfilerState
  { warnOnDeserializeError :: !Bool
  , callStackChunksRev :: ![GSP.BinaryCallStackMessage]
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  , symbolTable :: !GSP.IntMapTable
  }

{- |
Internal helper.

The initial state for `processGhcStackProfilerData`.
-}
emptyGhcStackProfilerState :: GhcStackProfilerState
emptyGhcStackProfilerState =
  GhcStackProfilerState
    { warnOnDeserializeError = True
    , callStackChunksRev = []
    , maybeTimeUnixNano = Nothing
    , symbolTable = GSP.emptyIntMapTable
    }

{- |
This machine processes the `E.UserBinaryMessage` events produced by
@ghc-stack-profiler@ into `CallStack` samples.
-}
processGhcStackProfilerData ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  DB.Table InfoProvId InfoProv ->
  ProcessT m (WithStartTime Event) CallStack
processGhcStackProfilerData logger infoProvTable =
  construct $ go emptyGhcStackProfilerState
 where
  go :: GhcStackProfilerState -> PlanT (Is (WithStartTime Event)) CallStack m ()
  go st =
    await >>= \i ->
      case i.value.evSpec of
        E.UserBinaryMessage{..} ->
          case GSP.deserializeEventlogMessage (BSL.fromStrict payload) of
            Left errMsg
              | st.warnOnDeserializeError -> do
                  lift . writeLog logger WARN . T.unlines $
                    [ "Could not parse UserBinaryMessage as ghc-stack-profiler message:"
                    , T.pack errMsg
                    , "If other plugins are communicating via binary eventlog messages, this is expected."
                    ]
                  go st{warnOnDeserializeError = False}
              | otherwise -> go st
            -- If we receive the final call-stack chunk, decode and yield the call-stack, the restart...
            Right (GSP.CallStackFinal callStackChunk) -> do
              let symbolTableReader = GSP.mkIntMapSymbolTableReader st.symbolTable
              let callStackChunks = NE.reverse (callStackChunk :| st.callStackChunksRev)
              let !maybeTimeUnixNano = st.maybeTimeUnixNano <|> tryGetTimeUnixNano i
              callStack <- lift $ decodeCallStack maybeTimeUnixNano symbolTableReader callStackChunks
              unless (V.null callStack.callStack) $ yield callStack
              go
                st
                  { callStackChunksRev = []
                  , maybeTimeUnixNano = Nothing
                  }
            -- If we receive a call-stack chunk, add it to the list of chunks and continue...
            Right (GSP.CallStackChunk callStackChunk) ->
              go
                st
                  { callStackChunksRev = callStackChunk : st.callStackChunksRev
                  , maybeTimeUnixNano = st.maybeTimeUnixNano <|> tryGetTimeUnixNano i
                  }
            -- If we receive a string definition, update the symbol table and continue...
            Right (GSP.StringDef string) -> do
              let !symbolTable' = GSP.insertTextMessage string st.symbolTable
              go st{symbolTable = symbolTable'}
            -- If we receive a source location definition, update the symbol table and continue...
            Right (GSP.SourceLocationDef sourceLocation) ->
              case GSP.insertSourceLocationMessage sourceLocation st.symbolTable of
                Left errMsg -> do
                  lift . writeLog logger WARN . T.unlines $
                    [ "Could not decode source location from ghc-stack-profiler message:"
                    , T.pack (displayException errMsg)
                    ]
                  go st
                Right symbolTable' ->
                  go st{symbolTable = symbolTable'}
        _otherwise -> pure ()

  decodeCallStack ::
    Maybe Timestamp ->
    GSP.SymbolTableReader ->
    NonEmpty GSP.BinaryCallStackMessage ->
    m CallStack
  decodeCallStack maybeTimeUnixNano symbolTableReader callStackChunks = do
    -- Concatenate the chunks into a full binary call-stack message.
    let !gspBinaryCallStack = GSP.catCallStackMessage callStackChunks

    -- Decode the binary call-stack and log any decoding errors.
    let !(gspCallStackMessage, decodeErrors) =
          GSP.hydrateEventlogCallStackMessage symbolTableReader gspBinaryCallStack
    let !gspCallStack = GSP.callStack gspCallStackMessage
    unless (null decodeErrors) $
      writeLog logger WARN . T.unlines $
        ["Encountered errors while decoding binary call-stack from ghc-stack-profiler message:"]
          <> [T.pack (displayException decodeError) | decodeError <- decodeErrors]

    -- Extract the IPE IDs and look them up in a single batched database query.
    let getMaybeInfoProvId :: GSP.StackItem -> Maybe InfoProvId
        getMaybeInfoProvId = \case GSP.IpeId iid -> Just (toInfoProvId iid); _otherwise -> Nothing
    let infoProvIds = V.fromList . mapMaybe getMaybeInfoProvId $ gspCallStack
    maybeInfoProvs <- liftIO $ lookups infoProvTable infoProvIds

    -- Convert each `GSP.StackItem` to a `CallStackFrame`.
    let toCallStackFrame :: [Maybe InfoProv] -> GSP.StackItem -> m ([Maybe InfoProv], Maybe CallStackFrame)
        toCallStackFrame (Just infoProv : acc) (GSP.IpeId _iid) =
          pure (acc, Just $! CallStackFrame infoProv)
        toCallStackFrame acc (GSP.UserAnnotation msg maybeSourceLocation) =
          pure (acc, Just $! CallStackMessage (T.pack msg) (toSrcLoc maybeSourceLocation))
        toCallStackFrame (Nothing : acc) (GSP.IpeId iid) = do
          writeLog logger WARN $
            "Could not resolve IPE ID " <> T.pack (show (toInfoProvId iid))
          pure (acc, Nothing)
        toCallStackFrame [] (GSP.IpeId _iid) = do
          writeLog logger ERROR $
            "Did not lookup all IPE IDs. Please report this as a bug."
          pure ([], Nothing)
    callStack <-
      V.fromList . catMaybes . snd
        <$> mapAccumM toCallStackFrame (V.toList maybeInfoProvs) gspCallStack

    let !capNo = fromCapabilityId . GSP.callCapabilityId $ gspCallStackMessage
    let !threadId = ThreadId . GSP.callThreadId $ gspCallStackMessage
    pure CallStack{..}

{- |
Internal helper.

Convert a @ghc-stack-profiler@ `GSP.IpeID` to an `InfoProvId`.
-}
toInfoProvId :: GSP.IpeId -> InfoProvId
toInfoProvId (GSP.MkIpeId x) = InfoProvId x

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

--------------------------------------------------------------------------------
-- Processor for cost-centre stack samples
--------------------------------------------------------------------------------

{- |
A GHC cost-centre stack.
-}
data CostCentreStack = CostCentreStack
  { capNo :: !CapNo
  , costCentreStack :: !(Vector CostCentre)
  , maybeTimeUnixNano :: !(Maybe Timestamp)
  }

{- |
This machine processes `E.ProfSampleCostCentre` events into `CostCentreStack` samples.
-}
processProfSampleCostCentreData ::
  forall m.
  (MonadIO m) =>
  Logger m ->
  DB.Table CostCentreId CostCentre ->
  ProcessT m (WithStartTime Event) CostCentreStack
processProfSampleCostCentreData logger costCentreTable =
  repeatedly $
    await >>= \i ->
      case i.value.evSpec of
        E.ProfSampleCostCentre{..} -> do
          -- Look up all cost centre IDs in the cost centre stack.
          let !costCentreIds = CostCentreId <$> V.convert profCcsStack
          -- TODO: This does not deduplicate the entries in costCentreIds.
          !maybeCostCentresAssocs <- liftIO $ lookups costCentreTable costCentreIds

          -- NOTE: The following code is equivalent to `V.catMaybes`, except
          --       that a warning a logged for each unresolved cost centre ID.
          let warnIfNotFound ix maybeCostCentre
                | Nothing <- maybeCostCentre
                , Just costCentreId <- costCentreIds V.!? ix = do
                    writeLog logger WARN . T.pack $
                      "Could not resolve cost centre ID " <> show costCentreId
                    pure Nothing
                | otherwise = pure maybeCostCentre
          !costCentreStack <- lift $ V.imapMaybeM warnIfNotFound maybeCostCentresAssocs

          -- Yield the cost centre stack.
          let !capNo = CapNo profCap
          let !maybeTimeUnixNano = tryGetTimeUnixNano i
          yield CostCentreStack{..}
        _otherwise -> pure ()

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

{- |
Variant of `DB.lookups` that deduplicates keys before performing the database lookup.
-}
lookups :: (Ord k, DB.Key k, DB.Value v) => DB.Table k v -> Vector k -> IO (Vector (Maybe v))
lookups table keys = do
  -- Build a set of keys:
  let !keysSet = S.fromList (V.toList keys)
  if S.size keysSet == V.length keys
    then do
      -- If all keys are unique, we can just perform the database lookup...
      DB.lookups table keys
    else do
      -- Otherwise, we perform the database lookup with the unique keys and resolve them via a map...
      let !keysUniq = V.fromList (S.toAscList keysSet)
      maybeValues <- DB.lookups table keysUniq
      let !keyValueMap = M.fromAscList . V.toList . V.mapMaybe sequence $ V.zip keysUniq maybeValues
      pure $ V.map (`M.lookup` keyValueMap) keys
