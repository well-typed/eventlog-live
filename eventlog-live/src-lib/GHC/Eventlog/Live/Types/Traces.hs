{- |
Module      : GHC.Eventlog.Live.Span
Description : Representation for OTLP spans.
Stability   : experimental
Portability : portable
-}
module GHC.Eventlog.Live.Types.Traces (
  -- * Known Spans
  SomeSpans (..),
  KnownTrace (..),
  traceConfig,

  -- * Span superclass
  IsSpan,
  duration,

  -- * Generic span type
  Span (..),
  asSpan,
  toSpan,
  ToSpanState,
  initToSpanState,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.ByteString (ByteString)
import Data.Default (Default)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Kind (Constraint, Type)
import Data.Machine (ProcessT, await, construct, yield)
import Data.Proxy (Proxy)
import GHC.Eventlog.Live.Config (CapabilityUsageSpan (..), IsTraceProcessorConfig, ThreadStateSpan (..), Traces (..))
import GHC.Eventlog.Live.Types.Attribute (Attrs)
import GHC.RTS.Events (Timestamp)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol)
import System.Random (StdGen, initStdGen)
import System.Random.Compat (uniformByteString)

--------------------------------------------------------------------------------
-- Known Spans
--------------------------------------------------------------------------------

type SomeSpans :: Type
data SomeSpans
  = forall trace. (KnownTrace trace) => SomeSpans !(Proxy trace) [Span]

-------------------------------------------------------------------------------
-- KnownTrace & Instances
-------------------------------------------------------------------------------

type KnownTrace :: Type -> Constraint
class
  ( HasField (GetTraceName trace) Traces (Maybe trace)
  , IsTraceProcessorConfig trace
  , Show trace
  , Default trace
  , KnownSymbol (GetTraceName trace)
  ) =>
  KnownTrace trace
  where
  type GetTraceName trace :: Symbol

traceConfig :: forall trace. (KnownTrace trace) => Traces -> Maybe trace
traceConfig = getField @(GetTraceName trace)
{-# INLINE traceConfig #-}

instance KnownTrace CapabilityUsageSpan where
  type GetTraceName CapabilityUsageSpan = "capabilityUsage"

instance KnownTrace ThreadStateSpan where
  type GetTraceName ThreadStateSpan = "threadState"

--------------------------------------------------------------------------------
-- Superclass for span types
--------------------------------------------------------------------------------

{- |
A span is any type with a start and end time.
-}
type IsSpan s k =
  ( HasField "traceId" s k
  , HasField "startTimeUnixNano" s Timestamp
  , HasField "endTimeUnixNano" s Timestamp
  , HasField "attrs" s Attrs
  , Hashable k
  )

{- |
Determine the duration of a span.
-}
duration ::
  ( HasField "startTimeUnixNano" s Timestamp
  , HasField "endTimeUnixNano" s Timestamp
  ) =>
  s -> Timestamp
duration s
  | s.startTimeUnixNano < s.endTimeUnixNano = s.endTimeUnixNano - s.startTimeUnixNano
  | otherwise = 0
{-# INLINEABLE duration #-}

--------------------------------------------------------------------------------
-- Generic span type
--------------------------------------------------------------------------------

-- TODO:
-- The current `Span` type only supports a small subset of OpenTelemetry's
-- specification for spans, e.g., it doesn't support parent spans, links,
-- or events, doesn't support span kinds, and doesn't support status codes.

data Span = Span
  { traceId :: !ByteString
  , spanId :: !ByteString
  , startTimeUnixNano :: !Timestamp
  , endTimeUnixNano :: !Timestamp
  , attrs :: Attrs
  }

data ToSpanState k = ToSpanState
  { traceIdMap :: !(HashMap k ByteString)
  , stdGen :: !StdGen
  }

initToSpanState :: StdGen -> ToSpanState k
initToSpanState stdGen = ToSpanState{traceIdMap = HM.empty, ..}

asSpan :: (MonadIO m, IsSpan s k) => ProcessT m s Span
asSpan = construct $ go Nothing
 where
  -- go :: Maybe (ToSpanState k) -> PlanT (Is s) Span m Void
  go Nothing = do
    stdGen <- liftIO initStdGen
    go (Just $ initToSpanState stdGen)
  go (Just st) =
    await >>= \s -> do
      let (s', st') = runState (toSpan s) st
      yield s'
      go (Just st')

toSpan :: (IsSpan s k) => s -> State (ToSpanState k) Span
toSpan s = do
  traceId <- state (getTraceId s.traceId)
  spanId <- state getSpanId
  pure
    Span
      { startTimeUnixNano = s.startTimeUnixNano
      , endTimeUnixNano = s.endTimeUnixNano
      , attrs = s.attrs
      , ..
      }

getTraceId :: (Hashable k) => k -> ToSpanState k -> (ByteString, ToSpanState k)
getTraceId k st =
  let ((traceId, stdGen), traceIdMap) = HM.alterF ensureTraceId k st.traceIdMap
   in (traceId, st{traceIdMap = traceIdMap, stdGen = stdGen})
 where
  ensureTraceId :: Maybe ByteString -> ((ByteString, StdGen), Maybe ByteString)
  ensureTraceId = \case
    Nothing ->
      ((traceId, stdGen'), Just traceId)
     where
      (traceId, stdGen') = uniformByteString 16 st.stdGen
    Just traceId ->
      ((traceId, st.stdGen), Just traceId)

getSpanId :: ToSpanState k -> (ByteString, ToSpanState k)
getSpanId st =
  let (spanId, stdGen) = uniformByteString 8 st.stdGen
   in (spanId, st{stdGen = stdGen})
