{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Eventlog.Live.App.Viewer (
  ViewerServerApi (..),
  startViewerServer,
) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Exception (SomeException, handle)
import Control.Monad (forever)
import Data.Aeson qualified as JSON
import Data.Aeson.Types (KeyValue (..), KeyValueOmit (..), ToJSON, Value (..), pairs)
import Data.ByteString.Lazy qualified as BSL
import Data.DList (DList)
import Data.DList qualified as D
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Eventlog.Live.App.Viewer.AlignedData (AlignedData, toAlignedData)
import GHC.Eventlog.Live.App.Viewer.AlignedData qualified as AD
import GHC.Eventlog.Live.Logger (Logger, logException, logInfo, logTrace)
import GHC.Eventlog.Live.Machine.Core (Tick (..))
import GHC.Eventlog.Live.Processor (ExportRequest (..), InstrumentationScope, Resource, toSomeMetrics)
import GHC.Eventlog.Live.Types.Attribute ((~=))
import GHC.Eventlog.Live.Types.Metrics (KnownMetric (..), Metric, SomeMetrics, getMetrics)
import GHC.Eventlog.Live.Types.Metrics qualified as M
import GHC.TypeLits (symbolVal)
import Network.WebSockets qualified as WS
import Prelude hiding (null)

newtype ViewerServerApi = ViewerServerApi
  { stop :: IO ()
  }

startViewerServer :: Logger IO -> Chan (Tick ExportRequest) -> IO ViewerServerApi
startViewerServer logger exportRequestChan = do
  logInfo logger $ "Starting viewer server"

  threadId <-
    forkIO $
      handle (logException @SomeException logger) $
        WS.runServer "127.0.0.1" 30180 $
          viewerApp logger exportRequestChan

  let stop :: IO ()
      stop = killThread threadId

  pure ViewerServerApi{..}

viewerApp ::
  Logger IO -> Chan (Tick ExportRequest) -> WS.ServerApp
viewerApp logger exportRequestChan pending = do
  conn <- WS.acceptRequest pending
  chan <- dupChan exportRequestChan

  let handleConnectionException :: WS.ConnectionException -> IO ()
      handleConnectionException = \case
        WS.CloseRequest{} ->
          logInfo logger $ "Viewer requested to close the connection."
        WS.ConnectionClosed{} ->
          logInfo logger $ "Viewer closed the connection."
        wsErr -> logException logger wsErr

  let viewer :: IO ()
      viewer =
        forever $
          readChan chan >>= \case
            Tick ->
              pure ()
            Item er -> do
              let ver = toViewerExportRequest er
              let verBytes = JSON.encode ver
              let verByteSize = BSL.length verBytes
              logTrace logger $ "Send " <> T.show verByteSize <> " bytes to viewer."
              WS.sendTextData conn verBytes

  handle handleConnectionException viewer

data ViewerExportRequest = ViewerExportRequest
  { resource :: !Resource
  , scope :: !InstrumentationScope
  , telemetry :: !ViewerTelemetry
  }

viewerExportRequestToKV :: (KeyValueOmit e kv, Monoid kv) => ViewerExportRequest -> kv
viewerExportRequestToKV vt =
  mconcat
    [ "resource" .= vt.resource
    , "scope" .= vt.scope
    , "telemetry" .= vt.telemetry
    ]
{-# INLINE viewerExportRequestToKV #-}

instance ToJSON ViewerExportRequest where
  toJSON :: ViewerExportRequest -> JSON.Value
  toJSON = Object . viewerExportRequestToKV

  toEncoding :: ViewerExportRequest -> JSON.Encoding
  toEncoding = pairs . viewerExportRequestToKV

data ViewerTelemetry = ViewerTelemetry
  { heapSize :: Maybe (AlignedData Word64)
  , gcCopied :: Maybe (AlignedData Word64)
  , gcSlop :: Maybe (AlignedData Word64)
  , gcFragmentation :: Maybe (AlignedData Word64)
  }

viewerTelemetryToKV :: (KeyValueOmit e kv, Monoid kv) => ViewerTelemetry -> kv
viewerTelemetryToKV vt =
  mconcat
    [ "heapSize" .?= vt.heapSize
    , "gcCopied" .?= vt.gcCopied
    , "gcSlop" .?= vt.gcSlop
    , "gcFragmentation" .?= vt.gcFragmentation
    ]
{-# INLINE viewerTelemetryToKV #-}

instance ToJSON ViewerTelemetry where
  toJSON :: ViewerTelemetry -> JSON.Value
  toJSON = Object . viewerTelemetryToKV

  toEncoding :: ViewerTelemetry -> JSON.Encoding
  toEncoding = pairs . viewerTelemetryToKV

toViewerExportRequest :: ExportRequest -> ViewerExportRequest
toViewerExportRequest r =
  ViewerExportRequest
    { resource = r.resource
    , scope = r.scope
    , telemetry =
        ViewerTelemetry
          { heapSize = heapSize
          , gcCopied = gcCopied
          , gcSlop = gcSlop
          , gcFragmentation = gcFragmentation
          }
    }
 where
  someMetrics :: [SomeMetrics]
  someMetrics = mapMaybe toSomeMetrics r.telemetry

  heapSize, gcCopied, gcSlop, gcFragmentation :: Maybe (AlignedData Word64)
  gcCopied = alignKnownMetric (Proxy @"gcCopied") someMetrics
  gcSlop = alignKnownMetric (Proxy @"gcSlop") someMetrics
  gcFragmentation = alignKnownMetric (Proxy @"gcFragmentation") someMetrics
  heapSize = alignKnownMetrics (Proxy @"heapSize" :&: Proxy @"blocksSize" :&: Proxy @"heapLive" :&: End) someMetrics

infixr 0 :&:

data KnownMetrics (a :: Type) where
  (:&:) :: (KnownMetric metric, GetMetricType metric ~ a) => Proxy metric -> KnownMetrics a -> KnownMetrics a
  End :: KnownMetrics a

alignKnownMetric :: (KnownMetric metric) => Proxy metric -> [SomeMetrics] -> Maybe (AlignedData (GetMetricType metric))
alignKnownMetric metric =
  alignKnownMetrics (metric :&: End)

alignKnownMetrics :: KnownMetrics a -> [SomeMetrics] -> Maybe (AlignedData a)
alignKnownMetrics metrics =
  (`onlyIf` not . AD.null)
    . toAlignedData
    . D.toList
    . foldMap (getNamedMetrics metrics)

getNamedMetrics :: KnownMetrics a -> SomeMetrics -> DList (Metric a)
getNamedMetrics End _someMetrics = mempty
getNamedMetrics (metric :&: metrics) someMetrics =
  getNamedMetric metric someMetrics <> getNamedMetrics metrics someMetrics

getNamedMetric :: (KnownMetric metric) => Proxy metric -> SomeMetrics -> DList (Metric (GetMetricType metric))
getNamedMetric (metric :: Proxy metric) =
  fmap (addName metric) . maybe mempty D.fromList . getMetrics metric

addName :: (KnownMetric metric) => Proxy metric -> Metric (GetMetricType metric) -> Metric (GetMetricType metric)
addName metric x = x{M.attrs = ["name" ~= symbolVal metric] <> x.attrs}

onlyIf :: a -> (a -> Bool) -> Maybe a
onlyIf a p = if p a then Just a else Nothing
