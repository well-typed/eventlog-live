{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Test (
  -- * Running a machine-based assertion on `ResourceTelemetryData`
  assertResourceTelemetryData,
  hasInput,

  -- * Running an OTLP server
  ResourceTelemetryData (..),
  HasOtlpServerInfo,
  OtlpServerInfo (..),
  withGrpcOtlpServer,

  -- * Running @eventlog-live-otelcol@
  HasEventlogLiveOtelcolInfo,
  withEventlogLiveOtelcol,

  -- * Running `ProgramTest`
  programTestFor,

  -- * Re-export "GHC.Eventlog.Socket.Test"
  module Export.GEST,
) where

import Control.Concurrent.STM (newTQueueIO, readTQueue)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Data.Machine (ProcessT, await, construct, repeatedly, runT_, stop, traversing, yield, (~>))
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Eventlog.Socket.Test as Export.GEST hiding (programTestFor)
import GHC.Eventlog.Socket.Test qualified as GEST (programTestFor)
import Network.GRPC.Common qualified as G
import Network.GRPC.Common.Protobuf ((^.))
import Network.GRPC.Common.Protobuf qualified as G
import Network.GRPC.Server qualified as G
import Network.GRPC.Server.Run qualified as G
import Network.GRPC.Server.StreamType qualified as G
import Network.Socket (HostName, PortNumber)
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as OLS
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as OLS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as OMS
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService qualified as OPS
import Proto.Opentelemetry.Proto.Collector.Profiles.V1development.ProfilesService_Fields qualified as OPS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as OTS
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as OTS
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import System.Process (getCurrentPid)
import Test.Tasty (TestName)
import Test.Tasty.HUnit (Assertion)

hasInput :: ProcessT IO i o
hasInput = construct (await >>= const stop)

programTestFor ::
  (HasLogger) =>
  TestName ->
  Program ->
  [String] ->
  ((HasTestInfo, HasProgramInfo, HasOtlpServerInfo, HasEventlogLiveOtelcolInfo) => Assertion) ->
  EventlogSocketAddr ->
  ProgramTest
programTestFor testName program eventlogLiveOtelcolArgs assertion =
  GEST.programTestFor testName program (const action)
 where
  action :: (HasTestInfo, HasProgramInfo) => Assertion
  action =
    withGrpcOtlpServer $
      withEventlogLiveOtelcol eventlogLiveOtelcolArgs $
        assertion

assertResourceTelemetryData :: (HasLogger, HasTestInfo, HasOtlpServerInfo) => ProcessT IO ResourceTelemetryData x -> Assertion
assertResourceTelemetryData validateResourceTelemetryData =
  runT_ $ source ~> logging ~> validateResourceTelemetryData
 where
  OtlpServerInfo{..} = ?otlpServerInfo
  source = repeatedly (yield =<< liftIO next)

logging :: (HasLogger, HasTestInfo, Show i) => ProcessT IO i i
logging = traversing (\i -> liftIO (debugInfo (show i)) >> pure i)

type HasEventlogLiveOtelcolInfo = (?eventlogLiveOtelcolInfo :: ProgramInfo)

{- |
Start an instance of @eventlog-live-otelcol@ with the given arguments.

The eventlog socket is automatically configured based on the eventlog socket address in the `ProgramInfo`.

The otelcol socket is automatically configured based on the port in the `OtlpServerInfo`.
-}
withEventlogLiveOtelcol ::
  (HasLogger, HasTestInfo, HasProgramInfo, HasOtlpServerInfo) =>
  [String] ->
  ((HasLogger, HasTestInfo, HasProgramInfo, HasOtlpServerInfo, HasEventlogLiveOtelcolInfo) => IO ()) ->
  IO ()
withEventlogLiveOtelcol extraArgs action = do
  let programInfo@ProgramInfo{..} = ?programInfo

  -- Configure the eventlog socket.
  let throwMissingEventlogSocketError = error $ "Program " <> program.programDesc <> " has no eventlog socket."
  eventlogSocketAddr <- maybe throwMissingEventlogSocketError pure program.maybeEventlogSocket
  let eventlogSocketArgs =
        case eventlogSocketAddr of
          EventlogSocketUnixAddr{..} ->
            ["--eventlog-socket=" <> esaUnixPath]
          EventlogSocketInetAddr{..} ->
            ["--eventlog-socket-host=" <> esaInetHost, "--eventlog-socket-port=" <> esaInetPort]

  -- Configure the otelcol socket.
  let OtlpServerInfo{..} = ?otlpServerInfo
  let otelcolArgs =
        ["--otelcol-host=" <> host, "--otelcol-port=" <> show port]

  -- Configure the eventlog-live-otelcol program.
  let args = extraArgs <> eventlogSocketArgs <> otelcolArgs
  let eventlogLiveOtelcolProgram = (findProgram "eventlog-live-otelcol"){args = args}
  withProgramResource (programResource eventlogLiveOtelcolProgram) $ do
    -- NOTE: By the GHC gods, please let this have the correct semantics.
    let ?eventlogLiveOtelcolInfo = ?programInfo
    let ?programInfo = programInfo
    action

type HasOtlpServerInfo = (?otlpServerInfo :: OtlpServerInfo)

data OtlpServerInfo = OtlpServerInfo
  { next :: IO ResourceTelemetryData
  , host :: HostName
  , port :: PortNumber
  }

withGrpcOtlpServer ::
  (HasLogger, HasTestInfo) =>
  ((HasLogger, HasTestInfo, HasOtlpServerInfo) => IO a) ->
  IO a
withGrpcOtlpServer action = do
  -- Create logging scope for the OTLP server.
  let programPidIO = Just <$> getCurrentPid
  programPid <- programPidIO
  let program = findProgram "otelcol"
  let programInfo = ProgramInfo{..}
  let debugServerInfo msg = debug (ProgramOut programInfo msg)

  -- Create the queue for the ResourceTelemetryData.
  queue <- newTQueueIO

  -- Create the action that enqueues data.
  let enqueue :: ResourceTelemetryData -> IO ()
      enqueue = atomically . writeTQueue queue

  -- Create the handlers for various kinds of telemetry exports.
  let logsServiceExportHandler :: G.ServerHandler IO (G.Protobuf OLS.LogsService "export")
      logsServiceExportHandler = G.mkNonStreaming . liftProto $ \req -> do
        let logs = req ^. OLS.vec'resourceLogs
        debugServerInfo $ "Received " <> show (V.length logs) <> " resource logs"
        enqueue (ResourceTelemetryData'Logs logs)
        pure G.defMessage
  let metricsServiceExportHandler :: G.ServerHandler IO (G.Protobuf OMS.MetricsService "export")
      metricsServiceExportHandler = G.mkNonStreaming . liftProto $ \req -> do
        let metrics = req ^. OMS.vec'resourceMetrics
        debugServerInfo $ "Received " <> show (V.length metrics) <> " resource metrics"
        enqueue (ResourceTelemetryData'Metrics metrics)
        pure G.defMessage
  let profilesServiceExportHandler :: G.ServerHandler IO (G.Protobuf OPS.ProfilesService "export")
      profilesServiceExportHandler = G.mkNonStreaming . liftProto $ \req -> do
        let profiles = req ^. OPS.vec'resourceProfiles
        debugServerInfo $ "Received " <> show (V.length profiles) <> " resource profiles"
        enqueue (ResourceTelemetryData'Profiles profiles)
        pure G.defMessage
  let tracesServiceExportHandler :: G.ServerHandler IO (G.Protobuf OTS.TraceService "export")
      tracesServiceExportHandler = G.mkNonStreaming . liftProto $ \req -> do
        let spans = req ^. OTS.vec'resourceSpans
        debugServerInfo $ "Received " <> show (V.length spans) <> " resource spans"
        enqueue (ResourceTelemetryData'Spans spans)
        pure G.defMessage

  -- Create the server methods.
  let methods =
        G.fromMethods $
          G.Method logsServiceExportHandler $
            G.Method metricsServiceExportHandler $
              G.Method profilesServiceExportHandler $
                G.Method tracesServiceExportHandler $
                  G.NoMoreMethods

  -- Create the server.
  server <- G.mkGrpcServer G.def methods

  -- Create the server configuration.
  let host = "127.0.0.1"
  let serverConfig =
        G.ServerConfig
          { G.serverInsecure =
              Just
                G.InsecureConfig
                  { G.insecureHost = Just host
                  , G.insecurePort = 0 -- Let the server pick its own port.
                  }
          , G.serverSecure = Nothing
          }

  -- Start the server.
  debugInfo $ "Starting OTLP server on " <> host <> " with dynamic port selection"
  G.forkServer G.def serverConfig server $ \runningServer -> do
    -- Get the server port.
    port <- G.getServerPort runningServer
    debugServerInfo $ "Started OTLP server on " <> host <> ":" <> show port

    -- Create the action that dequeues data.
    let next :: IO ResourceTelemetryData
        next = atomically $ readTQueue queue

    -- Run the continuation.
    let ?otlpServerInfo = OtlpServerInfo{..}
    action

-- | Lift a monadic action into a gRPC handler.
liftProto :: (Monad m) => (a -> m b) -> G.Proto a -> m (G.Proto b)
liftProto f pa = G.Proto <$> f (G.getProto pa)

--------------------------------------------------------------------------------
-- Code shared with eventlog-live-otelcol
--------------------------------------------------------------------------------

-- NOTE: This type is different from the type with the same name found in
--       eventlog-live-otelcol, as the latter still uses `OP.ProfileData`.
--       This should be changed, but requires the refactoring that moves the
--       profiles machinery into eventlog-live.
data ResourceTelemetryData
  = ResourceTelemetryData'Logs !(Vector OL.ResourceLogs)
  | ResourceTelemetryData'Metrics !(Vector OM.ResourceMetrics)
  | ResourceTelemetryData'Profiles !(Vector OP.ResourceProfiles)
  | ResourceTelemetryData'Spans !(Vector OT.ResourceSpans)
  deriving (Show)

type instance G.RequestMetadata (G.Protobuf OLS.LogsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (G.Protobuf OLS.LogsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (G.Protobuf OLS.LogsService meth) = G.NoMetadata

type instance G.RequestMetadata (G.Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (G.Protobuf OMS.MetricsService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (G.Protobuf OMS.MetricsService meth) = G.NoMetadata

type instance G.RequestMetadata (G.Protobuf OPS.ProfilesService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (G.Protobuf OPS.ProfilesService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (G.Protobuf OPS.ProfilesService meth) = G.NoMetadata

type instance G.RequestMetadata (G.Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseInitialMetadata (G.Protobuf OTS.TraceService meth) = G.NoMetadata
type instance G.ResponseTrailingMetadata (G.Protobuf OTS.TraceService meth) = G.NoMetadata
