{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Eventlog.Live.Test (
  -- * Running a machine-based assertion on `ResourceTelemetryData`
  assertResourceTelemetryData,
  hasInput,
  withLogRecord'body,
  toLogRecords,
  withMetric'name,
  toMetrics,
  toProfiles,
  toSpans,
  withScope,
  toScopeLogs,
  toScopeMetrics,
  toScopeProfiles,
  toScopeSpans,
  withServiceName,
  withResource,
  toResourceLogs,
  toResourceMetrics,
  toResourceProfiles,
  toResourceSpans,
  logging,

  -- * Running `ProgramTest`
  programTestFor,

  -- * Running @eventlog-live-otelcol@
  EventlogLiveOtelcolOptions (extraArgs, maybeConfigBody),
  defaultOptions,
  HasEventlogLiveOtelcolInfo,
  withEventlogLiveOtelcol,

  -- * Running an OTLP server
  ResourceTelemetryData (..),
  HasOtlpServerInfo,
  OtlpServerInfo (..),
  withGrpcOtlpServer,

  -- * Re-export "GHC.Eventlog.Socket.Test"
  module Export.GEST,
) where

import Control.Concurrent.STM (newTQueueIO, readTQueue)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.STM (atomically)
import Data.Foldable (find, traverse_)
import Data.Machine (ProcessT, asParts, await, construct, filtered, mapping, repeatedly, runT_, stop, traversing, yield, (~>))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Eventlog.Socket.Test as Export.GEST hiding (programTestFor)
import GHC.Eventlog.Socket.Test qualified as GEST (programTestFor)
import GHC.Exts (Proxy#, proxy#)
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
import Proto.Opentelemetry.Proto.Common.V1.Common qualified as OC
import Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as OC
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as OL
import Proto.Opentelemetry.Proto.Logs.V1.Logs_Fields qualified as OL
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as OM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as OM
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles qualified as OP
import Proto.Opentelemetry.Proto.Profiles.V1development.Profiles_Fields qualified as OP
import Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as OR
import Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as OR
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OS
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as OT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as OS
import System.IO qualified as IO
import System.IO.Temp (withSystemTempFile)
import System.Process (getCurrentPid)
import Test.Tasty (TestName)
import Test.Tasty.HUnit (Assertion)

--------------------------------------------------------------------------------
-- Running a machine-based assertion on `ResourceTelemetryData`
--------------------------------------------------------------------------------

{- |
Assert that any input is received.
-}
hasInput :: (Monad m) => ProcessT m i o
hasInput = construct (await >>= const stop)

{- |
Filters a stream of scope telemetry data to only those whose scope satisfies a given predicate.
-}
withScope ::
  forall m s.
  (Monad m, G.HasField s "maybe'scope" (Maybe OC.InstrumentationScope)) =>
  (Maybe OC.InstrumentationScope -> Bool) -> ProcessT m s s
withScope p = filtered (p . maybeScope)
 where
  maybeScope :: s -> Maybe OC.InstrumentationScope
  maybeScope = (^. G.fieldOf (proxy# :: Proxy# "maybe'scope"))

{- |
Filters a stream of resource telemetry data to only those for a given service.
-}
withServiceName ::
  forall m s.
  (Monad m, G.HasField s "maybe'resource" (Maybe OR.Resource)) =>
  Text -> ProcessT m s s
withServiceName serviceName =
  withResource (maybe False $ (Just serviceName ==) . toServiceName)
 where
  toServiceName :: OR.Resource -> Maybe Text
  toServiceName resource = do
    let keyIsServiceName = ("service.name" ==) . (^. OC.key)
    keyValue <- find keyIsServiceName (resource ^. OR.vec'attributes)
    keyValue ^. OC.value . OC.maybe'stringValue

{- |
Filters a stream of resource telemetry data to only those whose resource satisfies a given predicate.
-}
withResource ::
  forall m s.
  (Monad m, G.HasField s "maybe'resource" (Maybe OR.Resource)) =>
  (Maybe OR.Resource -> Bool) -> ProcessT m s s
withResource p = filtered (p . maybeResource)
 where
  maybeResource :: s -> Maybe OR.Resource
  maybeResource = (^. G.fieldOf (proxy# :: Proxy# "maybe'resource"))

{- |
Filters a stream of log records to only those whose body satisfies a given predicate.
-}
withLogRecord'body :: (Monad m) => (Text -> Bool) -> ProcessT m OL.LogRecord OL.LogRecord
withLogRecord'body p = filtered (maybe False p . ((^. OC.maybe'stringValue) <=< (^. OL.maybe'body)))

{- |
Stream scope logs as individual log records.
-}
toLogRecords :: (Monad m) => ProcessT m OL.ScopeLogs OL.LogRecord
toLogRecords = mapping (^. OL.vec'logRecords) ~> asParts

{- |
Filters a stream of metrics to only those whose name satisfies a given predicate.
-}
withMetric'name :: (Monad m) => (Text -> Bool) -> ProcessT m OM.Metric OM.Metric
withMetric'name p = filtered (p . (^. OM.name))

{- |
Stream scope metrics as individual metrics.
-}
toMetrics :: (Monad m) => ProcessT m OM.ScopeMetrics OM.Metric
toMetrics = mapping (^. OM.vec'metrics) ~> asParts

{- |
Stream scope profiles as individual profiles.
-}
toProfiles :: (Monad m) => ProcessT m OP.ScopeProfiles OP.Profile
toProfiles = mapping (^. OP.vec'profiles) ~> asParts

{- |
Stream scope spans as individual spans.
-}
toSpans :: (Monad m) => ProcessT m OS.ScopeSpans OS.Span
toSpans = mapping (^. OS.vec'spans) ~> asParts

{- |
Stream resource logs as individual scope logs.
-}
toScopeLogs :: (Monad m) => ProcessT m OL.ResourceLogs OL.ScopeLogs
toScopeLogs = mapping (^. OL.vec'scopeLogs) ~> asParts

{- |
Stream resource metrics as individual scope metrics.
-}
toScopeMetrics :: (Monad m) => ProcessT m OM.ResourceMetrics OM.ScopeMetrics
toScopeMetrics = mapping (^. OM.vec'scopeMetrics) ~> asParts

{- |
Stream resource profiles as individual scope profiles.
-}
toScopeProfiles :: (Monad m) => ProcessT m OP.ResourceProfiles OP.ScopeProfiles
toScopeProfiles = mapping (^. OP.vec'scopeProfiles) ~> asParts

{- |
Stream resource spans as individual scope spans.
-}
toScopeSpans :: (Monad m) => ProcessT m OS.ResourceSpans OS.ScopeSpans
toScopeSpans = mapping (^. OS.vec'scopeSpans) ~> asParts

{- |
Filter a resource telemetry stream to only resource logs.
-}
toResourceLogs :: (Monad m) => ProcessT m ResourceTelemetryData OL.ResourceLogs
toResourceLogs =
  repeatedly $
    await >>= \case
      ResourceTelemetryData'Logs logs -> traverse_ yield logs
      _otherwise -> pure ()

{- |
Filter a resource telemetry stream to only resource metrics.
-}
toResourceMetrics :: (Monad m) => ProcessT m ResourceTelemetryData OM.ResourceMetrics
toResourceMetrics =
  repeatedly $
    await >>= \case
      ResourceTelemetryData'Metrics metrics -> traverse_ yield metrics
      _otherwise -> pure ()

{- |
Filter a resource telemetry stream to only resource profiles.
-}
toResourceProfiles :: (Monad m) => ProcessT m ResourceTelemetryData OP.ResourceProfiles
toResourceProfiles =
  repeatedly $
    await >>= \case
      ResourceTelemetryData'Profiles profiles -> traverse_ yield profiles
      _otherwise -> pure ()

{- |
Filter a resource telemetry stream to only resource spans.
-}
toResourceSpans :: (Monad m) => ProcessT m ResourceTelemetryData OT.ResourceSpans
toResourceSpans =
  repeatedly $
    await >>= \case
      ResourceTelemetryData'Spans spans -> traverse_ yield spans
      _otherwise -> pure ()

{- |
Run a machine-based assertion on `ResourceTelemetryData`.
-}
assertResourceTelemetryData :: (HasOtlpServerInfo) => ProcessT IO ResourceTelemetryData x -> Assertion
assertResourceTelemetryData validateResourceTelemetryData =
  runT_ $ source ~> validateResourceTelemetryData
 where
  OtlpServerInfo{..} = ?otlpServerInfo
  source = repeatedly (yield =<< liftIO next)

{- |
Log each input.
-}
logging :: (HasLogger, HasTestInfo, Show i) => ProcessT IO i i
logging = traversing (\i -> liftIO (debugInfo (show i)) >> pure i)

--------------------------------------------------------------------------------
-- Running `ProgramTest`
--------------------------------------------------------------------------------

data EventlogLiveOtelcolOptions = EventlogLiveOtelcolOptions
  { extraArgs :: [String]
  , maybeConfigBody :: Maybe String
  }

defaultOptions :: EventlogLiveOtelcolOptions
defaultOptions =
  EventlogLiveOtelcolOptions
    { extraArgs = []
    , maybeConfigBody = Nothing
    }

{- |
Variant of `GEST.programTestFor` for @eventlog-live-otelcol@ tests.

This variant hides the eventlog socket from the continuation and manages the @eventlog-live-otelcol@ instance and the OTLP server.
-}
programTestFor ::
  (HasLogger) =>
  -- | The test name.
  TestName ->
  -- | The program to test against.
  Program ->
  -- | The command-line arguments for @eventlog-live-otelcol@.
  EventlogLiveOtelcolOptions ->
  -- | The test assertion.
  ((HasTestInfo, HasProgramInfo, HasOtlpServerInfo, HasEventlogLiveOtelcolInfo) => Assertion) ->
  EventlogSocketAddr ->
  ProgramTest
programTestFor testName program eventlogLiveOtelcolOptions assertion =
  GEST.programTestFor testName program (const action)
 where
  action :: (HasTestInfo, HasProgramInfo) => Assertion
  action =
    withGrpcOtlpServer $
      withEventlogLiveOtelcol eventlogLiveOtelcolOptions $
        assertion

--------------------------------------------------------------------------------
-- Running @eventlog-live-otelcol@
--------------------------------------------------------------------------------

{- |
An implicit constraint that requires an @eventlog-live-otelcol@ instance.
-}
type HasEventlogLiveOtelcolInfo = (?eventlogLiveOtelcolInfo :: ProgramInfo)

{- |
Start an instance of @eventlog-live-otelcol@ with the given arguments.

The eventlog socket is automatically configured based on the eventlog socket address in the `ProgramInfo`.

The otelcol socket is automatically configured based on the port in the `OtlpServerInfo`.
-}
withEventlogLiveOtelcol ::
  (HasLogger, HasTestInfo, HasProgramInfo, HasOtlpServerInfo) =>
  EventlogLiveOtelcolOptions ->
  ((HasLogger, HasTestInfo, HasProgramInfo, HasOtlpServerInfo, HasEventlogLiveOtelcolInfo) => IO ()) ->
  IO ()
withEventlogLiveOtelcol eventlogLiveOtelcolOptions action = do
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

  -- Create the temporary configuration file, if needed.
  withTempConfigFile eventlogLiveOtelcolOptions.maybeConfigBody $ \configArgs -> do
    -- Configure the eventlog-live-otelcol program.
    let args = eventlogLiveOtelcolOptions.extraArgs <> eventlogSocketArgs <> otelcolArgs <> configArgs
    let eventlogLiveOtelcolProgram = (findProgram "eventlog-live-otelcol"){args = args}
    withProgramResource (programResource eventlogLiveOtelcolProgram) $ do
      -- NOTE: By the GHC gods, please let this have the correct semantics.
      let ?eventlogLiveOtelcolInfo = ?programInfo
      let ?programInfo = programInfo
      action

{- |
Create a temporary configuration file with the given body, then run the action with access to that file.
-}
withTempConfigFile :: Maybe String -> ([String] -> IO a) -> IO a
withTempConfigFile maybeConfigBody action =
  case maybeConfigBody of
    Nothing ->
      action []
    Just configBody ->
      withSystemTempFile "eventlog-live-tests" $ \configFile configHandle -> do
        IO.hPutStrLn configHandle configBody >> IO.hClose configHandle
        action ["--config=" <> configFile]

--------------------------------------------------------------------------------
-- Running an OTLP server
--------------------------------------------------------------------------------

-- NOTE: This type is different from the type with the same name found in
--       eventlog-live-otelcol, as the latter still uses `OP.ProfileData`.
--       This should be changed, but requires the refactoring that moves the
--       profiles machinery into eventlog-live.

{- |
A batch of resource telemetry data.
-}
data ResourceTelemetryData
  = ResourceTelemetryData'Logs !(Vector OL.ResourceLogs)
  | ResourceTelemetryData'Metrics !(Vector OM.ResourceMetrics)
  | ResourceTelemetryData'Profiles !(Vector OP.ResourceProfiles)
  | ResourceTelemetryData'Spans !(Vector OT.ResourceSpans)
  deriving (Show)

{- |
An implicit constraint that requires an OTLP server.
-}
type HasOtlpServerInfo = (?otlpServerInfo :: OtlpServerInfo)

{- |
An instance of an OTLP server.

The `next` field contains an IO action that retrieves the next batch of resource telemetry data.

The `host` and `port` fields contain the information needed to connect to the server.
-}
data OtlpServerInfo = OtlpServerInfo
  { next :: IO ResourceTelemetryData
  , host :: HostName
  , port :: PortNumber
  }

{- |
Run a test with a gRPC OTLP server.
-}
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

-- TODO: These instances should be shared with eventlog-live-otelcol

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
