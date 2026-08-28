### 0.9.0.1

- Add support for cumulative `Productivity` metric computed by Eventlog Live,
  rather than from `CapabilityUsage` in Grafana.

- Fix bug where `HeapAllocated` metric was erroneously treated as a delta
  metric and aggregated by summation. From this version onwards, it is treated
  as a cumulative metric and aggregated by taking the most recent measurement.

- Added workaround where `CapabilityUsage` metric is exported as a cumulative
  metric, as deltas are not supported by most collectors (Alloy, Prometheus).

### 0.9.0.0

- Add support for the following OTLP exporter configuration options:
  - `OTEL_EXPORTER_OTLP_TIMEOUT` (full support with both `grpc` and `http/protobuf`)
  - `OTEL_EXPORTER_OTLP_COMPRESSION` (full support with both `grpc` and `http/protobuf`)
  - `OTEL_EXPORTER_OTLP_CLIENT_KEY` (not parsed, as mTLS is unsupported)
  - `OTEL_EXPORTER_OTLP_CLIENT_CERTIFICATE` (not parsed, as mTLS is unsupported)

- Add support for `console` exporter (e.g., via `OTEL_METRICS_EXPORTER=console`).

- Add support for HTTP proxy environment variables (`http_proxy` and `https_proxy`)
  when using HTTP/Protobuf exporter.

- Fix bug where `OTEL_EXPORTER_OTLP_INSECURE` applied to `http/protobuf` exporters.

- Fix bug where `OTEL_EXPORTER_OTLP_ENDPOINT` accepted URLs without scheme for `http/protobuf` exporters.

- Fix bug where `writeLog` shows in the backtraces for error messages.

- Add documentation for OpenTelemetry environment variables to command-line `--help`.

### 0.8.0.0

The version 0.7.0.0 was skipped to avoid confusion with `eventlog-live-otelcol-0.7.0.0`.

- Merge the executable `eventlog-live-otelcol` into this package as `eventlog-live-otlp`.

- Add support for OpenTelemetry Environment Variable Configuration:

  The following SDK configuration options are supported:
  https://opentelemetry.io/docs/specs/otel/configuration/sdk-environment-variables/#general-sdk-configuration
  - `OTEL_RESOURCE_ATTRIBUTES`
  - `OTEL_SERVICE_NAME`
  - `OTEL_LOG_LEVEL`
  - `OTEL_TRACES_EXPORTER`, `OTEL_METRICS_EXPORTER`, `OTEL_LOGS_EXPORTER`, and `OTEL_PROFILES_EXPORTER`

  The following OTLP exporter configuration options are supported:
  https://opentelemetry.io/docs/specs/otel/protocol/exporter/
  - `OTEL_EXPORTER_OTLP_ENDPOINT`
  - `OTEL_EXPORTER_OTLP_INSECURE`
  - `OTEL_EXPORTER_OTLP_CERTIFICATE` (only by `grpc` protocol)
  - `OTEL_EXPORTER_OTLP_CLIENT_KEY` (parsed but ignored)
  - `OTEL_EXPORTER_OTLP_CLIENT_CERTIFICATE` (parsed but ignored)
  - `OTEL_EXPORTER_OTLP_HEADERS` (only by `http/protobuf` protocol)
  - `OTEL_EXPORTER_OTLP_TIMEOUT` (parsed but ignored, support to be added in next version)
  - `OTEL_EXPORTER_OTLP_PROTOCOL` (only `grpc` and `http/protobuf` protocols are supported)

  All special-specific variants of the above environment variables are supported, e.g., `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT`, `OTEL_EXPORTER_OTLP_METRICS_ENDPOINT`, `OTEL_EXPORTER_OTLP_LOGS_ENDPOINT`, and `OTEL_EXPORTER_OTLP_PROFILES_ENDPOINT` are all parsed and used.

  The command-line arguments that controlled these options have been removed:

  ```
  --service-name                   -> OTEL_SERVICE_NAME
  --verbosity                      -> OTEL_LOG_LEVEL
  --otlp-protocol                  -> OTEL_EXPORTER_OTLP_PROTOCOL
  --otlp-endpoint                  -> OTEL_EXPORTER_OTLP_ENDPOINT
  --otlp-grpc-certificate-store    -> OTEL_EXPORTER_OTLP_CERTIFICATE
  --otlp-http-headers              -> OTEL_EXPORTER_OTLP_HEADERS
  ```

- The `--otlp-grpc-ssl-key-log` and `--otlp-grpc-ssl-key-log-from-env` options were removed.

  Support for setting a gRPC SSL keylog was removed. If you relied on this, please open an issue.

- Support was added for _signal-specific exporters_, e.g., if you supply different values for `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT` and `OTEL_EXPORTER_OTLP_METRICS_ENDPOINT` then traces and metrics are exported to different endpoints.

  You can use signal-specific exporter selection to disable certain signals altogether, e.g., `OTEL_TRACES_EXPORTER=none` causes no traces to be exported.

### 0.6.0.0

- Add support for `ipedb` databases for IPE and cost-centre information.
- **BREAKING**: Refactor profiles processors.
- **BREAKING**: Use `InfoProv`, `CostCentre`, and `SrcLoc` types from `ipedb`.

### 0.5.0.0

- Overhaul documentation for `GHC.Eventlog.Live.Machine.Core`.
- **BREAKING**: Drop `counterBy` and `counterByTick`.
- **BREAKING**: Drop `batchListToTick` and `batchListToTicks`.
- **BREAKING**: Drop `batchToTick` and `batchToTicks`.
- **BREAKING**: Drop `aggregateByTick` and `aggregateByTicks`.
- **BREAKING**: Drop `liftBatch`.
- **BREAKING**: Rename `sortByBatchTick` to `sortByTick`.
- **BREAKING**: Change `batchByTick` and `batchByTicks` to preserve ticks.
- **BREAKING**: Generalise `sortByBatch`, `sortByTick`, and `validateOrder` to work on arbitrary keys.
- **BREAKING**: Rename `between` to `betweenFirst`.
- **BREAKING**: Generalise `betweenFirst` and `delimit` to work on arbitrary items.
- Add `betweenEach`.
- **BREAKING**: Change `liftRouter` to ignore inputs after the child process stops.
- Add `fanoutTick`.
- **BREAKING**: Add implicit `TickInfo` to each `Tick` via `HasTickInfo`.
- **BREAKING**: Change `onlyTick` to yield actual `Tick` values.
- **BREAKING**: Change interval argument for `runWithEventlogSource` and `sourceHandleBatch` to batch in milliseconds.
- **BREAKING**: Add `AttrBoot` to `AttrValue`.
- Add support for user messages and markers.
- **BREAKING**: Replace logging with `co-log-core` contravariant `LogAction`.
- Add support for concurrent `fanoutTickCC` and `mergeWithTickCC`.
- **BREAKING**: Rename `EventlogSource` to `EventlogSourceOptions`.
- **BREAKING**: Rename `GHC.Eventlog.Live.Socket` to `GHC.Eventlog.Live.Source`.
- Add `GHC.Eventlog.Live.Source.Core`.
- **BREAKING**: Move `EventlogSourceOptions` to `GHC.Eventlog.Live.Source.Core`.
- Add `EventlogSourceHandle`.
- **BREAKING**: Rename `runWithEventlogSource` to `runWithEventlogSourceOptions`.
- Add `withEventlogSourceHandle` and `runWithEventlogSourceHandle` to `GHC.Eventlog.Live.Source`.
- **BREAKING**: Rename `sourceHandleBatch` to `eventlogSourceTick`.

### 0.4.0.0

- Add parser for `--stats` flag (`statsParser`).
- Add support for aggregation:
  - Add `aggregateByTick`.
  - Add `GHC.Eventlog.Live.Machine.Group` module for aggregation.
- **BREAKING**: Change `processHeapProfSampleData` to yield all the metrics
  from a single garbage collection pass at once as `HeapProfSampleData`.
- **BREAKING**: Replace `[Attr]` with opaque `Attrs` type.
- **BREAKING**: Log info to stderr with `--verbosity==debug`.

### 0.3.0.0

- **BREAKING**: Move capability usage analysis machines to their own module.
- **BREAKING**: Move heap analysis machines to their own module.
- **BREAKING**: Move thread label analysis machines to their own module.
- **BREAKING**: Move thread state analysis machines to their own module.
- **BREAKING**: Merge all sub-libraries into the main library.
- Expose `GHC.Eventlog.Live.Logger`.

### 0.2.0.1

- Fix error due to incorrect formatting string.

### 0.2.0.0

- Add `verbosityInfo` and `verbosityDebug`.
- Add `counterBy` and `counterByTick`.
- **BREAKING**: Add logs to `runWithEventlogSocket` connection loop.
- **BREAKING**: Downgrade severity of all errors logged during eventlog analyses to warnings.
- **BREAKING**: Move attributes, metrics, and spans into their own modules.
- **BREAKING**: Drop `Stream` and `supplier` machine.
- **BREAKING**: Drop `WithMainThreadId` and `withMainThreadId` machines.
- **BREAKING**: Rename `GHC.Eventlog.Live.Machines` to `GHC.Eventlog.Live.Machine`.
- **BREAKING**: Move core machines to their own module.
- **BREAKING**: Use ANSI colors for logs, if supported.
- Support reading the eventlog from stdin.
- Support reading the eventlog from file.
- **BREAKING**: Rename `EventlogSocket` and related functions to `EventlogSource`.

### 0.1.0.1

- Drop unused package `transformers` from dependencies.
- Lower `cabal-version` constraint to `3.0`

### 0.1.0.0

- Initial release.
