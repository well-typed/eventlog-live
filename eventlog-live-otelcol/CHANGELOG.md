### 0.7.0.0

- **BREAKING**: Change default semantics of the configuration file.

  Starting from version 0.7.0.0, if the key for any specific processor
  is present in the file, even if it sets none of the properties, the
  processor is _enabled_, and any missing keys will default to the values
  in this file. Otherwise, the processor is _disabled_. For instance,
  the following configuration will run _only_ the `heap_prof_sample`
  processor with the default configuration.

  ```yaml
  processors:
    metrics:
      heap_prof_sample:
  ```

  Previously, if any key in the configuration file was commented out,
  it was given the value in the default configuration. Unfortunately,
  this was very unintuitive. Consider the following configuration...

  ```yaml
  processors:
    metrics:
      heap_prof_sample:
        name: ghc_eventlog_HeapProfSample
        aggregate: 5s
        export: 5s
  ```

  Previously, this would activate `heap_prof_sample` with the given
  configuration and activate _all other processors_ with their default
  configuration. Moreover, if the user edited the file and commented out
  the `heap_prof_sample` section...

  ```yaml
  processors:
    metrics:
  #     heap_prof_sample:
  #       name: ghc_eventlog_HeapProfSample
  #       aggregate: 5s
  #       export: 5s
  ```

  ...this would _not_ deactive the `heap_prof_sample` processor. Rather,
  it would keep the `heap_prof_sample` processor active with its default
  configuration.

### 0.6.1.0

- Support eventlog over TCP/IP.

### 0.6.0.0

- **BREAKING**: Drop `enabled` from configuration files.
- **BREAKING**: Change value of `aggregate` in configuration files to ``(boolean | `${number}x`)``.
- Support logs and markers (`thread_label`, `user_marker`, `user_message`, `internal_log_message`).
- Support internal telemetry (currently only `internal_log_message`).
- Support cost-centre profiles.
- Support `ghc-stack-profiler` call-stack profiles.
- **BREAKING**: Use OTLP v1.9.
- Support an optional control server via a REST API.

### 0.5.0.0

- **BREAKING**: Put `ghc-debug-stub` support behind `use-ghc-debug-stub` feature flag.

### 0.4.0.0

- Fix space leak in `processHeapProfSampleData`.
- Support aggregation (via configuration files).
- Support statistics with the `--stats` flag:

  ```
  ┌────────┬──────────┬──────────────┬───────────────┬───────────────────┐
  │  Item  │  Action  │ Total (item) │ Rate (item/s) │ Peak (item/batch) │
  ╞════════╪══════════╪══════════════╪═══════════════╪═══════════════════╡
  │ Event  │ Received │        66036 │        6603/s │           22197/s │
  ├────────┼──────────┼──────────────┼───────────────┼───────────────────┤
  │ Metric │ Exported │          274 │          45/s │              59/s │
  ├────────┼──────────┼──────────────┼───────────────┼───────────────────┤
  │        │ Rejected │            0 │           0/s │               0/s │
  ├────────┼──────────┼──────────────┼───────────────┼───────────────────┤
  │ Span   │ Exported │        19372 │        3228/s │            5979/s │
  ├────────┼──────────┼──────────────┼───────────────┼───────────────────┤
  │        │ Rejected │            0 │           0/s │               0/s │
  └────────┴──────────┴──────────────┴───────────────┴───────────────────┘
  ```

### 0.3.0.0

- Support configuration files (via `--config`; see `--print-defaults`).
- **BREAKING**: Remove `--otelcol-no-metrics` and `--otelcol-no-traces` flags.
  The behaviour of these flags is superceded by the configuration files.

### 0.2.0.0

- **BREAKING**: Support info and debug verbosity. This changes the semantics of `--verbosity=3` and up.
- Support reading the eventlog from stdin.
- Support reading the eventlog from file.

### 0.1.0.3

- Support `random-1.2`

### 0.1.0.2

- Lower `cabal-version` constraint to `3.0`

### 0.1.0.0

- Initial release.
