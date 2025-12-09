### 0.6.0.0

- **BREAKING**: Drop `enabled` from configuration files.
- **BREAKING**: Change value of `aggregate` in configuration files to ``(boolean | `${number}x`)``.
- Support logs and markers (`thread_label`, `user_marker`, `user_message`, `internal_log_message`).
- Support internal telemetry (currently only `internal_log_message`).

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
