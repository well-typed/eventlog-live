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
