### 0.4.0.0

- Add `averageCounterBy` and `averageCounterByTick`.
- Add `statsParser`.
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
