### 0.2.0.0

- Add `verbosityInfo` and `verbosityDebug`.
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
