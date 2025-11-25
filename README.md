# `eventlog-live`

> [!WARNING]
> This package is experimental.
> It is versioned according to the [PVP](https://pvp.haskell.org).
> However, breaking changes should be expected and no effort will be
> made to avoid major version bumps until at least version 1.0.0.0.

This repository contains a collection of libraries and tools for the live profiling of Haskell applications instrumented with [`eventlog-socket`](https://github.com/well-typed/ghc-eventlog-socket).

## Demo

The following is a screenshow of Grafana which shows live heap profiling statistics coming from the [`oddball`](examples/oddball) example application.

![A screenshot of Grafana showing live heap profiling statistics coming from the `oddball` example application.](assets/eventlog-live-otelcol.png)

To run this example for yourself, run the following command from the root of the repository, wait until all containers have started, then navigate to Grafana at <localhost:3000>, log in using username `admin` and password `admin`, and open the heap profiling visualisation under ☰ > _Dashboards_ > _Browse_ then _General_ > _Eventlog Heap_.

```sh
docker compose -f dockerfiles/eventlog-live-otelcol/docker-compose.yml up --build
```

This [Docker Compose](https://docs.docker.com/compose/) configuration builds and runs a number of Docker containers:

- [`oddball`](dockerfiles/Dockerfile.oddball)

  The monitored application, `oddball`, which repeatedly generates and sums large quantities of random numbers.

- [`eventlog-live-otelcol`](dockerfiles/Dockerfile.eventlog-live-otelcol)

  The `eventlog-live-otelcol` application, which streams eventlog data from `oddball` to the
  OpenTelemetry Collector.

- [`grafana/grafana-oss`](https://hub.docker.com/r/grafana/grafana-oss)

  The Grafana instance, which visualises the eventlog data.

- [`grafana/loki`](https://hub.docker.com/r/grafana/loki/)

  The Loki log processor and database, which acts as a log datasource for Grafana.

- [`prom/prometheus`](https://hub.docker.com/r/prom/prometheus)

  The Prometheus metric processor and database, which acts as a metric datasource for Grafana.

- [`grafana/tempo`](https://hub.docker.com/r/grafana/tempo)

  The Tempo span processor and database, which acts as a span datasource for Grafana.

- [`otel/opentelemetry-collector-contrib`](https://hub.docker.com/r/otel/opentelemetry-collector-contrib)

  The OpenTelemetry Collector, which streams the telemetry data to the various datasources.

## Getting Started

To use the code in this repository to profile your own application, follow these steps.

### Install `eventlog-live-otelcol`

This is the primary executable for `eventlog-live`. It can be installed using Cabal:

```sh
cabal install eventlog-live-otelcol
```

### Add `eventlog-socket` as a dependency

Add `eventlog-socket` to the `build-depends` for your application:

```cabal
executable my-app
  ...
  build-depends:
    ...
    , eventlog-socket  >=0.1.0 && <0.2
    ...
```

### Instrument your application for monitoring

To instrument your application, and allow the eventlog data to be streamed over a Unix socket, all you have to do is call `GHC.Eventlog.Socket.start` with the path to your socket.

```haskell
module Main where

import           Data.Foldable (traverse_)
import qualified GHC.Eventlog.Socket
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  putStrLn "Creating eventlog socket..."
  traverse_ GHC.Eventlog.Socket.start =<< lookupEnv "GHC_EVENTLOG_SOCKET"
  ...
```

This starts the `eventlog-socket` writer if the `GHC_EVENTLOG_SOCKET` environment variable is set.

If you wish for your application to block until the client process connects to the eventlog socket, you can call `GHC.Eventlog.Socket.startWait`.

### Build your application for monitoring

To enable monitoring your application with `eventlog-live`, you must build it with some GHC options. If you're looking for options to copy, these are the minimal additions.

```diff
  executable my-app
    ...
+   ghc-options: -rtsopts
+   ghc-options: -threaded
+   if impl(ghc < 9.4)
+     ghc-options: -eventlog
    ...
```

Let's briefly discuss why these options are needed:

- The [`-rtsopts`](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#ghc-flag-rtsopts-none-some-all-ignore-ignoreAll) flag enables the RTS options for your application. This allows us to enable the eventlog at runtime and enable various kinds of profiling. Setting this option may pose a security risk. If this is a concern, you can set all the required RTS options at compile time using [`-with-rtsopts`](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#ghc-flag-with-rtsopts-opts). (See [the next section](#configuring-your-application-for-monitoring) for the necessary RTS options).

- The [`-eventlog`](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#ghc-flag-with-rtsopts-opts) flag builds eventlog support into your application. This is enabled unconditionally since GHC 9.4, but if you're using GHC 9.2 or earlier, you must explicitly pass this flag.

- The [`-threaded`](https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#ghc-flag-threaded) flag builds your application with the threaded RTS. This is required because one crucial RTS option, `--eventlog-flush-interval`, is only safe to use with the threaded RTS.

### Configuring your application for monitoring

To monitor your application, you must pass certain [RTS options](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#runtime-system-rts-options) to its runtime system. This can be done in any Haskell application built with `-rtsopts` (see [above](#build-your-application-for-monitoring)). If you're looking for some options to copy, these are the minimal additions. However, I encourage you to read the remainder of this section, as this specific configuration may have a significant performance impact on your application.

```sh
./my-app +RTS -l -hT --eventlog-flush-interval=1 -RTS
```

Let's briefly discuss the relevant RTS options and why they are needed.

#### Required flags

The [`-l`](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag-l-flags) flag tells the RTS to write the eventlog in binary form.

The [`--eventlog-flush-interval=N`](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html#rts-flag-eventlog-flush-interval-seconds) tells the RTS to flush the eventlog buffers every `N` seconds.

> [!WARNING]
> Flushing the eventlog may have a significant performance impact, as each flush requires all threads in your application to synchronise. To mitigate this impact, choose a higher value for `N`.

> [!WARNING]
> Setting `--eventlog-flush-interval=N` without `-threaded` throws an error in GHC 9.14 and later, and causes eventlog corruption in GHC version 9.12 an earlier.
> See GHC issue [#26222](https://gitlab.haskell.org/ghc/ghc/-/issues/26222) for details.

#### Heap profiling

The [`-h`](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-options-for-heap-profiling) flag tells the RTS to enable heap profiling, which provides a detailed breakdown of memory usage. This is required for the `heap_prof_sample` processor and the corresponding detailed breakdown in the demo.
Both the `-hT` and `-hi` flags are supported:

- The `-hT` flag tells the RTS to use the "closure type" breakdown, which is the most well-supported option and should work for every application without any further configuration.
- The `-hi` flag tells the RTS to use the "info table" breakdown, which may provide more detailed information, but requires compiling your application and its dependencies with the GHC option [`-finfo-table-map`](https://downloads.haskell.org/ghc/latest/docs/users_guide/debug-info.html#ghc-flag-finfo-table-map).

The `-hm`, `-hd`, `-hy`, `-he` and `-hr` flags are untested, but should work in theory.

The `-hc` and `-hb` flags are unsupported.

> [!WARNING]
> Heap profiling has a significant performance impact, as each sample requires a major garbage collection. The default sampling interval is 0.1, but this can be adjusted with the [`-i`](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-flag-i-secs) flag.
>
> Alternatively, heap profiling can be enabled/disabled from within your application using the functions in [`GHC.Profiling`](https://hackage.haskell.org/package/base/docs/GHC-Profiling.html). If you plan to use these functions, you can pass [`--no-automatic-heap-samples`](https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html#rts-flag-no-automatic-heap-samples) to disable heap samples until you first call [`startHeapProfTimer`](https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-Profiling.html#v:startHeapProfTimer).

### Putting it all together

To visualise the profiling data of your instrumented application, you must connect it to the demo system.
The Docker Compose configuration in [`dockerfiles/eventlog-live-otelcol/docker-compose-external.yml`](dockerfiles/eventlog-live-otelcol/docker-compose-external.yml) sets up the same infrastructure used in the demo without the example application.

To use it, follow these steps:

1.  Start the containers with the OpenTelemetry Collector, Prometheus, and Grafana using the configuration files in this repository:

    ```sh
    docker compose -f dockerfiles/eventlog-live-otelcol/docker-compose-external.yaml up --build -d
    ```

2.  Set the `GHC_EVENTLOG_SOCKET` environment variable:

    ```sh
    export GHC_EVENTLOG_SOCKET="/tmp/my_eventlog.sock"
    ```

3.  Start your instrumented application:

    ```sh
    ./my-app +RTS -l -hT --eventlog-flush-interval=1
    ```

4.  Start `eventlog-live-otelcol`:

    ```sh
    eventlog-live-otelcol \
      --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
      -hT \
      --otelcol-host=localhost
    ```

### Fine-tuning `eventlog-live-otelcol`

The telemetry data produced by `eventlog-live-otelcol` can be configured in great detail via its configuration file in the YAML format. To print the [default configuration](eventlog-live-otelcol/data/default.yaml), as well as commentary explaining it, run the following command:

```sh
eventlog-live-otelcol --print-defaults
```

For validation and editor support, `eventlog-live-otelcol` ships with a JSON Schema for its configuration files. To print the [JSON Schema](eventlog-live-otelcol/data/config.schema.json), run the following command:

```sh
eventlog-live-otelcol --print-config-json-schema
```

If you use the RedHat YAML language server, you can instruct your editor to load this schema. See ["Associating a schema to a glob pattern via yaml.schemas"](https://github.com/redhat-developer/yaml-language-server?tab=readme-ov-file#associating-a-schema-to-a-glob-pattern-via-yamlschemas) in their README.

> [!NOTE]
> The configuration files are parsed using [`HsYAML`](https://hackage.haskell.org/package/HsYAML) which is a [YAML 1.2](https://yaml.org/spec/1.2/spec.html) compliant parser.
