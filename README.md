# `eventlog-live`

> :warning:
> This package is experimental.
> It is versioned according to the [PVP](https://pvp.haskell.org).
> However, breaking changes should be expected and no effort will be
> made to avoid major version bumps until at least version @1.0.0.0@.

This repository contains a collection of libraries and tools for the live profiling of Haskell applications instrumented with [`eventlog-socket`](https://github.com/well-typed/ghc-eventlog-socket).

## Demo

The following is a screenshow of Grafana which shows live heap profiling statistics coming from the [`oddball`](examples/oddball) example program.

![A screenshot of Grafana showing live heap profiling statistics coming from the `oddball` example program.](eventlog-live/assets/eventlog-live-otelcol.png)

To run this example for yourself, run the following command from the root of the repository, wait until all containers have started, then navigate to Grafana at <localhost:3000>, log in using username `admin` and password `admin`, and open the heap profiling visualisation under ☰ > _Dashboards_ > _Browse_ then _General_ > _Eventlog Heap_.

```sh
docker compose -f dockerfiles/eventlog-live-otelcol/docker-compose.yml up --build
```

This [Docker Compose](https://docs.docker.com/compose/) configuration builds and runs a number of Docker containers:

- [`oddball`](dockerfiles/Dockerfile.oddball)

  The `oddball` example program, which repeatedly generates and sums large quantities of random numbers.

- [`eventlog-live-otelcol`](dockerfiles/Dockerfile.eventlog-live-otelcol)

  The `eventlog-live-otelcol` program, which streams eventlog data from `oddball` to the
  OpenTelemetry Collector.

- [`otel/opentelemetry-collector-contrib`](https://hub.docker.com/r/otel/opentelemetry-collector-contrib)

  The OpenTelemetry Collector, which streams the data to Prometheus.

- [`prom/prometheus`](https://hub.docker.com/r/prom/prometheus)

  The Prometheus metric processor and database, which acts as a datasource for Grafana.

- [`grafana/grafana-oss`](https://hub.docker.com/r/grafana/grafana-oss)

  The Grafana instance, which visualises the eventlog data.

## Getting Started

To use the code in this repository to profile your own application, follow these steps.

### Add `eventlog-socket` as a dependency

The `eventlog-socket` package is not yet published on Hackage, so you must add it to your `cabal.project` file as a source repository package:

```cabal
source-repository-package
  type:     git
  location: https://github.com/well-typed/eventlog-socket
  tag:      1acb92ff60f4bbc87815466f904366ea5078ed9a
```

Then add `eventlog-socket` to the `build-depends` for your application:

```cabal
executable my-app
  ...
  build-depends:
    ...
    , eventlog-socket  >=0.1.0 && <0.2
    ...
```

### Instrument your application

To instrument your application, and allow the eventlog data to be streamed over a socket, all you have to do is call `GHC.Eventlog.Socket.start` with the path to your eventlog socket.

```haskell
module Main where

import           Data.Maybe (fromMaybe)
import qualified GHC.Eventlog.Socket
import           System.Environment (lookupEnv)

main :: IO ()
main = do
  putStrLn "Creating eventlog socket..."
  eventlogSocket <-
      fromMaybe "/tmp/ghc_eventlog.sock"
          <$> lookupEnv "GHC_EVENTLOG_SOCKET"
  GHC.Eventlog.Socket.start eventlogSocket
  ...
```

If you wish for your application to block until the client process connects to the eventlog socket, you can call `GHC.Eventlog.Socket.startWait`.

### Build your application with profiling

To enable runtime configuration of various eventlog options, you must compile your application with the `-rtsopts` GHC option.

```cabal
executable my-app
  ...
  ghc-options: -rtsopts
  ...
```

Since GHC 9.4, eventlog profiling is enabled by default.
However, if you are using GHC 9.2 or earlier, you must also enable eventlog profiling at compile time by passing the `-eventlog` GHC option.

```cabal
executable my-app
  ...
  if impl(ghc < 9.4)
    ghc-options: -eventlog
  ...
```

To ensure that the RTS flushes the events queue consistently and every second, you can pass the `--eventlog-flush-interval=1` and `--no-automatic-heap-samples` RTS options at runtime.
If you do, you must compile your application with `-threaded`.
See GHC issue [#26222](https://gitlab.haskell.org/ghc/ghc/-/issues/26222) for further details.

```cabal
executable my-app
  ...
  ghc-options: -threaded
  ...
```

The program `eventlog-live-influxdb` requires that your eventlog is written in binary form, which requires the `-l` RTS option. The heap statistics visualisation in the demo requires that your application is run with heap profiling enabled, which requires the `-hT` RTS option. You can pass these options at runtime with:

```sh
./my-app +RTS -l -hT --eventlog-flush-interval=1 --no-automatic-heap-samples
```

Alternatively, you can set these options at compile time with:

```cabal
executable my-app
  ...
  ghc-options: -rtsopts "-with-rtsopts=-l -hT --eventlog-flush-interval=1 --no-automatic-heap-samples"
  ...
```

### Putting it all together

To visualise the profiling data of your instrumented application, you must connect it to the demo system.
The Docker Compose configuration in [`dockerfiles/eventlog-live-otelcol/docker-compose-external.yml`](dockerfiles/eventlog-live-otelcol/docker-compose-external.yml) sets up the same infrastructure used in the demo without the example program.
To use it, follow these steps:

1.  Start the containers with the OpenTelemetry Collector, Prometheus, and Grafana:

    ```sh
    docker compose -f dockerfiles/eventlog-live-otelcol/docker-compose-external.yaml up --build -d
    ```

2.  Set the `GHC_EVENTLOG_SOCKET` environment variable:

    ```sh
    export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"
    ```

3.  Start your instrumented application:

    ```sh
    ./my-app +RTS -l -hT --eventlog-flush-interval=1 --no-automatic-heap-samples
    ```

4.  Start `eventlog-live-otelcol`:

    ```sh
    eventlog-live-otelcol \
      --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
      -hT \
      --otelcol-host=localhost
    ```
