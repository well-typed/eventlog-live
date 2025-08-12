# `eventlog-live`

> :warning: This library is under active development. Any part of this repository may change without warning and the package versions do not follow the [Package Versioning Policy](https://pvp.haskell.org).

This repository contains a collection of libraries and tools for the live profiling of Haskell applications instrumented with [`eventlog-socket`](https://github.com/well-typed/ghc-eventlog-socket).

## Demo

The following is a screenshow of Grafana which shows live heap profiling statistics coming from the [`oddball`](examples/oddball) example program.

![A screenshot of Grafana showing live heap profiling statistics coming from the `oddball` example program.](assets/oddball-grafana.png)

To run this example for yourself, run the following command from the root of the repository, wait until all containers have started, then nagivate to Grafana at <localhost:3000>, log in using username `admin` and password `admin`, and open the heap profiling visualisation under ☰ > _Dashboards_ > _Browse_ then _General_ > _Heap Stats_.

```sh
docker compose -f dockerfiles/docker-compose-oddball-grafana.yml up --build
```

This [Docker Compose](https://docs.docker.com/compose/) configuration builds and runs a number of Docker containers:

- [`oddball`](dockerfiles/Dockerfile.oddball) for the example program.
- [`ekg-eventlog-influxdb`](dockerfiles/Dockerfile.ekg-eventlog-influxdb) for the program that forwards the eventlog profiling data from `oddball` to the InfluxDB database.
- [`influxdb`](https://github.com/influxdata/influxdata-docker/blob/063caa0d729da41b70760c8f7362345f1bb79779/influxdb/1.8/Dockerfile) for the InfluxDB database.
- [`grafana`](https://hub.docker.com/r/grafana/grafana/) for the Grafana instance.

## Getting Started

To use the code in this repository to profile your own application, follow these steps.

### Add `eventlog-socket` as a dependency

The `eventlog-socket` package is not yet published on Hackage, so you must add it to your `cabal.project` file as a source repository package:

```cabal
source-repository-package
  type: git
  location: https://github.com/well-typed/ghc-eventlog-socket
  tag: 1acb92ff60f4bbc87815466f904366ea5078ed9a
```

Then add `eventlog-socket` to the `build-depends` for your application:

```cabal
executable my-app
  ...
  build-depends:
    ...
    eventlog-socket,
    ...
```

### Instrument your application

To instrument your application, and allow the eventlog data to be streamed over a socket, all you have to do is call `GHC.Eventlog.Socket.start` with the path to your eventlog socket.

```haskell
module Main where

import qualified GHC.Eventlog.Socket

main :: IO ()
main = do
  putStrLn "Creating eventlog socket..."
  GHC.Eventlog.Socket.start "/tmp/ghc-eventlog.sock"
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

The program `ekg-eventlog-influxdb` requires that your eventlog is written in binary form, which requires the `-l` RTS option.
The heap statistics visualisation in the demo requires that your application is run with heap profiling enabled, which requires the `-hT` RTS option.
Finally, to ensure that the RTS flushes the events queue consistently and every second, you should set the `--eventlog-flush-interval=1` and `--no-automatic-heap-samples` RTS options.
You can pass these options at runtime with:

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

> :warning: This does not work on non-Linux hosts.
> See [Profiling on Mac](#profiling-on-mac) below for a discussion of workarounds on Mac.

To visualise the profiling data of your instrumented application, you must connect it to the demo system.
The Docker Compose configuration in [`docker-compose-grafana.yml`](dockerfiles/docker-compose-grafana.yml) sets up the same infrastructure used in the demo without the example program.
To use it, follow these steps:

1.  Start your instrumented application:

    ```sh
    ./my-app +RTS -l -hT --eventlog-flush-interval=1 --no-automatic-heap-samples
    ```

2.  Start the Grafana container:

    ```sh
    docker compose -f dockerfiles/docker-compose-grafana.yml up --build
    ```

### Profiling on Mac

Profiling an application running on the host plaform using the infrastructure in the Docker container requires mounting the eventlog locked into a Docker container. This does not, and likely will never, work on non-Linux hosts.
See Docker for Mac [issue 483](https://github.com/docker/for-mac/issues/483) for a discussion of why this will not work on Mac.

If you are using a Mac, there are three possible workarounds.

1. Containerize your application, as is done in [`docker-compose-oddball-grafana.yml`](dockerfiles/docker-compose-oddball-grafana.yml).
2. Run all services locally.
3. Use [`socat`](http://www.dest-unreach.org/socat/) locally to forward the eventlog traffic to a TCP socket and use `socat` within a container to forward the eventlog traffic back to a Unix domain socket.
