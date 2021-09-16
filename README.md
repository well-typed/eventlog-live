# eventlog-live

To setup the build copy `cabal.project.local.sample` to `cabal.project.local`
and adjust according to your local preferences.

## Demo

Run server

```
cabal run collatzer-server -- --eventlog-socket /tmp/collatzer.eventlog.sock +RTS -N4 -l --eventlog-flush-interval=1 -RTS
cabal run collatzer-server -- --eventlog-socket /tmp/collatzer.eventlog.sock +RTS -N4 -l --eventlog-flush-interval=1 --null-eventlog-writer -RTS
```

Start monitor

```
cabal run eventlog-recv -- /tmp/collatzer.eventlog.sock
```

Make some load

```
cabal run collatzer-client -- --quit-after 2 --threads 5
```
