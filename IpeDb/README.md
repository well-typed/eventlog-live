# IpeDb

Example:

```bash
# assumes a GHC HEAD binary is on PATH and named `ghc-9.15`
$ cabal run -w ghc-9.15 exe:ecosim -- +RTS -l
# run ctrl+c after the first output
$ cabal run exe:ipedb -- ecosim.sqlite index -f ecosim.eventlog
$ mv ecosim.sqlite ./examples/ecosim/
$ ./examples/ecosim/ecosim-otelcol-config-with-ipedb.sh
```
