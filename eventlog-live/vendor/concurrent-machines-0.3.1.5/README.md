# concurrent-machines-0.3.1.5

This directory contains a vendored copy of version 0.3.1.5 of the concurrent
machines package, as the version published on Hackage does not currently build
with later version of GHC without adding `--allow-newer` constraints.

This vendored copy exports a subset of the full concurrent-machines API, so it
is important that development and testing is done against the vendored copy.
