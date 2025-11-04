#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/spectral_norm_eventlog.sock"

# Build spectral-norm
echo "Build spectral-norm"
cabal build spectral-norm -v0
SPECTRAL_NORM_BIN=$(cabal list-bin exe:spectral-norm -v0 | head -n1)

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0
EVENTLOG_LIVE_OTELCOL_BIN=$(cabal list-bin exe:eventlog-live-otelcol -v0 | head -n1)

# Run spectral-norm
echo "Start spectral-norm"
"${SPECTRAL_NORM_BIN}" 15000 20000 25000 30000 15000 20000 25000 30000 15000 20000 25000 30000 +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &

# Install cleanup handler
# shellcheck disable=SC2064
trap "trap - TERM && kill -- -$$" INT TERM EXIT

# Run eventlog-live-otelcol
echo "Start eventlog-live-otelcol"
"${EVENTLOG_LIVE_OTELCOL_BIN}" \
	--verbosity=quiet \
	--stats \
	--config="$DIR/spectral-norm-otelcol-config.yaml" \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --otelcol-host=localhost
