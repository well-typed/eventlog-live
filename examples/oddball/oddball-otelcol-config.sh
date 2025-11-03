#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0
EVENTLOG_LIVE_OTELCOL_BIN=$(cabal list-bin exe:eventlog-live-otelcol -v0 | head -n1)

# Run oddball
echo "Start oddball"
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &

# Install cleanup handler
# shellcheck disable=SC2064
trap "trap - TERM && kill -- -$$" INT TERM EXIT

# Run eventlog-live-otelcol
echo "Start eventlog-live-otelcol"
"${EVENTLOG_LIVE_OTELCOL_BIN}" \
	--verbosity=quiet \
	--stats \
	--config="$DIR/oddball-otelcol-config.yaml" \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --otelcol-host=localhost
