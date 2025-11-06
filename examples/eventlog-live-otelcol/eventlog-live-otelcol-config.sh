#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball__ghc_eventlog.sock"
export MY_GHC_EVENTLOG_SOCKET="/tmp/eventlog_live_otelcol__ghc_eventlog.sock"

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

# Run eventlog-live-otelcol (for oddball)
echo "Start eventlog-live-otelcol (for oddball)"
"${EVENTLOG_LIVE_OTELCOL_BIN}" \
	--verbosity=quiet \
	--stats \
	--config="$DIR/config/oddball.yaml" \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
	--my-eventlog-socket "$MY_GHC_EVENTLOG_SOCKET" \
	--my-ghc-debug \
    -hT \
    --otelcol-host=localhost \
	+RTS -l -hT --eventlog-flush-interval=1 -RTS &

# Install cleanup handler
# shellcheck disable=SC2064
trap "trap - TERM && kill -- -$$" INT TERM EXIT

# Run eventlog-live-otelcol (for itself)
echo "Start eventlog-live-otelcol (for itself)"
"${EVENTLOG_LIVE_OTELCOL_BIN}" \
	--verbosity=quiet \
	--stats \
	--config="$DIR/config/eventlog-live-otelcol.yaml" \
    --eventlog-socket "$MY_GHC_EVENTLOG_SOCKET" \
    -hT \
    --otelcol-host=localhost \
	 >/dev/null
