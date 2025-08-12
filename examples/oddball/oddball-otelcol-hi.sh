#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0

# Install cleanup handler
trap 'trap - TERM && kill -- -$$' INT TERM EXIT

# Run oddball
echo "Start oddball"
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)
"${ODDBALL_BIN}" +RTS -l -hi --eventlog-flush-interval=1 -RTS >/dev/null &
ODDBALL_PID=$!

# Run eventlog-live-otelcol
# NOTE: The purpose of 'sleep 5' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Start eventlog-live-otelcol"
sleep 5 && cabal run eventlog-live-otelcol -v0 -- \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hi \
    --otelcol-host=localhost

# Wait for oddball to finish
wait $ODDBALL_PID
