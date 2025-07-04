#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0 -w${HOME}/Projects/ghc/_build/stage1/bin/ghc

# Build eventlog-influxdb
echo "Build eventlog-influxdb"
cabal build eventlog-influxdb -v0

# Install cleanup handler
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# Run oddball
echo "Start oddball"
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 -w${HOME}/Projects/ghc/_build/stage1/bin/ghc | head -n1)
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
ODDBALL_PID=$!

# Run eventlog-influxdb
# NOTE: The purpose of 'sleep 5' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Start eventlog-influxdb"
sleep 5 && cabal run eventlog-influxdb -v0 -- \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --influxdb-host=localhost \
    --influxdb-database=eventlog \
    --influxdb-username=admin \
    --influxdb-password=admin

# Wait for oddball to finish
wait $ODDBALL_PID
