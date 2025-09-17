#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0 --project-file=cabal.project.profiling

# Build eventlog-live-influxdb
echo "Build eventlog-live-influxdb"
cabal build eventlog-live-influxdb -v0

# Install cleanup handler
trap 'trap - TERM && kill -- -$$' INT TERM

# Run oddball
echo "Start oddball"
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 --project-file=cabal.project.profiling | head -n1)
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
ODDBALL_PID=$!

# Run eventlog-live-influxdb
# NOTE: The purpose of 'sleep 5' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Start eventlog-live-influxdb"
cabal run eventlog-live-influxdb -v0 -- \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --influxdb-host=localhost \
    --influxdb-database=eventlog \
    --influxdb-username=admin \
    --influxdb-password=admin

# Wait for oddball to finish
wait $ODDBALL_PID
