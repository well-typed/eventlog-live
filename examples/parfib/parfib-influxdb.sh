#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/parfib_eventlog.sock"

# Build parfib
echo "Build parfib"
cabal build parfib -v0

# Build eventlog-influxdb
echo "Build eventlog-influxdb"
cabal build eventlog-influxdb -v0

# Install cleanup handler
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# Run parfib
echo "Start parfib"
PARFIB_BIN=$(cabal list-bin exe:parfib -v0 | head -n1)
"${PARFIB_BIN}" 11 45 46 47 48 49 50 +RTS -N -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
PARFIB_PID=$!

# Run eventlog-influxdb
# NOTE: The purpose of 'sleep 5' is to give the parfib process
#       sufficient time to create the Unix socket.
echo "Start eventlog-influxdb"
sleep 5 && cabal run eventlog-influxdb -v0 -- \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --influxdb-host=localhost \
    --influxdb-database=eventlog \
    --influxdb-username=admin \
    --influxdb-password=admin

# Wait for parfib to finish
wait $PARFIB_PID
