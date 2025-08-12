#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/spectral_norm_eventlog.sock"

# Build spectral-norm
echo "Build spectral-norm"
cabal build spectral-norm -v0

# Build eventlog-live-influxdb
echo "Build eventlog-live-influxdb"
cabal build eventlog-live-influxdb -v0

# Install cleanup handler
trap 'trap - TERM && kill -- -$$' INT TERM EXIT

# Run spectral-norm
echo "Start spectral-norm"
SPECTRAL_NORM_BIN=$(cabal list-bin exe:spectral-norm -v0 | head -n1)
"${SPECTRAL_NORM_BIN}" 15000 20000 25000 30000 15000 20000 25000 30000 15000 20000 25000 30000 +RTS -N -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
SPECTRAL_NORM_PID=$!

# Run eventlog-live-influxdb
# NOTE: The purpose of 'sleep 5' is to give the spectral-norm process
#       sufficient time to create the Unix socket.
echo "Start eventlog-live-influxdb"
sleep 5 && cabal run eventlog-live-influxdb -v0 -- \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --influxdb-host=localhost \
    --influxdb-database=eventlog \
    --influxdb-username=admin \
    --influxdb-password=admin

# Wait for spectral-norm to finish
wait $SPECTRAL_NORM_PID
