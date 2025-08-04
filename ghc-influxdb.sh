#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/spectral_norm_eventlog.sock"

# Install cleanup handler
trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

# Run spectral-norm
echo "Start ghc"
GHC_CMD=./build-cabal.sh
export GHC=/home/matt/ghc-wen/_build/stage1/bin/ghc
bash "$GHC_CMD" +RTS -N -l -hT --eventlog-flush-interval=1 -RTS &
GHC_PID=$!


# Run eventlog-influxdb
# NOTE: The purpose of 'sleep 5' is to give the spectral-norm process
#       sufficient time to create the Unix socket.
echo "Start eventlog-influxdb"
cabal run eventlog-influxdb -v1 -- \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --influxdb-host=localhost \
    --influxdb-database=eventlog \
    --influxdb-username=admin \
    --influxdb-password=admin

# Wait for spectral-norm to finish
wait $GHC_PID
