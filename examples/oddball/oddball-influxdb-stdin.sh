#!/bin/bash -e

# Build oddball
echo "Build oddball"
cabal build oddball -v0

# Build eventlog-live-influxdb
echo "Build eventlog-live-influxdb"
cabal build eventlog-live-influxdb -v0

# Get path to oddball binary
echo "Get path to oddball binary"
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Get path to eventlog-live-influxdb binary
echo "Get path to eventlog-live-influxdb binary"
EVENTLOG_LIVE_INFLUXDB_BIN=$(cabal list-bin exe:eventlog-live-influxdb -v0 | head -n1)

# Create the FIFO pipe
echo "Create FIFO pipe"
GHC_EVENTLOG_FIFO="ghc-eventlog.fifo"
trap 'rm -f "${GHC_EVENTLOG_FIFO}"' INT TERM EXIT
mkfifo "${GHC_EVENTLOG_FIFO}"

# Start oddball
echo "Start oddball"
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -ol"${GHC_EVENTLOG_FIFO}" -RTS &
ODDBALL_PID=$!
trap 'trap - TERM && kill -- -$$' INT TERM

# Start eventlog-live-influxdb
echo "Start eventlog-live-influxdb"
"${EVENTLOG_LIVE_INFLUXDB_BIN}" --eventlog-stdin -hT --influxdb-host=localhost --influxdb-database=eventlog --influxdb-username=admin --influxdb-password=admin <"${GHC_EVENTLOG_FIFO}" &

# Wait for oddball to finish
wait $ODDBALL_PID