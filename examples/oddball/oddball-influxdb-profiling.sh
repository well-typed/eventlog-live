#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Build eventlog-live-influxdb
echo "Build eventlog-live-influxdb"
cabal build eventlog-live-influxdb -v0
EVENTLOG_LIVE_INFLUXDB_BIN=$(cabal list-bin exe:eventlog-live-influxdb -v0 | head -n1)

# Run oddball
echo "Start oddball"
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &

# Install cleanup handler
# shellcheck disable=SC2064
trap "trap - TERM && kill -- -$$" INT TERM EXIT

# Run eventlog-live-influxdb
echo "Start eventlog-live-influxdb"
"${EVENTLOG_LIVE_INFLUXDB_BIN}" \
    --eventlog-socket "$GHC_EVENTLOG_SOCKET" \
    -hT \
    --influxdb-host=localhost \
    --influxdb-database=eventlog \
    --influxdb-username=admin \
    --influxdb-password=admin
