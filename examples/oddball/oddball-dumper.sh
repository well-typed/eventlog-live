#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0

# Build dumper
echo "Build dumper"
cabal build dumper -v0

# Install cleanup handler
trap 'trap - TERM && kill -- -$$' INT TERM EXIT

# Run oddball
echo "Start oddball"
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
ODDBALL_PID=$!

# Run dumper
echo "Start dumper"
cabal run dumper -v0 -- --eventlog-socket="$GHC_EVENTLOG_SOCKET" "$@"

# Wait for oddball to finish
wait $ODDBALL_PID
