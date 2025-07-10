#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball
echo

# Build dumper
echo "Build dumper"
cabal build dumper
echo

# Run oddball
echo "Run oddball"
cabal run oddball -v0 >/dev/null -- +RTS -l -hT -RTS &
ODDBALL_PID=$!
echo

# Run dumper
# NOTE: The purpose of 'sleep 5' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Run dumper"
sleep 5 && cabal run dumper -v0 -- --unix "$GHC_EVENTLOG_SOCKET" "$@"

# Wait for oddball to finish
wait $ODDBALL_PID
