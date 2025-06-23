#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball
echo

# Build collatzer
echo "Build collatzer"
cabal build collatzer
echo

# Run oddball
echo "Run oddball"
cabal run oddball -v0 >/dev/null &
ODDBALL_PID=$!
echo

# Run collatzer
# NOTE: The purpose of 'sleep 5' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Run collatzer"
sleep 5 && cabal run collatzer -v0 -- --unix "$GHC_EVENTLOG_SOCKET"

# Wait for oddball to finish
wait $ODDBALL_PID
