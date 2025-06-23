#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/fibber_eventlog.sock"
GHC_EVENTLOG="/tmp/fibber.eventlog"

# Build fibber
echo "Build fibber"
cabal build fibber
echo

# Build collatzer
echo "Build collatzer"
cabal build collatzer
echo

# Run fibber
echo "Run fibber"
cabal run fibber -v0 -- 44 &
FIBBER_PID=$!
echo

# Run collatzer
# NOTE: The purpose of 'sleep 5' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Run collatzer"
sleep 5 && cabal run collatzer -v0 -- --unix "$GHC_EVENTLOG_SOCKET"

# Wait for fibber to finish
wait $FIBBER_PID
