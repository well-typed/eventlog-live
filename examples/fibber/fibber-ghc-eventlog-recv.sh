#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/fibber_eventlog.sock"
GHC_EVENTLOG="/tmp/fibber.eventlog"

# Build fibber
echo "Build fibber"
cabal build fibber
echo

# Build eventlog-recv
echo "Build eventlog-recv"
cabal build eventlog-recv
echo

# Run fibber
echo "Run fibber"
cabal run fibber -v0 -- 44 &
FIBBER_PID=$!
echo

# Run eventlog-recv
# NOTE: The purpose of 'sleep 5' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Run eventlog-recv"
sleep 5 && cabal run eventlog-recv -v0 -- --unix "$GHC_EVENTLOG_SOCKET"

# Wait for fibber to finish
wait $FIBBER_PID
