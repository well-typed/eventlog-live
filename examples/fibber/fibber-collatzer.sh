#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/fibber_eventlog.sock"

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
FIBBER_BIN=$(cabal list-bin exe:fibber -v0 --project-file=cabal.project.profiling | head -n1)
"${FIBBER_BIN}" 44 +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
FIBBER_PID=$!
echo

# Run collatzer
# NOTE: The purpose of 'sleep 2' is to give the fibber process
#       sufficient time to create the Unix socket.
echo "Run collatzer"
sleep 2 && cabal run collatzer -v0 -- --unix "$GHC_EVENTLOG_SOCKET"

# Wait for fibber to finish
wait $FIBBER_PID
