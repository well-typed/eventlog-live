#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Build collatzer
echo "Build collatzer"
cabal build collatzer -v0
COLLATZER_BIN=$(cabal list-bin exe:collatzer -v0 | head -n1)

# Run oddball
echo "Start oddball"
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &

# Install cleanup handler
# shellcheck disable=SC2064
trap "trap - TERM && kill -- -$$" INT TERM EXIT

# Build oddball
echo "Build oddball"
cabal build oddball
echo

# Build collatzer
echo "Build collatzer"
cabal build collatzer
echo

# Install cleanup handler
trap 'trap - TERM && kill -- -$$' INT TERM

# Run collatzer
# NOTE: The purpose of 'sleep 2' is to give the oddball process
#       sufficient time to create the Unix socket.
echo "Run collatzer"
sleep 2 && "${COLLATZER_BIN}" --unix "$GHC_EVENTLOG_SOCKET"
