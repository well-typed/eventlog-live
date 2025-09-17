#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/fibber_eventlog.sock"

# Build fibber
echo "Build fibber"
cabal build fibber -v0

# Build dumper
echo "Build dumper"
cabal build dumper -v0

# Install cleanup handler
trap 'trap - TERM && kill -- -$$' INT TERM

# Run fibber
echo "Start fibber"
FIBBER_BIN=$(cabal list-bin exe:fibber -v0 | head -n1)
"${FIBBER_BIN}" 44 +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
FIBBER_PID=$!

# Run dumper
echo "Start dumper"
cabal run dumper -v0 -- --eventlog-socket="$GHC_EVENTLOG_SOCKET" "$@"

# Wait for fibber to finish
wait $FIBBER_PID
