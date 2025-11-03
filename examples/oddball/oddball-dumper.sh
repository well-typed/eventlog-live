#!/bin/sh -e

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Build dumper
echo "Build dumper"
cabal build dumper -v0
DUMPER_BIN=$(cabal list-bin exe:dumper -v0 | head -n1)

# Run oddball
echo "Start oddball"
"${ODDBALL_BIN}" +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &

# Install cleanup handler
# shellcheck disable=SC2064
trap "trap - TERM && kill -- -$$" INT TERM EXIT

# Run dumper
echo "Start dumper"
"${DUMPER_BIN}" --eventlog-socket="$GHC_EVENTLOG_SOCKET" "$@"
