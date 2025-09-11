#!/bin/sh -e

# Find ghc-events:
if ! which -s "ghc-events"; then
    echo "Requires ghc-events; no version found"
    echo "To install, run:"
    echo
    echo "  cabal install ghc-events"
    echo
    exit 1
fi

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/fibber_eventlog.sock"
GHC_EVENTLOG="/tmp/fibber.eventlog"

# Build fibber
echo "Build fibber"
cabal build fibber
echo

# Run fibber
echo "Run fibber"
FIBBER_BIN=$(cabal list-bin exe:fibber -v0 --project-file=cabal.project.profiling | head -n1)
"${FIBBER_BIN}" 44 +RTS -l -hT --eventlog-flush-interval=1 -RTS >/dev/null &
FIBBER_PID=$!
echo

# Run NetCat
# NOTE: The purpose of 'sleep 5' is to give the fibber process
#       sufficient time to create the Unix socket.
echo "Run netcat"
sleep 5 && nc -U "$GHC_EVENTLOG_SOCKET" >"$GHC_EVENTLOG"

# Wait for fibber to finish
wait $FIBBER_PID

# Run ghc-events show
echo "Run ghc-events show"
ghc-events show "$GHC_EVENTLOG"
