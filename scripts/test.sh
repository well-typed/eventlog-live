#!/bin/sh -e

# Usage: ./scripts/test.sh [CABAL_ARGS] -- [TEST_ARGS]
#
# NOTE: If environment variable DEBUG is defined, this script logs both the
#       output and error streams of the test suite to files and shows only
#       the error stream. Otherwise, it shows only the output stream and logs
#       only the error stream.

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Include helper functions
. "${DIR}/functions.sh"

# Build eventlog-live-otlp
EVENTLOG_LIVE_OTLP_BIN=$(cabal_build eventlog-live:exe:eventlog-live-otlp --enable-tests)
EVENTLOG_LIVE_OTLP_DIR=$(dirname "${EVENTLOG_LIVE_OTLP_BIN}")

# Build eventlog-live-tests
EVENTLOG_LIVE_TESTS_BIN=$(cabal_build eventlog-live-tests:test:eventlog-live-tests --enable-tests --constraint=eventlog-socket-tests+debug)

# Log file for stderr.
ERR_FILE="${DIR}/../eventlog-live-tests.err.log"

# Run test command.
if [ -n "${DEBUG+x}" ]; then
	# Log file for stdout.
	OUT_FILE="${DIR}/../eventlog-live-tests.out.log"

	# Pipe for stderr.
	ERR_FIFO="${TMPDIR:-/tmp}/eventlog-live-tests.err.$$"
	mkfifo "${ERR_FIFO}"
	trap 'rm "${ERR_FIFO}"' EXIT
	tee "${ERR_FILE}" <"${ERR_FIFO}" >&2 &

	# Run test suite and log debug information.
	PATH="${EVENTLOG_LIVE_OTLP_DIR}:${PATH}" ${EVENTLOG_LIVE_TESTS_BIN} -- "$@" >"${OUT_FILE}" 2>"${ERR_FIFO}"
else
	PATH="${EVENTLOG_LIVE_OTLP_DIR}:${PATH}" ${EVENTLOG_LIVE_TESTS_BIN} -- "$@" 2>"${ERR_FILE}"
fi
