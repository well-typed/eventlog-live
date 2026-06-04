#!/bin/sh -e

# Usage: ./scripts/test.sh [CABAL_ARGS] -- [TEST_ARGS]
#
# NOTE: If environment variable DEBUG is defined, this script logs both the
#       output and error streams of the test suite to files and shows only
#       the error stream. Otherwise, it shows only the output stream and logs
#       only the error stream.

# Find the repository root directory
REPO_ROOT_DIR="$(CDPATH='' cd -- "$(dirname -- "$(dirname -- "$0")")" && pwd)"

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^cabal=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal:
#
# 1. Use CABAL if it is set.
# 2. Look for cabal-$EXPECTED_VERSION.
# 3. Look for cabal.
#
if [ "${CABAL}" = "" ]; then
	if ! CABAL="$(which "cabal-${EXPECT_VERSION}")"; then
		if ! CABAL="$(which "cabal")"; then
			echo "Requires cabal ${EXPECT_VERSION}; no version found"
			exit 1
		fi
	fi
fi

# Check cabal version:
ACTUAL_VERSION="$("${CABAL}" --numeric-version | head -n 1)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
	# Version mismatch is never an error:
	echo "Requires cabal ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
fi

# Log file for stderr.
ERR_FILE="${REPO_ROOT_DIR}/eventlog-live-tests.err.log"

# Run test command.
if [ -n "${DEBUG+x}" ]; then
	# Log file for stdout.
	OUT_FILE="${REPO_ROOT_DIR}/eventlog-live-tests.out.log"

	# Pipe for stderr.
	ERR_FIFO="${TMPDIR:-/tmp}/eventlog-live-tests.err.$$"
	mkfifo "${ERR_FIFO}"
	trap 'rm "${ERR_FIFO}"' EXIT
	tee "${ERR_FILE}" <"${ERR_FIFO}" >&2 &

	# Run test suite and log debug information.
	${CABAL} run eventlog-live-tests --enable-tests --constraint='eventlog-socket-tests+debug' "$@" >"${OUT_FILE}" 2>"${ERR_FIFO}"
else
	${CABAL} run eventlog-live-tests --enable-tests --constraint='eventlog-socket-tests+debug' "$@" 2>"${ERR_FILE}"
fi
