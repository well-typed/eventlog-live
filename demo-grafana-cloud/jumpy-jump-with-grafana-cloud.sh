#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Configure the eventlog socket
export GHC_EVENTLOG_WAIT="true"
export GHC_EVENTLOG_UNIX_PATH="/tmp/jumpy-jump-eventlog.sock"

# Configure OpenTelemetry exporter
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="jumpy-jump"
export OTEL_RESOURCE_ATTRIBUTES="service.instance.id=$(uuidgen)"
export OTEL_EXPORTER_OTLP_PROTOCOL="http/protobuf"

# Configure the OTLP exporter endpoint from the .env file
if [ -f "${DIR}/.env" ]; then
# shellcheck disable=SC1091
	. "${DIR}/.env"
else
	echo "Error: ${DIR}/.env does not exist"
	exit 1
fi
if [ "${OTEL_EXPORTER_OTLP_ENDPOINT}" = "" ]; then
	echo "Error: The .env file did not defined OTEL_EXPORTER_OTLP_ENDPOINT."
	exit 1
fi
if [ "${OTEL_EXPORTER_OTLP_HEADERS}" = "" ]; then
	echo "Error: The .env file did not defined OTEL_EXPORTER_OTLP_HEADERS."
	exit 1
fi

# Find project file
PROJECT_FILE="$DIR/../cabal.profiling.project"

# Find build directory
BUILDDIR="$DIR/../dist-newstyle/jumpy-jump-with-cost-centre-profiler"

# Build jumpy-jump
echo "Build jumpy-jump"
cabal build jumpy-jump --project-file="${PROJECT_FILE}" --builddir="${BUILDDIR}" -f-use-ghc-stack-profiler --enable-profiling -v0
JUMPY_JUMP_BIN=$(cabal list-bin exe:jumpy-jump --project-file="${PROJECT_FILE}" --builddir="${BUILDDIR}" -f-use-ghc-stack-profiler --enable-profiling -v0 | head -n1)

# Build eventlog-live-otlp
echo "Build eventlog-live-otlp"
cabal build eventlog-live-otlp -v0
EVENTLOG_LIVE_OTLP_BIN=$(cabal list-bin exe:eventlog-live-otlp -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create a cleanup hook for background processes
trap 'kill $(jobs -p)' EXIT

# Start jumpy-jump
echo 'Start jumpy-jump'
${JUMPY_JUMP_BIN} \
	+RTS \
	-l \
	-p \
	-hT \
	--eventlog-flush-interval=1 \
	-RTS \
	>/dev/null &

# Start eventlog-live-otlp
# Create the command to start eventlog-live-otlp
echo 'Start eventlog-live-otlp (for jumpy-jump)'
${EVENTLOG_LIVE_OTLP_BIN} \
	--config="${DIR}/eventlog-live.yaml" \
	--eventlog-socket="${GHC_EVENTLOG_UNIX_PATH}" \
	-hT \
	--eventlog-flush-interval=1 \
	>/dev/null
