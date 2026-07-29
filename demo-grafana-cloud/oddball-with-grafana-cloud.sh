#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Configure the OTLP exporter from the .env file
if [ -f "${DIR}/.env" ]; then
	source "${DIR}/.env"
else
	echo "Error: Missing .env file in ${DIR}"
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

# Configure the eventlog socket
export GHC_EVENTLOG_WAIT="true"
export GHC_EVENTLOG_UNIX_PATH="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Build eventlog-live-otlp
echo "Build eventlog-live-otlp"
cabal build eventlog-live-otlp -v0
EVENTLOG_LIVE_OTLP_BIN=$(cabal list-bin exe:eventlog-live-otlp -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create a cleanup hook for background processes
trap 'kill $(jobs -p)' EXIT

# Start oddball
echo 'Start oddball'
${ODDBALL_BIN} \
	+RTS \
	-l \
	-hT \
	--eventlog-flush-interval=1 \
	-RTS \
	>/dev/null &

# Start eventlog-live-otlp
# Create the command to start eventlog-live-otlp
echo 'Start eventlog-live-otlp (for oddball)'
${EVENTLOG_LIVE_OTLP_BIN} \
	--verbosity=debug \
	--config="${DIR}/eventlog-live.yaml" \
	--service-name='oddball' \
	--eventlog-socket="${GHC_EVENTLOG_UNIX_PATH}" \
	-hT \
	--eventlog-flush-interval=1 \
	--otlp-protocol=http/protobuf \
	--otlp-endpoint="${OTEL_EXPORTER_OTLP_ENDPOINT}" \
	--otlp-http-headers="${OTEL_EXPORTER_OTLP_HEADERS}"
	>/dev/null
