#!/bin/sh -e

# Find otel-tui:
#
# 1. Use OTEL_TUI if it is set.
# 2. Look for otel-tui.
#
if [ "${OTEL_TUI}" = "" ]; then
	if ! OTEL_TUI="$(which "otel-tui")"; then
		echo "Requires otel-tui; no version found"
		exit 1
	fi
fi

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

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
trap "rm -rf '${TMPDIR}'" EXIT INT TERM HUP

# Create the eventlog pipe
ODDBALL_EVENTLOG_FIFO="${TMPDIR}/oddball-eventlog.fifo"
mkfifo "${ODDBALL_EVENTLOG_FIFO}" || exit

# Create the command to start oddball
# shellcheck disable=SC2089
ODDBALL_CMD="
trap 'screen -X quit' EXIT INT &&   \
  echo 'Start oddball' &&           \
    ${ODDBALL_BIN}                  \
      +RTS                          \
      -l                            \
      -ol${ODDBALL_EVENTLOG_FIFO}   \
      -hT                           \
      --eventlog-flush-interval=1   \
      -RTS
"

# Create the command to start eventlog-live-otlp
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTLP_CMD="
trap 'screen -X quit' EXIT INT &&                        \
  echo 'Start eventlog-live-otlp (for oddball)' &&       \
    OTEL_LOG_LEVEL='debug'                               \
    OTEL_SERVICE_NAME='oddball'                          \
    OTEL_EXPORTER_OTLP_PROTOCOL='http/protobuf'          \
    OTEL_EXPORTER_OTLP_ENDPOINT='http://localhost:54318' \
    ${EVENTLOG_LIVE_OTLP_BIN}                            \
      --config='${DIR}/eventlog-live.yaml'               \
      --eventlog-file='${ODDBALL_EVENTLOG_FIFO}'         \
      -hT                                                \
      --eventlog-flush-interval=1
"

# Create the command to start otel-tui
# shellcheck disable=SC2089
OTEL_TUI_CMD="
trap 'screen -X quit' EXIT INT &&     \
  ${OTEL_TUI}                         \
  	--host localhost                  \
    --http 54318                      \
	--debug-log '${DIR}/otel-tui.log' \
    --disable-internal-metrics
"

# Create the screen conf file
SCREEN_CONF="${TMPDIR}/screen.conf"
cat >"${SCREEN_CONF}" <<'EOF' || exit
split -v
focus right
split
focus up
resize -h 20%
focus down
resize -h 20%
focus left
screen -t 'otel-tui' sh -c 'eval "${OTEL_TUI_CMD}"'
focus right
focus up
screen -t 'oddball' sh -c 'eval "${ODDBALL_CMD}"'
focus down
screen -t 'eventlog-live-otlp' sh -c 'eval "${EVENTLOG_LIVE_OTLP_CMD}"'
focus left
EOF

# Start screen
export \
	ODDBALL_CMD \
	EVENTLOG_LIVE_OTLP_CMD \
	OTEL_TUI_CMD
screen -mc "$SCREEN_CONF"
