#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Configure OpenTelemetry exporter
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="oddball"
export OTEL_RESOURCE_ATTRIBUTES="service.instance.id=$(uuidgen)"
export OTEL_LOGS_EXPORTER="console"
export OTEL_METRICS_EXPORTER="console"
export OTEL_PROFILES_EXPORTER="console"
export OTEL_TRACES_EXPORTER="console"

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

# Create the eventlog pipe
ODDBALL_EVENTLOG_FIFO="$TMPDIR/oddball-eventlog.fifo"
mkfifo "${ODDBALL_EVENTLOG_FIFO}" || exit

# Create the screen pipe for oddball
ODDBALL_SCREEN_FIFO="$TMPDIR/oddball-screen.fifo"
mkfifo "$ODDBALL_SCREEN_FIFO" || exit

# Create the screen pipe for eventlog-live-otlp
EVENTLOG_LIVE_OTLP_SCREEN_FIFO=$TMPDIR/eventlog-live-otlp-screen.fifo
mkfifo "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO" || exit

# Create the command to start oddball
# shellcheck disable=SC2089
ODDBALL_CMD="
echo 'Start oddball' && \
	${ODDBALL_BIN} \
		+RTS \
		-l \
		-ol'${ODDBALL_EVENTLOG_FIFO}' \
		-hT \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live-otlp
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTLP_CMD="
echo 'Start eventlog-live-otlp (for oddball)' && \
	${EVENTLOG_LIVE_OTLP_BIN} \
		--config='$DIR/eventlog-live.yaml' \
		--eventlog-file='${ODDBALL_EVENTLOG_FIFO}' \
	    -hT \
		--eventlog-flush-interval=1 \
"

# Create the screen conf file
SCREEN_CONF="$TMPDIR/screen.conf"
cat > "$SCREEN_CONF" << 'EOF' || exit
split
split -v
focus right
screen -t 'oddball/stderr' sh -c 'tty > "$ODDBALL_SCREEN_FIFO"; read done < "$ODDBALL_SCREEN_FIFO"'
focus left
screen -t 'oddball/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$ODDBALL_SCREEN_FIFO"; eval "$ODDBALL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$ODDBALL_SCREEN_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otlp/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO"; read done < "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO"'
focus left
screen -t 'eventlog-live-otlp/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO"; eval "$EVENTLOG_LIVE_OTLP_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTLP_FOR_ODDBALL_SCREEN_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	ODDBALL_SCREEN_FIFO \
	ODDBALL_CMD \
	EVENTLOG_LIVE_OTLP_SCREEN_FIFO \
	EVENTLOG_LIVE_OTLP_CMD
screen -mc "$SCREEN_CONF"
