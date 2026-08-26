#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Eventlog Socket Configuration
export GHC_EVENTLOG_UNIX_PATH="/tmp/oddball_eventlog.sock"
export GHC_EVENTLOG_WAIT="true"

# Configure OpenTelemetry exporter
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="oddball"
export OTEL_RESOURCE_ATTRIBUTES="service.instance.id=$(uuidgen)"
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

# Find GHC version to build oddball
if [ "$GHC" = "" ]; then
	GHC="$(which ghc)"
fi
PROJECT_FILE="$DIR/../../cabal.ipe.project"

# Build oddball
echo "Build oddball"
cabal build oddball --with-compiler="$GHC" --project-file="${PROJECT_FILE}" --constraint=eventlog-socket+control -v0
ODDBALL_BIN=$(cabal list-bin exe:oddball --with-compiler="$GHC" --project-file="${PROJECT_FILE}" --constraint=eventlog-socket+control -v0 | head -n1)

# Build eventlog-live-otlp
echo "Build eventlog-live-otlp"
cabal build eventlog-live-otlp -f+control -v0
EVENTLOG_LIVE_OTLP_BIN=$(cabal list-bin exe:eventlog-live-otlp -f+control -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for oddball
ODDBALL_FIFO="$TMPDIR/oddball.fifo"
mkfifo "$ODDBALL_FIFO" || exit

# Create the screen pipe for eventlog-live-otlp
EVENTLOG_LIVE_OTLP_FIFO=$TMPDIR/eventlog-live-otlp.fifo
mkfifo "$EVENTLOG_LIVE_OTLP_FIFO" || exit

# Create the command to start oddball
# shellcheck disable=SC2089
ODDBALL_CMD="
echo 'Start oddball' && \
	${ODDBALL_BIN} \
		+RTS \
		-l \
		-hi \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live-otlp
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTLP_CMD="
echo 'Start eventlog-live-otlp (for oddball)' && \
	${EVENTLOG_LIVE_OTLP_BIN} \
		--stats \
		--config='$DIR/eventlog-live.yaml' \
	    --eventlog-socket '$GHC_EVENTLOG_UNIX_PATH' \
	    -hi \
		--eventlog-flush-interval=1 \
		--control \
		--control-port 30719 \
		--control-cors-ignore-failure
"

# Create the screen conf file
SCREEN_CONF="$TMPDIR/screen.conf"
cat > "$SCREEN_CONF" << 'EOF' || exit
split
split -v
focus right
screen -t 'oddball/stderr' sh -c 'tty > "$ODDBALL_FIFO"; read done < "$ODDBALL_FIFO"'
focus left
screen -t 'oddball/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$ODDBALL_FIFO"; eval "$ODDBALL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$ODDBALL_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otlp/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTLP_FIFO"; read done < "$EVENTLOG_LIVE_OTLP_FIFO"'
focus left
screen -t 'eventlog-live-otlp/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTLP_FIFO"; eval "$EVENTLOG_LIVE_OTLP_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTLP_FOR_ODDBALL_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	ODDBALL_FIFO \
	ODDBALL_CMD \
	EVENTLOG_LIVE_OTLP_FIFO \
	EVENTLOG_LIVE_OTLP_CMD
screen -mc "$SCREEN_CONF"
