#!/bin/bash -

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Include helper functions
. "${DIR}/../../scripts/functions.sh"

# Set the eventlog socket
export GHC_EVENTLOG_WAIT="true"
export GHC_EVENTLOG_UNIX_PATH="/tmp/oddball_eventlog.sock"
export MY_GHC_EVENTLOG_UNIX_PATH="/tmp/eventlog_live_eventlog.sock"

# Configure OpenTelemetry exporter
export OTEL_LOG_LEVEL="debug"
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

# Build oddball
ODDBALL_BIN="$(cabal_list_bin oddball --constraint=eventlog-socket+control)"

# Build eventlog-live
EVENTLOG_LIVE_BIN=$(cabal_list_bin eventlog-live -f+use-ghc-debug-stub -f+use-eventlog-socket)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for oddball
ODDBALL_FIFO="$TMPDIR/oddball.fifo"
mkfifo "$ODDBALL_FIFO" || exit

# Create the screen pipe for eventlog-live (for oddball)
EVENTLOG_LIVE_FOR_ODDBALL_FIFO=$TMPDIR/eventlog-live-for-oddball.fifo
mkfifo "$EVENTLOG_LIVE_FOR_ODDBALL_FIFO" || exit

# Create the screen pipe for eventlog-live (for oddball)
EVENTLOG_LIVE_FOR_ITSELF_FIFO=$TMPDIR/eventlog-live-for-itself.fifo
mkfifo "$EVENTLOG_LIVE_FOR_ITSELF_FIFO" || exit

# Create the command to start oddball
# shellcheck disable=SC2089
ODDBALL_CMD="
echo 'Start oddball' && \
	OTEL_SERVICE_NAME='oddball' \
	${ODDBALL_BIN} \
		+RTS \
		-l \
		-hT \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live (for oddball)
# shellcheck disable=SC2089
EVENTLOG_LIVE_FOR_ODDBALL_CMD="
echo 'Start eventlog-live (for oddball)' && \
	OTEL_SERVICE_NAME='eventlog-live-for-oddball' \
	${EVENTLOG_LIVE_BIN} \
		--config='$DIR/config/eventlog-live.yaml' \
		--eventlog-flush-interval=1 \
	    --eventlog-socket='${GHC_EVENTLOG_UNIX_PATH}' \
		--my-eventlog-socket-unix='${MY_GHC_EVENTLOG_UNIX_PATH}' \
	    -hT \
		+RTS -l -hT --eventlog-flush-interval=1 -RTS
"

# Create the command to start eventlog-live (for itself)
# shellcheck disable=SC2089
EVENTLOG_LIVE_FOR_ITSELF_CMD="
echo 'Start eventlog-live (for itself)' && \
	OTEL_SERVICE_NAME='eventlog-live-for-eventlog-live' \
	${EVENTLOG_LIVE_BIN} \
	--config='$DIR/config/eventlog-live-for-eventlog-live.yaml' \
	--eventlog-flush-interval=1 \
    --eventlog-socket='${MY_GHC_EVENTLOG_UNIX_PATH}' \
    -hT
"

# Create the screen conf file
SCREEN_CONF="$TMPDIR/screen.conf"
cat > "$SCREEN_CONF" << 'EOF' || exit
split
split
split -v
focus right
screen -t 'oddball/stderr' sh -c 'tty > "$ODDBALL_FIFO"; read done < "$ODDBALL_FIFO"'
focus left
screen -t 'oddball/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$ODDBALL_FIFO"; eval "$ODDBALL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$ODDBALL_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-for-oddball/stderr' sh -c 'tty > "$EVENTLOG_LIVE_FOR_ODDBALL_FIFO"; read done < "$EVENTLOG_LIVE_FOR_ODDBALL_FIFO"'
focus left
screen -t 'eventlog-live-for-oddball/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_FOR_ODDBALL_FIFO"; eval "$EVENTLOG_LIVE_FOR_ODDBALL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_FOR_ODDBALL_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-for-itself/stderr' sh -c 'tty > "$EVENTLOG_LIVE_FOR_ITSELF_FIFO"; read done < "$EVENTLOG_LIVE_FOR_ITSELF_FIFO"'
focus left
screen -t 'eventlog-live-for-itself/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_FOR_ITSELF_FIFO"; eval "$EVENTLOG_LIVE_FOR_ITSELF_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_FOR_ITSELF_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	ODDBALL_FIFO \
	ODDBALL_CMD \
	EVENTLOG_LIVE_FOR_ODDBALL_FIFO \
	EVENTLOG_LIVE_FOR_ODDBALL_CMD \
	EVENTLOG_LIVE_FOR_ITSELF_FIFO \
	EVENTLOG_LIVE_FOR_ITSELF_CMD
screen -mc "$SCREEN_CONF"
