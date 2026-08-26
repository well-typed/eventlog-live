#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Find project file
PROJECT_FILE="$DIR/../../cabal.profiling.project"

# Configure OpenTelemetry exporter
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="jumpy-jump"
export OTEL_RESOURCE_ATTRIBUTES="service.instance.id=$(uuidgen)"
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

# Build jumpy-jump
echo "Build jumpy-jump"
cabal build jumpy-jump --project-file="${PROJECT_FILE}" --builddir=dist-newstyle/jumpy-jump-with-cost-centre-profiler -f-use-ghc-stack-profiler --enable-profiling -v0
JUMPY_JUMP_BIN=$(cabal list-bin exe:jumpy-jump --project-file="${PROJECT_FILE}" --builddir=dist-newstyle/jumpy-jump-with-cost-centre-profiler -f-use-ghc-stack-profiler --enable-profiling -v0 | head -n1)

# Build eventlog-live-otlp
echo "Build eventlog-live-otlp"
cabal build eventlog-live-otlp -v0
EVENTLOG_LIVE_OTLP_BIN=$(cabal list-bin exe:eventlog-live-otlp -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the eventlog pipe
JUMPY_JUMP_EVENTLOG_FIFO="$TMPDIR/jumpy-jump-eventlog.fifo"
mkfifo "${JUMPY_JUMP_EVENTLOG_FIFO}" || exit

# Create the screen pipe for jumpy-jump
JUMPY_JUMP_SCREEN_FIFO="$TMPDIR/jumpy-jump-screen.fifo"
mkfifo "$JUMPY_JUMP_SCREEN_FIFO" || exit

# Create the screen pipe for eventlog-live-otlp
EVENTLOG_LIVE_OTLP_SCREEN_FIFO=$TMPDIR/eventlog-live-otlp-screen.fifo
mkfifo "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO" || exit

# Create the command to start jumpy-jump
# shellcheck disable=SC2089
JUMPY_JUMP_CMD="
echo 'Start jumpy-jump' && \
	${JUMPY_JUMP_BIN} \
		+RTS \
		-l \
		-ol'${JUMPY_JUMP_EVENTLOG_FIFO}' \
		-hT \
		-p \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live-otlp
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTLP_CMD="
echo 'Start eventlog-live-otlp (for jumpy-jump)' && \
	${EVENTLOG_LIVE_OTLP_BIN} \
		--stats \
		--config='$DIR/eventlog-live.yaml' \
	    --eventlog-file='${JUMPY_JUMP_EVENTLOG_FIFO}' \
	    -hT
"

# Create the screen conf file
SCREEN_CONF="$TMPDIR/screen.conf"
cat > "$SCREEN_CONF" << 'EOF' || exit
split
split -v
focus right
screen -t 'jumpy-jump/stderr' sh -c 'tty > "$JUMPY_JUMP_SCREEN_FIFO"; read done < "$JUMPY_JUMP_SCREEN_FIFO"'
focus left
screen -t 'jumpy-jump/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$JUMPY_JUMP_SCREEN_FIFO"; eval "$JUMPY_JUMP_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$JUMPY_JUMP_SCREEN_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otlp/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO"; read done < "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO"'
focus left
screen -t 'eventlog-live-otlp/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTLP_SCREEN_FIFO"; eval "$EVENTLOG_LIVE_OTLP_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTLP_FOR_JUMPY_JUMP_SCREEN_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	JUMPY_JUMP_SCREEN_FIFO \
	JUMPY_JUMP_CMD \
	EVENTLOG_LIVE_OTLP_SCREEN_FIFO \
	EVENTLOG_LIVE_OTLP_CMD
screen -mc "$SCREEN_CONF"
