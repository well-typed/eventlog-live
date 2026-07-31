#!/bin/sh -e

#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_WAIT="true"
export GHC_EVENTLOG_UNIX_PATH="/tmp/spectral_norm_eventlog.sock"

# Configure OpenTelemetry exporter
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="spectral-norm"
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

# Build spectral-norm
echo "Build spectral-norm"
cabal build spectral-norm --constraint=eventlog-socket+control -v0
SPECTRAL_NORM_BIN=$(cabal list-bin exe:spectral-norm --constraint=eventlog-socket+control -v0 | head -n1)

# Build eventlog-live-otlp
echo "Build eventlog-live-otlp"
cabal build eventlog-live-otlp -v0
EVENTLOG_LIVE_OTLP_BIN=$(cabal list-bin exe:eventlog-live-otlp -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for spectral-norm
SPECTRAL_NORM_FIFO="$TMPDIR/spectral-norm.fifo"
mkfifo "$SPECTRAL_NORM_FIFO" || exit

# Create the screen pipe for eventlog-live-otlp
EVENTLOG_LIVE_OTLP_FIFO=$TMPDIR/eventlog-live-otlp.fifo
mkfifo "$EVENTLOG_LIVE_OTLP_FIFO" || exit

# Create the command to start spectral-norm
# shellcheck disable=SC2089
SPECTRAL_NORM_CMD="
echo 'Start spectral-norm' && \
	${SPECTRAL_NORM_BIN} \
		15000 20000 25000 30000 15000 20000 25000 30000 15000 20000 25000 30000 \
		+RTS \
		-l \
		-hT \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live-otlp
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTLP_CMD="
echo 'Start eventlog-live-otlp (for spectral-norm)' && \
	${EVENTLOG_LIVE_OTLP_BIN} \
		--stats \
		--config='$DIR/eventlog-live.yaml' \
	    --eventlog-socket='$GHC_EVENTLOG_UNIX_PATH' \
	    -hT \
		+RTS -l -hT --eventlog-flush-interval=1 -RTS
"

# Create the screen conf file
SCREEN_CONF="$TMPDIR/screen.conf"
cat > "$SCREEN_CONF" << 'EOF' || exit
split
split -v
focus right
screen -t 'spectral-norm/stderr' sh -c 'tty > "$SPECTRAL_NORM_FIFO"; read done < "$SPECTRAL_NORM_FIFO"'
focus left
screen -t 'spectral-norm/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$SPECTRAL_NORM_FIFO"; eval "$SPECTRAL_NORM_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$SPECTRAL_NORM_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otlp/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTLP_FIFO"; read done < "$EVENTLOG_LIVE_OTLP_FIFO"'
focus left
screen -t 'eventlog-live-otlp/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTLP_FIFO"; eval "$EVENTLOG_LIVE_OTLP_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTLP_FOR_SPECTRAL_NORM_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	SPECTRAL_NORM_FIFO \
	SPECTRAL_NORM_CMD \
	EVENTLOG_LIVE_OTLP_FIFO \
	EVENTLOG_LIVE_OTLP_CMD
screen -mc "$SCREEN_CONF"
