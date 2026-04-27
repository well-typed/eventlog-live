#!/bin/sh -e

#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/spectral_norm_eventlog.sock"

# Build spectral-norm
echo "Build spectral-norm"
cabal build spectral-norm -v0
SPECTRAL_NORM_BIN=$(cabal list-bin exe:spectral-norm -v0 | head -n1)

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0
EVENTLOG_LIVE_OTELCOL_BIN=$(cabal list-bin exe:eventlog-live-otelcol -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for spectral-norm
SPECTRAL_NORM_FIFO="$TMPDIR/spectral-norm.fifo"
mkfifo "$SPECTRAL_NORM_FIFO" || exit

# Create the screen pipe for eventlog-live-otelcol
EVENTLOG_LIVE_OTELCOL_FIFO=$TMPDIR/eventlog-live-otelcol.fifo
mkfifo "$EVENTLOG_LIVE_OTELCOL_FIFO" || exit

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

# Create the command to start eventlog-live-otelcol
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTELCOL_CMD="
echo 'Start eventlog-live-otelcol (for spectral-norm)' && \
	${EVENTLOG_LIVE_OTELCOL_BIN} \
		--verbosity=debug \
		--stats \
		--config='$DIR/spectral-norm-otelcol-config.yaml' \
		--service-name='spectral-norm' \
	    --eventlog-socket '$GHC_EVENTLOG_SOCKET' \
	    -hT \
	    --otelcol-host=localhost \
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
screen -t 'eventlog-live-otelcol/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTELCOL_FIFO"; read done < "$EVENTLOG_LIVE_OTELCOL_FIFO"'
focus left
screen -t 'eventlog-live-otelcol/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTELCOL_FIFO"; eval "$EVENTLOG_LIVE_OTELCOL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTELCOL_FOR_SPECTRAL_NORM_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	SPECTRAL_NORM_FIFO \
	SPECTRAL_NORM_CMD \
	EVENTLOG_LIVE_OTELCOL_FIFO \
	EVENTLOG_LIVE_OTELCOL_CMD
screen -mc "$SCREEN_CONF"
