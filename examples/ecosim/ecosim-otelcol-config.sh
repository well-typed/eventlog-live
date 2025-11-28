#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/ecosim_eventlog.sock"

# Build ecosim
echo "Build ecosim"
cabal build -w ghc-9.14 ecosim -v0
ECOSIM_BIN=$(cabal list-bin -w ghc-9.14 exe:ecosim -v0 | head -n1)

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0
EVENTLOG_LIVE_OTELCOL_BIN=$(cabal list-bin exe:eventlog-live-otelcol -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for ecosim
ECOSIM_FIFO="$TMPDIR/ecosim.fifo"
mkfifo "$ECOSIM_FIFO" || exit

# Create the screen pipe for eventlog-live-otelcol
EVENTLOG_LIVE_OTELCOL_FIFO=$TMPDIR/eventlog-live-otelcol.fifo
mkfifo "$EVENTLOG_LIVE_OTELCOL_FIFO" || exit

# Create the command to start ecosim
# shellcheck disable=SC2089
ECOSIM_CMD="
echo 'Start ecosim' && \
	${ECOSIM_BIN} \
		+RTS \
		-l \
		-hT \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live-otelcol
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTELCOL_CMD="
echo 'Start eventlog-live-otelcol (for ecosim)' && \
	${EVENTLOG_LIVE_OTELCOL_BIN} \
		--verbosity=debug \
		--stats \
		--config='$DIR/ecosim-otelcol-config.yaml' \
		--service-name='ecosim' \
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
screen -t 'ecosim/stderr' sh -c 'tty > "$ECOSIM_FIFO"; read done < "$ECOSIM_FIFO"'
focus left
screen -t 'ecosim/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$ECOSIM_FIFO"; eval "$ECOSIM_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$ECOSIM_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otelcol/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTELCOL_FIFO"; read done < "$EVENTLOG_LIVE_OTELCOL_FIFO"'
focus left
screen -t 'eventlog-live-otelcol/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTELCOL_FIFO"; eval "$EVENTLOG_LIVE_OTELCOL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTELCOL_FOR_ECOSIM_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	ECOSIM_FIFO \
	ECOSIM_CMD \
	EVENTLOG_LIVE_OTELCOL_FIFO \
	EVENTLOG_LIVE_OTELCOL_CMD
screen -mc "$SCREEN_CONF"
