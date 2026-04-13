#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/jumpy_jump_eventlog.sock"

# Build jumpy-jump
echo "Build jumpy-jump"
cabal build jumpy-jump --builddir=dist-newstyle/jumpy-jump-with-cost-centre-profiler -f-use-ghc-stack-profiler --enable-profiling -v0
JUMPY_JUMP_BIN=$(cabal list-bin exe:jumpy-jump --builddir=dist-newstyle/jumpy-jump-with-cost-centre-profiler -f-use-ghc-stack-profiler --enable-profiling -v0 | head -n1)

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0
EVENTLOG_LIVE_OTELCOL_BIN=$(cabal list-bin exe:eventlog-live-otelcol -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for jumpy-jump
JUMPY_JUMP_FIFO="$TMPDIR/jumpy-jump.fifo"
mkfifo "$JUMPY_JUMP_FIFO" || exit

# Create the screen pipe for eventlog-live-otelcol
EVENTLOG_LIVE_OTELCOL_FIFO=$TMPDIR/eventlog-live-otelcol.fifo
mkfifo "$EVENTLOG_LIVE_OTELCOL_FIFO" || exit

# Create the command to start jumpy-jump
# shellcheck disable=SC2089
JUMPY_JUMP_CMD="
echo 'Start jumpy-jump' && \
	${JUMPY_JUMP_BIN} \
		+RTS \
		-l \
		-p \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live-otelcol
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTELCOL_CMD="
echo 'Start eventlog-live-otelcol (for jumpy-jump)' && \
	${EVENTLOG_LIVE_OTELCOL_BIN} \
		--verbosity=debug \
		--stats \
		--config='$DIR/jumpy-jump-otelcol-config.yaml' \
		--service-name='jumpy-jump' \
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
screen -t 'jumpy-jump/stderr' sh -c 'tty > "$JUMPY_JUMP_FIFO"; read done < "$JUMPY_JUMP_FIFO"'
focus left
screen -t 'jumpy-jump/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$JUMPY_JUMP_FIFO"; eval "$JUMPY_JUMP_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$JUMPY_JUMP_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otelcol/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTELCOL_FIFO"; read done < "$EVENTLOG_LIVE_OTELCOL_FIFO"'
focus left
screen -t 'eventlog-live-otelcol/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTELCOL_FIFO"; eval "$EVENTLOG_LIVE_OTELCOL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTELCOL_FOR_JUMPY_JUMP_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	JUMPY_JUMP_FIFO \
	JUMPY_JUMP_CMD \
	EVENTLOG_LIVE_OTELCOL_FIFO \
	EVENTLOG_LIVE_OTELCOL_CMD
screen -mc "$SCREEN_CONF"
