#!/bin/bash -

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball__ghc_eventlog.sock"
export MY_GHC_EVENTLOG_SOCKET="/tmp/eventlog_live_otelcol__ghc_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0 -f+use-ghc-debug-stub
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0 -f+use-ghc-debug-stub
EVENTLOG_LIVE_OTELCOL_BIN=$(cabal list-bin exe:eventlog-live-otelcol -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for oddball
ODDBALL_FIFO="$TMPDIR/oddball.fifo"
mkfifo "$ODDBALL_FIFO" || exit

# Create the screen pipe for eventlog-live-otelcol (for oddball)
EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO=$TMPDIR/eventlog-live-otelcol-for-oddball.fifo
mkfifo "$EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO" || exit

# Create the screen pipe for eventlog-live-otelcol (for oddball)
EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_FIFO=$TMPDIR/eventlog-live-otelcol-for-itself.fifo
mkfifo "$EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_FIFO" || exit

# Create the command to start oddball
# shellcheck disable=SC2089
ODDBALL_CMD="
echo 'Start oddball' && \
	${ODDBALL_BIN} \
		+RTS \
		-l \
		-hT \
		--eventlog-flush-interval=1 \
		-RTS
"

# Create the command to start eventlog-live-otelcol (for oddball)
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_CMD="
echo 'Start eventlog-live-otelcol (for oddball)' && \
	${EVENTLOG_LIVE_OTELCOL_BIN} \
		--verbosity=debug \
		--stats \
		--config='$DIR/config/oddball.yaml' \
	    --eventlog-socket '$GHC_EVENTLOG_SOCKET' \
		--enable-my-eventlog-socket-unix '$MY_GHC_EVENTLOG_SOCKET' \
	    -hT \
	    --otelcol-host=localhost \
		+RTS -l -hT --eventlog-flush-interval=1 -RTS
"

# Create the command to start eventlog-live-otelcol (for itself)
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_CMD="
echo 'Start eventlog-live-otelcol (for itself)'
${EVENTLOG_LIVE_OTELCOL_BIN} \
	--verbosity=debug \
	--stats \
	--config='$DIR/config/eventlog-live-otelcol.yaml' \
    --eventlog-socket '$MY_GHC_EVENTLOG_SOCKET' \
    -hT \
    --otelcol-host=localhost
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
screen -t 'eventlog-live-otelcol-for-oddball/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO"; read done < "$EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO"'
focus left
screen -t 'eventlog-live-otelcol-for-oddball/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO"; eval "$EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otelcol-for-itself/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_FIFO"; read done < "$EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_FIFO"'
focus left
screen -t 'eventlog-live-otelcol-for-itself/stdout' sh -c 'trap "screen -X quit" INT; read tty < "$EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_FIFO"; eval "$EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	ODDBALL_FIFO \
	ODDBALL_CMD \
	EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO \
	EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_CMD \
	EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_FIFO \
	EVENTLOG_LIVE_OTELCOL_FOR_ITSELF_CMD
screen -mc "$SCREEN_CONF"
