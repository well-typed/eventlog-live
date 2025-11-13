#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Set the eventlog socket
export GHC_EVENTLOG_SOCKET="/tmp/oddball_eventlog.sock"

# Build oddball
echo "Build oddball"
cabal build oddball -v0
ODDBALL_BIN=$(cabal list-bin exe:oddball -v0 | head -n1)

# Build eventlog-live-otelcol
echo "Build eventlog-live-otelcol"
cabal build eventlog-live-otelcol -v0
EVENTLOG_LIVE_OTELCOL_BIN=$(cabal list-bin exe:eventlog-live-otelcol -v0 | head -n1)

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Create the screen pipe for oddball
ODDBALL_FIFO="$TMPDIR/oddball.fifo"
mkfifo "$ODDBALL_FIFO" || exit

# Create the screen pipe for eventlog-live-otelcol
EVENTLOG_LIVE_OTELCOL_FIFO=$TMPDIR/eventlog-live-otelcol.fifo
mkfifo "$EVENTLOG_LIVE_OTELCOL_FIFO" || exit

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

# Create the command to start eventlog-live-otelcol
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTELCOL_CMD="
echo 'Start eventlog-live-otelcol (for oddball)' && \
	${EVENTLOG_LIVE_OTELCOL_BIN} \
		--verbosity=debug \
		--stats \
		--config='$DIR/oddball-otelcol-config.yaml' \
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
screen -t 'oddball/stderr' sh -c 'tty > "$ODDBALL_FIFO"; read done < "$ODDBALL_FIFO"'
focus left
screen -t 'oddball/stdout' sh -c 'read tty < "$ODDBALL_FIFO"; eval "$ODDBALL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$ODDBALL_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otelcol/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTELCOL_FIFO"; read done < "$EVENTLOG_LIVE_OTELCOL_FIFO"'
focus left
screen -t 'eventlog-live-otelcol/stdout' sh -c 'read tty < "$EVENTLOG_LIVE_OTELCOL_FIFO"; eval "$EVENTLOG_LIVE_OTELCOL_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTELCOL_FOR_ODDBALL_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	ODDBALL_FIFO \
	ODDBALL_CMD \
	EVENTLOG_LIVE_OTELCOL_FIFO \
	EVENTLOG_LIVE_OTELCOL_CMD
screen -mc "$SCREEN_CONF"
