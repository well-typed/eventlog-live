#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Configure OpenTelemetry exporter
export OTEL_LOG_LEVEL="debug"
export OTEL_SERVICE_NAME="ghc"
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc"

# Get the GHC_DIR argument.
GHC_DIR="$1"
if [ ! -d "${GHC_DIR}" ]; then
	echo "Usage: $0 [GHC_DIR]"
	echo
	echo "  GHC_DIR  The root of an instrumented GHC source repository."
	exit 1
fi

# Create the temporary directory
TMPDIR=$(mktemp -d) || exit
trap 'rm -rf "$TMPDIR"' EXIT INT TERM HUP

# Build eventlog-live-otlp
echo "Build eventlog-live-otlp"
cabal build eventlog-live-otlp -v0
EVENTLOG_LIVE_OTLP_BIN=$(cabal list-bin exe:eventlog-live-otlp -v0 | head -n1)

# Find pre-built GHC binary.
GHC="${GHC_DIR}/_build/stage1/bin/ghc"
if [ ! -f "${GHC}" ]; then
	echo "Could not find pre-built stage2 GHC compiler at ${GHC}"
	echo
	echo "This script requires a pre-built GHC in ${GHC_DIR}."
	exit 1
fi
echo "Found GHC at ${GHC}"

# Find bootstrap Cabal on the PATH.
if [ "${CABAL}" = "" ]; then
	if ! CABAL="$(which "cabal")"; then
		echo "Could not find Cabal on the PATH or in the CABAL environment variable."
		echo
		echo "This script requires a copy of cabal to bootstrap Cabal-syntax."
		exit 1
	fi
fi
echo "Found Cabal at ${CABAL}"

# Create the screen pipe for ghc
GHC_FIFO="$TMPDIR/ghc.fifo"
mkfifo "$GHC_FIFO" || exit

# Create the screen pipe for eventlog-live-otlp
EVENTLOG_LIVE_OTLP_FIFO=$TMPDIR/eventlog-live-otlp.fifo
mkfifo "$EVENTLOG_LIVE_OTLP_FIFO" || exit

# Set source directories for Cabal and Cabal-syntax.
CABAL_DIR="${GHC_DIR}/libraries/Cabal"
CABAL_SYNTAX_DIR="${CABAL_DIR}/Cabal-syntax"
CWD="$(pwd)"
trap 'cd "$CWD"' EXIT INT TERM HUP
cd "${CABAL_DIR}"

# Get GHC command.
GHC_CMD_FILE="${DIR}/build-Cabal-syntax.sh"
if [ -f "${GHC_CMD_FILE}" ]; then
	# Read GHC command from file.
	echo "Read GHC command to build Cabal-syntax."
	GHC_CMD="$(cat "${GHC_CMD_FILE}")"
else
	# Create the setup pipe for cabal
	CABAL_FIFO="$TMPDIR/cabal.fifo"
	mkfifo "$CABAL_FIFO" || exit

	# Capture GHC command from active build.
	echo "Capture GHC command to build Cabal-syntax from verbose build."
	echo "Running: ${CABAL} build -w \"${GHC}\" lib:Cabal-syntax -v3"
	#shellcheck disable=SC2069
	${CABAL} build -w "${GHC}" lib:Cabal-syntax -v3 2>&1 >"${CABAL_FIFO}" &
	CABAL_PID=$!
	while read -r line; do
		case "${line}" in
		"Running: ${GHC} --make -fbuilding-cabal-package "*)
			GHC_CMD="${line#Running: }"
			echo "Captured GHC command from verbose build; stop Cabal-syntax build."
			echo "${GHC_CMD}"
			kill -s KILL ${CABAL_PID}
			break
			;;
		*)
			continue
			;;
		esac
	done <"${CABAL_FIFO}"
	sleep 1
	# Ask for permission to proceed.
	echo "Proceed? [Y/n] "
	read -r yn
	case $yn in
	[Nn]*)
		exit 0
		;;
	*)
		;;
	esac
	# Write GHC command to shell script.
	echo "${GHC_CMD}" >"${DIR}/build-Cabal-syntax.sh"
fi

# Set the eventlog socket
export GHC_EVENTLOG_WAIT="true"
export GHC_EVENTLOG_UNIX_PATH="/tmp/ghc_eventlog.sock"

# Create the command to start building Cabal-syntax
# shellcheck disable=SC2089
GHC_CMD="cd \"${CABAL_SYNTAX_DIR}\" && ${GHC_CMD} -fforce-recomp +RTS -l -hT --eventlog-flush-interval=1 -RTS"

# Create the command to start eventlog-live-otlp
# shellcheck disable=SC2089
EVENTLOG_LIVE_OTLP_CMD="
echo 'Start eventlog-live-otlp' && \
	${EVENTLOG_LIVE_OTLP_BIN} \
		--stats \
		--config='$DIR/eventlog-live.yaml' \
	    --eventlog-socket '$GHC_EVENTLOG_UNIX_PATH' \
	    -hT
"

# Create the screen conf file
SCREEN_CONF="$TMPDIR/screen.conf"
cat >"$SCREEN_CONF" <<'EOF' || exit
split
split -v
focus right
screen -t 'ghc/stderr' sh -c 'tty > "$GHC_FIFO"; read -r done < "$GHC_FIFO"'
focus left
screen -t 'ghc/stdout' sh -c 'trap "screen -X quit" INT; read -r tty < "$GHC_FIFO"; eval "$GHC_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read -r prompt; echo done > "$GHC_FIFO"'
focus down
split -v
focus right
screen -t 'eventlog-live-otlp/stderr' sh -c 'tty > "$EVENTLOG_LIVE_OTLP_FIFO"; read -r done < "$EVENTLOG_LIVE_OTLP_FIFO"'
focus left
screen -t 'eventlog-live-otlp/stdout' sh -c 'trap "screen -X quit" INT; read -r tty < "$EVENTLOG_LIVE_OTLP_FIFO"; eval "$EVENTLOG_LIVE_OTLP_CMD" 2> "$tty"; echo "[Command exited with status $?, press enter to exit]"; read prompt; echo done > "$EVENTLOG_LIVE_OTLP_FOR_GHC_FIFO"'
EOF

# Start screen
# shellcheck disable=SC2090
export \
	GHC_FIFO \
	GHC_CMD \
	EVENTLOG_LIVE_OTLP_FIFO \
	EVENTLOG_LIVE_OTLP_CMD
screen -mc "$SCREEN_CONF"
