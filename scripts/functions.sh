#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^cabal=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal:
#
# 1. Use CABAL if it is set.
# 2. Look for cabal-$EXPECTED_VERSION.
# 3. Look for cabal.
#
if [ "${CABAL}" = "" ]; then
	if ! CABAL="$(which "cabal-${EXPECT_VERSION}")"; then
		if ! CABAL="$(which "cabal")"; then
			echo "Requires cabal ${EXPECT_VERSION}; no version found"
			exit 1
		fi
	fi
fi

# Find project root:
#
# This only works if the current working directory is somewhere
# under the project root and the root has a cabal.project file.
#
find_project_root() {
	DIR="$(pwd)"
	while [ ! -f "${DIR}/cabal.project" ]; do
		if ! DIR="$(realpath "${DIR}/..")"; then
			return 1
		fi
	done
	echo "${DIR}"
}
PROJECT_ROOT="$(find_project_root)"

# Compute the hash of the input string
#
hash_digest() {
	(
		which -s md5 && md5 -s "$1" | cut -c1-8
	) ||
		(
			which -s md5sum && echo "$1" | md5sum - | cut -c1-8
		) ||
		(
			which -s cksum && (
				(
					echo "$1" | cksum -amd5 --raw - | cut -c1-8
				) || (
					echo "$1" | cksum | cut -c1-8
				)
			)
		)
}

# Find GHC version:
#
if [ "${GHC}" = "" ]; then
	GHC="$(which ghc)"
fi

cabal_build() {
	# Get the component name
	COMPONENT="$1"
	shift

	# Get the build options
	BUILD_OPTS=""
	PROJECT_FILE="cabal.project"
	PROJECT_DIR="${PROJECT_ROOT}"
	while [ $# -gt 0 ]; do
		case $1 in
		# Handle cabal options:
		--builddir=*)
			echo "Warning: ignored option $1" 1>&2
			;;
		--project-dir=*)
			PROJECT_DIR="${1#*=}"
			shift
			;;
		--project-dir)
			PROJECT_DIR="$2"
			shift
			shift
			;;
		--project-file=*)
			PROJECT_FILE="${1#*=}"
			shift
			;;
		--project-file)
			PROJECT_FILE="$2"
			shift
			shift
			;;
		--with-compiler=*)
			GHC="${1#*=}"
			shift
			;;
		-w | --with-compiler)
			GHC="$2"
			shift
			shift
			;;
		*)
			BUILD_OPTS="${BUILD_OPTS} $1"
			shift
			;;
		esac
		BUILD_OPTS=" --with-compiler=${GHC}${BUILD_OPTS}"
	done

	# Set directory for source distributions
	SDIST_DIR="${PROJECT_ROOT}/dist-newstyle/sdist"
	mkdir -p "${SDIST_DIR}"

	# Run cabal sdist
	echo "Creating source distributions..." 1>&2
	${CABAL} sdist all --output-directory=${SDIST_DIR} 1>&2

	# Create directory for sources
	SOURCE_DIR="${PROJECT_ROOT}/dist-newstyle/source"
	mkdir -p "${SOURCE_DIR}"

	# Create temporary directory for sources
	SOURCE_TMPDIR="$(mktemp -d)"
	trap 'rm -rf "${SOURCE_TMPDIR}"' EXIT INT TERM HUP

	# Extract the sdists:
	find "${SDIST_DIR}/" -name '*.tar.gz' -type f -print | while IFS= read -r sdist; do
		# TODO: Extract sdists to temporary directory, then rsync to SDISTS
		tar -xzf "${sdist}" -C "${SOURCE_TMPDIR}" 1>&2
	done

	# Create the default cabal.project file:
	echo "import: cabal.project.config" >"${SOURCE_TMPDIR}/cabal.project"
	find "${SOURCE_TMPDIR}" -type d -mindepth 1 -maxdepth 1 | while IFS= read -r pkg; do
		echo "packages: ${pkg#"${SOURCE_TMPDIR}/"}" >>"${SOURCE_TMPDIR}/cabal.project"
	done

	# Copy the cabal.project.config file:
	if [ -f "${PROJECT_DIR}/cabal.project.config" ]; then
		cp "${PROJECT_DIR}/cabal.project.config" "${SOURCE_TMPDIR}"
	fi

	# Copy the user-provided project file:
	if [ "${PROJECT_FILE}" != "cabal.project" ]; then
		cp "${PROJECT_DIR}/${PROJECT_FILE}" "${SOURCE_TMPDIR}"
	fi

	# Synchronise the temporary sources with the previous sources
	if ! rsync -ia --no-times --checksum "${SOURCE_TMPDIR}/" "${SOURCE_DIR}/"; then
		# If this fails, just copy the whole thing...
		cp -r "${SOURCE_TMPDIR}/"* "${SOURCE_DIR}/"
	fi

	# Get the build directory
	BUILD_ROOT="../$(hash_digest -s "${BUILD_OPTS}")"

	# Run cabal build
	echo "Building ${COMPONENT} with${BUILD_OPTS}" 1>&2
	BUILD_OPTS="--builddir=${BUILD_ROOT} --project-dir=${SOURCE_DIR} --project-file=${PROJECT_FILE}${BUILD_OPTS}"
	${CABAL} build ${COMPONENT} ${BUILD_OPTS} 1>&2

	case "${COMPONENT}" in
	exe:* | test:* | *:exe:* | *:test:*)
		# Run cabal list-bin, if the component is an executable or test.
		${CABAL} list-bin ${COMPONENT} ${BUILD_OPTS} | head -n1
		;;
	esac
}
