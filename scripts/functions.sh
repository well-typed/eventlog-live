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

# Determine the builddir based on the arguments.
#
cabal_builddir() {
	echo "test $*" | sed 's/ /-/g'
}

cabal_build() {
	${CABAL} build "$@" --builddir="dist-newstyle/$(cabal_builddir "$@")" 1>&2
}

cabal_list_bin() {
	if cabal_build "$@"; then
		${CABAL} list-bin "$@" --builddir="dist-newstyle/$(cabal_builddir "$@")" | head -n 1
	fi
}

cabal_build_from_sdist() {
	# Find the project root
	PROJECT_ROOT="$(find_project_root)"

	# Create source distributions
	SDIST_DIR="${PROJECT_ROOT}/dist-newstyle/sdist"
	${CABAL} sdist all --output-directory="${SDIST_DIR}" 1>&2

	# Extract sources to temporary directory
	SOURCE_TMPDIR="$(mktemp -d)"
	trap 'rm -rf "${SOURCE_TMPDIR}"' EXIT INT TERM HUP
	find "${SDIST_DIR}/" -name '*.tar.gz' -type f -print | while IFS= read -r sdist; do
		tar -xzf "${sdist}" -C "${SOURCE_TMPDIR}" 1>&2
	done

	# Create cabal.project file
	echo "import: cabal.config.project" >"${SOURCE_TMPDIR}/cabal.project"
	find "${SOURCE_TMPDIR}" -type d -mindepth 1 -maxdepth 1 | while IFS= read -r package; do
		echo "packages: ${package#"${SOURCE_TMPDIR}/"}" >>"${SOURCE_TMPDIR}/cabal.project"
	done

	# Copy other cabal project files
	find "${PROJECT_ROOT}/" -type f -mindepth 1 -maxdepth 1 -name 'cabal.*.project' | while IFS= read -r project_src; do
		project="${project_src#"${PROJECT_ROOT}/"}"
		project_dst="${SOURCE_TMPDIR}/${project}"
		cp "${project_src}" "${project_dst}"
	done

	# Copy sources to source directory
	SOURCE_DIR="${PROJECT_ROOT}/dist-newstyle/sources"
	if [ -d "${SOURCE_DIR}" ]; then
		find "${SOURCE_TMPDIR}/" -type f -print | while IFS= read -r source_new; do
			source="${source_new#"${SOURCE_TMPDIR}/"}"
			source_old="${SOURCE_DIR}/${source}"
			if [ -f "${source_old}" ]; then
				cksum_new="$(cksum "${source_new}" | cut -d' ' -f1)"
				cksum_old="$(cksum "${source_old}" | cut -d' ' -f1)"
				if [ "${cksum_new}" != "${cksum_old}" ]; then
					echo "Update ${source}"
					cp -f "${source_new}" "${source_old}"
				fi
			else
				cp "${source_new}" "${source_old}"
			fi
		done
	else
		cp -r "${SOURCE_TMPDIR}" "${SOURCE_DIR}"
	fi

	# Build the components
	${CABAL} build "$@" --builddir="../$(cabal_builddir "$@")" --project-dir="${SOURCE_DIR}" 1>&2
}
