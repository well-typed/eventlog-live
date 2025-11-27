#!/usr/bin/env bash

# List supported dependencies:
declare -a DEPENDENCIES
DEPENDENCIES=("cabal-docspec" "cabal-fmt" "fourmolu" "hlint" "nixfmt" "ShellCheck")

# Extra dependencies:
declare -a EXTRA_DEPENDENCIES
EXTRA_DEPENDENCIES=("actionlint" "cabal" "ghc" "git" "prettier" "protoc")

# Read version information:
declare -A VERSION_INFO
VERSION_INFO=()

for dependency in "${DEPENDENCIES[@]}" "${EXTRA_DEPENDENCIES[@]}"; do
	dependency_lower="$(echo "${dependency}" | tr '[:upper:]' '[:lower:]')"
	version=
	VERSION_INFO["${dependency}"]="$(awk -F'=' "/^${dependency_lower}=/{print\$2}" ./scripts/dev-dependencies.txt)"
done

# Check for missing dependencies:
declare -A FOUND_INFO
FOUND_INFO=()

declare -a MISSING_DEPENDENCIES
MISSING_DEPENDENCIES=()

declare -a MISSING_EXTRA_DEPENDENCIES
MISSING_EXTRA_DEPENDENCIES=()

for dependency in "${DEPENDENCIES[@]}" "${EXTRA_DEPENDENCIES[@]}"; do
	version="${VERSION_INFO["${dependency}"]}"
	if found="$(which "${dependency}-${version}")"; then
		FOUND_INFO["${dependency}"]="${found}"
	else
		case "${dependency}" in
		actionlint | cabal | ghc | git | protoc)
			if found="$(which "${dependency}")"; then
				FOUND_INFO["${dependency}"]="${found}"
			else
				MISSING_EXTRA_DEPENDENCIES+=("${dependency}")
			fi
			;;
		prettier)
			if NPM="$(which npm)"; then
				FOUND_INFO['prettier']="${NPM} exec --yes --package=prettier@${version} prettier -- "
			else
				MISSING_EXTRA_DEPENDENCIES+=('prettier')
			fi
			;;
		*)
			MISSING_DEPENDENCIES+=("${dependency}")
			;;
		esac
	fi
done

# Print installed dependencies:
if [ ${#FOUND_INFO[@]} -gt 0 ]; then
	echo "The following dependencies are already installed:"
	echo
	for dependency in "${DEPENDENCIES[@]}" "${EXTRA_DEPENDENCIES[@]}"; do
		version="${VERSION_INFO["$dependency"]}"
		found="${FOUND_INFO["$dependency"]}"
		if [ -n "${found}" ]; then
			case "${dependency}" in
			actionlint)
				actual_version="$(${found} --version | head -n1)"
				;;
			ghc | cabal)
				actual_version="$(${found} --numeric-version)"
				;;
			protoc)
				actual_version="$(${found} --version | cut -d' ' -f2)"
				;;
			*)
				actual_version="${version}"
				;;
			esac
			if [ "${actual_version}" = "${version}" ]; then
				echo "- ${dependency}=${found}"
			else
				echo "- ${dependency}=${found} (expected ${version}, found ${actual_version})"
			fi
		fi
	done
	echo
fi

# Print missing extra dependencies:
if [ ${#MISSING_EXTRA_DEPENDENCIES[@]} -gt 0 ]; then
	echo "This script can NOT install the following dependencies:"
	echo
	for dependency in "${MISSING_EXTRA_DEPENDENCIES[@]}"; do
		echo "- ${dependency}-${VERSION_INFO[${dependency}]}"
	done
	echo
	for dependency in "${MISSING_EXTRA_DEPENDENCIES[@]}"; do
		case "${dependency}" in
		actionlint)
			echo "To use actionlint, follow the installation instructions at:"
			echo "https://github.com/rhysd/actionlint?tab=readme-ov-file#quick-start"
			echo
			;;
		prettier)
			echo "To use prettier, follow the installation instructions for Node.js at:"
			echo "https://nodejs.org/en/download"
			echo
			;;
		protoc)
			echo "To use protoc, follow the installation instructions at:"
			echo "https://protobuf.dev/installation/"
			echo
			;;
		esac
	done
fi

# Print missing dependencies:
if [ ${#MISSING_DEPENDENCIES[@]} -eq 0 ]; then
	echo "There is nothing for this script to do."
else
	echo "This script can install the following dependencies:"
	echo
	for dependency in "${MISSING_DEPENDENCIES[@]}"; do
		echo "- ${dependency}-${VERSION_INFO[${dependency}]}"
	done
	echo
	CABAL="${FOUND_INFO['cabal']}"
	GHC="${FOUND_INFO['ghc']}"
	GIT="${FOUND_INFO['git']}"
	if [ -z "${CABAL}" ] || [ -z "${CABAL}" ] || [ -z "${GIT}" ]; then
		echo "This script needs Cabal, GHC, and Git."
		exit 1
	fi
	echo "This script will use:"
	echo
	echo "- cabal=${CABAL}"
	echo "- ghc=${GHC}"
	echo "- git=${GIT}"
	echo

	# Ask for permission:
	read -r -p "Would you like to continue? [y/N] " response && [[ ! "$response" =~ [yY] ]] && exit 0

	# Run cabal update:
	echo "cabal update"
	"${CABAL}" -v0 -w"${GHC}" update

	# Create temporary directory:
	TMPDIR="$(mktemp -d)"
	trap 'read -r -p "Remove ${TMPDIR}? [y/N]" response && [[ ! "$response" =~ [nN] ]] && rm -rf "${TMPDIR}"' EXIT

	# Create temporary file for warnings:
	WARNINGS="${TMPDIR}/warnings"

	# Install dependencies:
	for dependency in "${MISSING_DEPENDENCIES[@]}"; do
		echo
		echo "Installing ${dependency}..."
		version="${VERSION_INFO["${dependency}"]}"
		cabal_install_args="--install-method=copy --overwrite-policy=always --program-suffix=-${version}"
		case "$dependency" in
		cabal-docspec)
			# Clone source
			git_clone_args="--branch cabal-docspec-${version} --depth 1 https://github.com/phadej/cabal-extras.git ${TMPDIR}/cabal-extras"
			echo "git clone ${git_clone_args}"
			# shellcheck disable=SC2086
			if ! "${GIT}" clone -c advice.detachedHead=false -q ${git_clone_args}; then
				echo "cabal-docspec">>"${WARNINGS}"
				continue
			fi
			# Install cabal-docspec
			echo "cabal install ${cabal_install_args}"
			if ! pushd "${TMPDIR}/cabal-extras/cabal-docspec"; then
				echo "cabal-docspec">>"${WARNINGS}"
				continue
			fi
			# shellcheck disable=SC2086
			if ! "${CABAL}" -v0 -w"${GHC}" install ${cabal_install_args}; then
				echo "cabal-docspec">>"${WARNINGS}"
				continue
			fi
			if ! popd; then
				echo "cabal-docspec">>"${WARNINGS}"
				continue
			fi
			;;
		nixfmt)
			# Clone source
			git_clone_args="--branch v${version} --depth 1 https://github.com/NixOS/nixfmt.git ${TMPDIR}/nixfmt"
			echo "git clone ${git_clone_args}"
			# shellcheck disable=SC2086
			if ! "${GIT}" clone -c advice.detachedHead=false -q ${git_clone_args}; then
				echo "nixfmt">>"${WARNINGS}"
				continue
			fi
			# Install nixfmt
			echo "cabal install ${cabal_install_args}"
			if ! pushd "${TMPDIR}/nixfmt"; then
				echo "nixfmt">>"${WARNINGS}"
				continue
			fi
			# shellcheck disable=SC2086
			if ! "${CABAL}" -v0 -w"${GHC}" install ${cabal_install_args}; then
				echo "nixfmt">>"${WARNINGS}"
				continue
			fi
			if ! popd; then
				echo "nixfmt">>"${WARNINGS}"
				continue
			fi
			;;
		*)
			echo "cabal install ${dependency}-${version} ${cabal_install_args}"
			# shellcheck disable=SC2086
			if ! "${CABAL}" -v0 -w"${GHC}" install "${dependency}-${version}" ${cabal_install_args}; then
				echo "${dependency}">>"${WARNINGS}"
				continue
			fi
			;;
		esac
	done
	echo

	# Check for warnings:
	if [ -s "${WARNINGS}" ]; then
		echo "The following dependencies failed to install:"
		echo
		while read -r dependency; do
			echo "- ${dependency}"
		done <"${WARNINGS}"
		echo
		exit 1
	fi
fi
