#!/bin/sh -e

# Get the script directory
DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd -P)

# Include helper functions
. "${DIR}/functions.sh"

# TODO: include different build flags

# Find installed GHC versions
INSTALLED="$(
	ghcup list -r -tghc -cinstalled \
		| sed -n 's/^ghc \([0-9]\{1,\}\.[0-9]\{1,\}\.[0-9]\{1,\}\) .*$/\1/p' \
		| sort \
		| uniq
		)"

# Find supported GHC versions
SUPPORTED="$(
	cat "${DIR}/../eventlog-live/eventlog-live.cabal" \
		| grep "^ *ghc *==" \
		| grep -o "[0-9]\+\.[0-9]\+\.[0-9]\+"
		)"

# Test each supported GHC version
echo "${SUPPORTED}" | while IFS= read -r supported; do
	major="$(echo "${supported}" | cut -d'.' -f1)"
	minor="$(echo "${supported}" | cut -d'.' -f2)"
	if [ "${INSTALLED#*"${major}.${minor}"}" = "${INSTALLED}" ]; then
		echo "Skip: GHC ${major}.${minor}"
	else
		installed="$(echo "${INSTALLED}" | grep "${major}\.${minor}\.[0-9]\+")"
		if GHC="$(which "ghc-${installed}")"; then
			echo "Test: GHC ${supported} using ${GHC}"
			cabal_build all --with-compiler="${GHC}"
		fi
	fi
done
