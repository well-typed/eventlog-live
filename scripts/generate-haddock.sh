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

# Check cabal version:
ACTUAL_VERSION="$("${CABAL}" --numeric-version | head -n 1)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  # Version mismatch is never an error:
  echo "Requires cabal ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
fi

# Create the OUTPUT directory
OUTPUT="${OUTPUT:-./dist-newstyle/haddocks}"
mkdir -p "${OUTPUT}"

# Create the PROLOGUE file
PROLOGUE="${OUTPUT}/prologue.haddock"
"./scripts/generate-haddock-prologue.hs" \
	<"./eventlog-live/eventlog-live.cabal" \
	>"${PROLOGUE}"

# Create the Haddocks
"${CABAL}" \
	haddock-project \
	--hackage \
	--output="${OUTPUT}" \
	--prologue="${PROLOGUE}"
