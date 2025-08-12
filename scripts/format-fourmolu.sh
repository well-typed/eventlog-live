#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^fourmolu=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find fourmolu:
#
# 1. Use FOURMOLU if it is set.
# 2. Look for fourmolu-$EXPECTED_VERSION.
# 3. Look for fourmolu.
#
if [ "${FOURMOLU}" = "" ]; then
  if ! FOURMOLU="$(which "fourmolu-${EXPECT_VERSION}")"; then
    if ! FOURMOLU="$(which "fourmolu")"; then
      echo "Requires fourmolu ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install fourmolu-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check fourmolu version:
ACTUAL_VERSION="$("${FOURMOLU}" --version | head -n 1 | cut -d' ' -f2)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires fourmolu ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Format Haskell files
echo "Format Haskell files with cabal-fmt version ${ACTUAL_VERSION}"
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '*.hs' | xargs -L50 ${FOURMOLU} --mode=inplace
