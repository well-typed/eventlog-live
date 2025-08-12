#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^hlint=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find HLint:
#
# 1. Use HLINT if it is set.
# 2. Look for hlint-$EXPECTED_VERSION.
# 3. Look for hlint.
#
if [ "${HLINT}" = "" ]; then
  if ! HLINT="$(which "hlint-${EXPECT_VERSION}")"; then
    if ! HLINT="$(which "hlint")"; then
      echo "Requires HLint ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install hlint-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check HLint version:
ACTUAL_VERSION="$("${HLINT}" --version | head -n 1 | cut -d' ' -f 2 | sed -E 's/v(.*),/\1/')"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires HLint ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Lint Haskell files
echo "Lint Haskell files with HLint version ${ACTUAL_VERSION}"
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '*.hs' | xargs -L50 ${HLINT}
