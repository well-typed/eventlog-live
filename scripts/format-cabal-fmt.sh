#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^cabal-fmt=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal-fmt:
#
# 1. Use CABAL_FMT if it is set.
# 2. Look for cabal-fmt-$EXPECTED_VERSION.
# 3. Look for cabal-fmt.
#
if [ "${CABAL_FMT}" = "" ]; then
  if ! CABAL_FMT="$(which "cabal-fmt-${EXPECT_VERSION}")"; then
    if ! CABAL_FMT="$(which "cabal-fmt")"; then
      echo "Requires cabal-fmt ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install cabal-fmt-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check cabal-fmt version:
ACTUAL_VERSION="$("${CABAL_FMT}" --version | head -n 1)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires cabal-fmt ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Format Cabal files
echo "Format Cabal files with cabal-fmt version ${ACTUAL_VERSION}"
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '*.cabal' | xargs -L1 ${CABAL_FMT} --inplace
