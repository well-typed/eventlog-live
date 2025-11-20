#!/bin/sh

# Check cabal-docspec version
EXPECT_VERSION="$(awk -F'=' '/^cabal-docspec=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal-docspec:
#
# 1. Use CABAL_DOCSPEC if it is set.
# 2. Look for cabal-docspec-$EXPECTED_VERSION.
# 3. Look for cabal-docspec.
#
if [ "${CABAL_DOCSPEC}" = "" ]; then
  if ! CABAL_DOCSPEC="$(which "cabal-docspec-${EXPECT_VERSION}")"; then
    if ! CABAL_DOCSPEC="$(which "cabal-docspec")"; then
      echo "Requires cabal-docspec ${EXPECT_VERSION}; no version found"
      echo "See: https://github.com/phadej/cabal-extras/"
      exit 1
    fi
  fi
fi

# Check fourmolu version:
ACTUAL_VERSION="$(${CABAL_DOCSPEC} --version | head -n 1)"
if [ ! "${ACTUAL_VERSION}" = "${EXPECT_VERSION}" ]; then
  echo "Requires cabal-docspec ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Test Haskell files with cabal-docspec
echo "Testing Haskell files with cabal-docspec version ${ACTUAL_VERSION}..."
# shellcheck disable=SC2016
if [ "${SKIP_CABAL_BUILD}" = "" ]; then
  if ! cabal build all; then
    exit 1
  fi
fi
${CABAL_DOCSPEC} \
  --verbose \
  --no-check-properties \
  || exit 1
