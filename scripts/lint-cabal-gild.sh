#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^cabal-gild=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find cabal-gild:
#
# 1. Use CABAL_GILD if it is set.
# 2. Look for cabal-gild-$EXPECTED_VERSION.
# 3. Look for cabal-gild.
#
if [ "${CABAL_GILD}" = "" ]; then
  if ! CABAL_GILD="$(which "cabal-gild-${EXPECT_VERSION}")"; then
    if ! CABAL_GILD="$(which "cabal-gild")"; then
      echo "Requires cabal-gild ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install cabal-gild-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check cabal-gild version:
ACTUAL_VERSION="$("${CABAL_GILD}" --version | head -n 1)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires cabal-gild ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Lint Cabal files
echo "Lint Cabal files with cabal-gild version ${ACTUAL_VERSION}"
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate -z '*.cabal' | xargs --verbose -0 -L1 ${CABAL_GILD} --mode=check
