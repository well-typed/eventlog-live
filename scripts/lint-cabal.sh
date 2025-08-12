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

# Lint Cabal files
echo "Lint Cabal files with Cabal version ${ACTUAL_VERSION}"
echo

# POSIX compliant method for 'pipefail':
FAIL=$(mktemp)

# shellcheck disable=SC2046
set -- $(git ls-files --exclude-standard --no-deleted --deduplicate '*.cabal')

CWD="$(pwd)"
for PACKAGE_CABAL_FILE; do
  PACKAGE_DIR=$(dirname "${PACKAGE_CABAL_FILE}")
  echo "Lint ${PACKAGE_DIR}"
  cd "${PACKAGE_DIR}" && ${CABAL} check -ilicense-none || echo >"${FAIL}"
  cd "${CWD}" || exit 2
  echo
done

# Check whether or not any subcommand failed:
if [ -s "${FAIL}" ]; then
  rm "${FAIL}"
  exit 1
else
  rm "${FAIL}"
  exit 0
fi
