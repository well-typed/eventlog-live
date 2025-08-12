#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^shellcheck=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find shellcheck:
#
# 1. Use SHELLCHECK if it is set.
# 2. Look for shellcheck-$EXPECTED_VERSION.
# 3. Look for shellcheck.
#
if [ "${SHELLCHECK}" = "" ]; then
  if ! SHELLCHECK="$(which "shellcheck-${EXPECT_VERSION}")"; then
    if ! SHELLCHECK="$(which "shellcheck")"; then
      echo "Requires ShellCheck ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install ShellCheck-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check shellcheck version:
ACTUAL_VERSION="$(${SHELLCHECK} --version | head -n 2 | tail -n 1 | cut -d' ' -f2)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires shellcheck ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Lint shell scripts
echo "Lint shell scripts with ShellCheck version ${ACTUAL_VERSION}"
# shellcheck disable=SC2086
if ! git ls-files --exclude-standard --no-deleted --deduplicate '*.sh' | xargs -L50 ${SHELLCHECK} -s sh; then
  exit 1
fi
