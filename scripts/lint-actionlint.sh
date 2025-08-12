#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^actionlint=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find actionlint:
#
# 1. Use ACTIONLINT if it is set.
# 2. Look for actionlint-$EXPECTED_VERSION.
# 3. Look for actionlint.
#
if [ "${ACTIONLINT}" = "" ]; then
  if ! ACTIONLINT="$(which "actionlint-${EXPECT_VERSION}")"; then
    if ! ACTIONLINT="$(which "actionlint")"; then
      echo "Requires actionlint ${EXPECT_VERSION}; no version found"
      echo "To install, run:"
      echo
      echo "  cabal install actionlint-${EXPECT_VERSION}"
      echo
      exit 1
    fi
  fi
fi

# Check actionlint version:
ACTUAL_VERSION="$("${ACTIONLINT}" --version | head -n 1)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires actionlint ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Lint GitHub Actions workflows
echo "Lint GitHub Actions workflows with actionlint version ${ACTUAL_VERSION}"
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '.github/workflows/*.yml' | xargs -L50 ${ACTIONLINT}
