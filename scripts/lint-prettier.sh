#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^prettier=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find prettier:
#
# 1. Use PRETTIER if it is set.
# 2. Look for prettier-$EXPECTED_VERSION.
# 3. Look for prettier.
# 4. Look for npm.
#
if [ "${PRETTIER}" = "" ]; then
  if ! PRETTIER="$(which "prettier-${EXPECT_VERSION}")"; then
    if ! PRETTIER="$(which "prettier")"; then
      if ! NPM="$(which "npm")"; then
        echo "Requires prettier ${EXPECT_VERSION}; no version found"
        echo "To install, run:"
        echo
        echo "  npm install -g prettier@${EXPECT_VERSION}"
        echo
        exit 1
      else
        PRETTIER="${NPM} exec prettier@${EXPECT_VERSION} -- "
      fi
    fi
  fi
fi

# Check prettier version:
ACTUAL_VERSION="$(NODE_NO_WARNINGS=1 ${PRETTIER} --version | head -n 1)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires prettier ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Lint JavaScript, TypeScript, JSON, YAML, and Markdown files with Prettier
echo "Lint JavaScript, TypeScript, JSON, YAML, and Markdown files with Prettier version ${ACTUAL_VERSION}"
echo
NODE_NO_WARNINGS=1 ${PRETTIER} -l --log-level=warn .
