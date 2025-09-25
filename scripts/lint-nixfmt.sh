#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^nixfmt=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find nixfmt:
#
# 1. Use NIXFMT if it is set.
# 2. Look for nixfmt-$EXPECTED_VERSION.
# 3. Look for nixfmt.
#
if [ "${NIXFMT}" = "" ]; then
  if ! NIXFMT="$(which "nixfmt-${EXPECT_VERSION}")"; then
    if ! NIXFMT="$(which "nixfmt")"; then
      echo "Requires nixfmt ${EXPECT_VERSION}; no version found"
      exit 1
    fi
  fi
fi

# Check nixfmt version:
ACTUAL_VERSION="$("${NIXFMT}" --version | head -n 1 | cut -d' ' -f2)"
if [ "${ACTUAL_VERSION}" != "${EXPECT_VERSION}" ]; then
  echo "Requires nixfmt ${EXPECT_VERSION}; version ${ACTUAL_VERSION} found"
  # Version mismatch is an error on CI:
  [ "${CI}" = "" ] || exit 1
fi

# Lint Nix files
echo "Lint Nix files with nixfmt version ${ACTUAL_VERSION}"
# shellcheck disable=SC2086
git ls-files --exclude-standard --no-deleted --deduplicate '*.nix' | xargs -L1 ${NIXFMT} --check --strict --verify
