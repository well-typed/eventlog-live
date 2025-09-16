#!/bin/sh -e

# To install as a Git pre-commit hook, run:
#
# > ln ./scripts/pre-commit.sh .git/hooks/pre-commit
#

# POSIX compliant method for 'pipefail':
FAIL=$(mktemp)

# Check for unstaged changes in Shell scripts:
UNSTAGED_SCRIPT_FILES="$(git ls-files --exclude-standard --no-deleted --deduplicate --modified '*.sh')"
if [ ! "${UNSTAGED_SCRIPT_FILES}" = "" ]; then
  echo "Found unstaged Shell scripts"
  echo "${UNSTAGED_SCRIPT_FILES}"
  echo
  echo >"${FAIL}"
fi
# Check for unstaged changes in Haskell files:
UNSTAGED_HASKELL_FILES="$(git ls-files --exclude-standard --no-deleted --deduplicate --modified '*.hs')"
if [ ! "${UNSTAGED_HASKELL_FILES}" = "" ]; then
  echo "Found unstaged Haskell files"
  echo "${UNSTAGED_HASKELL_FILES}"
  echo
  echo >"${FAIL}"
fi
# Check for unstaged changes in Cabal files:
UNSTAGED_CABAL_FILES="$(git ls-files --exclude-standard --no-deleted --deduplicate --modified '*.cabal')"
if [ ! "${UNSTAGED_CABAL_FILES}" = "" ]; then
  echo "Found unstaged Cabal files"
  echo "${UNSTAGED_CABAL_FILES}"
  echo
  echo >"${FAIL}"
fi

# Run various checks and formatters
(./scripts/format-cabal-fmt.sh || echo >"${FAIL}")
echo
(./scripts/format-fourmolu.sh || echo >"${FAIL}")
echo
(./scripts/format-prettier.sh || echo >"${FAIL}")
echo
(./scripts/lint-actionlint.sh || echo >"${FAIL}")
echo
(./scripts/lint-cabal.sh || echo >"${FAIL}")
echo
(./scripts/lint-hlint.sh || echo >"${FAIL}")
echo
(./scripts/lint-shellcheck.sh || echo >"${FAIL}")
echo

# Check whether or not any subcommand failed:
if [ -s "${FAIL}" ]; then
  rm "${FAIL}"
  exit 1
else
  rm "${FAIL}"
  exit 0
fi
