# Scripts

This directory contains various scripts for code formatting and linting, as well as scripts for various build tasks such as building the Haddock API documentation.

The `pre-commit.sh` checks for unstaged files and then runs all formatters followed by all linters. It is intended to be installed as a Git pre-commit hook, which can be done by running:

```sh
ln ./scripts/pre-commit.sh .git/hooks/pre-commit
```

The naming scheme of most of these scripts is `<ACTION>-<TOOL>`, e.g., `lint-fourmolu.sh` lints the Haskell code in the repository using fourmolu, whereas `format-formolu.sh` formats the Haskell code.

The `dev-dependencies.txt` file contains the expected version of each tool. These versions are used by every script in this directory, as well as all GitHub Actions workflows.

The scripts in this directory MUST follow this logic for finding their external tools:

1.  If the `<UPPER_TOOL_NAME>` environment variable is set, use it.
    For instance, `lint-fourmolu.sh` uses the path in `FOURMOLU`, if set.
2.  Otherwise, search the PATH for `<tool-name>-<expected-tool-version>`.
    For instance, `lint-fourmolu.sh` searches for `fourmolu-0.16.2.0`.
3.  Otherwise, search the PATH for `<tool-name>`.
    For instance, `lint-fourmolu.sh` searches for `fourmolu`.

If the actual version of the tool does not match the expected version of the tool,
the script should print a warning, except if the `CI` environment variable is set, in which case the script should exit with an error.
