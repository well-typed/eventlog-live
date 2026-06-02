# Overview

## Publishing a release for `eventlog-live`

1.  Ensure that the current HEAD is ready to be published:
    - The version number is updated in all relevant places. This includes at least `eventlog-live/eventlog-live.cabal` and `eventlog-live/CHANGELOG.md`. This includes the `source-repository this` declaration in `eventlog-live/eventlog-live.cabal`, which should refer to the Git tag that you will create in step (2).
    - The Haddock documentation builds without warnings and renders without errors.
    - The tests pass on CI.

2.  Create a Git tag of the form `eventlog-live-${VERSION}`, e.g., `eventlog-live-0.5.0.0`:

    ```sh
    git tag eventlog-live-${VERSION}
    ```

    > ⚠️ Replace `${VERSION}` with the new version.

3.  Publish the Git tag:

    ```sh
    git push --tags
    ```

4.  Build the source distribution.

    ```sh
    cabal sdist eventlog-live
    ```

    > ℹ️ This writes the source distribution to `dist-newstyle/sdist/eventlog-live-${VERSION}.tar.gz`.

5.  Upload the source distribution to Hackage _as a package candidate_
    - Navigate to <https://hackage.haskell.org/packages/candidates/upload>.
    - Upload the source distribution built in the previous step.

6.  Ensure that the package candidate page has no errors.

7.  Ensure that the `CHANGELOG.md` has no errors.

8.  Publish the candidate package.
    - On the package candidate package, click on _"[Publish]"_ and confirm.

## Publishing a release for `eventlog-live-otelcol`

1.  Ensure that the current HEAD is ready to be published:
    - The version number is updated in all relevant places. This includes at least `eventlog-live-otelcol/eventlog-live-otelcol.cabal` and `eventlog-live-otelcol/CHANGELOG.md`. This includes the `source-repository this` declaration in `eventlog-live-otelcol/eventlog-live-otelcol.cabal`, which should refer to the Git tag that you will create in step (2).
    - The Haddock documentation builds without warnings and renders without errors.
    - The tests pass on CI.

2.  Create a Git tag of the form `eventlog-live-otelcol-${VERSION}`, e.g., `eventlog-live-otelcol-0.6.0.0`:

    ```sh
    git tag eventlog-live-otelcol-${VERSION}
    ```

    > ⚠️ Replace `${VERSION}` with the new version.

3.  Publish the Git tag:

    ```sh
    git push --tags
    ```

4.  Build the source distribution.

    ```sh
    cabal sdist eventlog-live-otelcol
    ```

    > ℹ️ This writes the source distribution to `dist-newstyle/sdist/eventlog-live-otelcol-${VERSION}.tar.gz`.

5.  Upload the source distribution to Hackage _as a package candidate_
    - Navigate to <https://hackage.haskell.org/packages/candidates/upload>.
    - Upload the source distribution built in the previous step.

6.  Ensure that the package candidate page has no errors.

7.  Ensure that the `CHANGELOG.md` has no errors.

8.  Publish the candidate package.
    - On the package candidate package, click on _"[Publish]"_ and confirm.
