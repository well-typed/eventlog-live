# Contributing to `eventlog-live-viewer`

## Publishing a release for `eventlog-live-viewer`

To publish the latest version to the source tree for `eventlog-live`, run:

```sh
npm run export
```

This builds the viewer and copies the files to `eventlog-live/data/viewer/`,
which will be included in subsequent source distributions for `eventlog-live`.

Please check that all generated files are included in `extra-source-files`.

## Developing `eventlog-live-viewer`

When developing `eventlog-live-viewer`, use one of the examples for the viewer
together with `npm run watch`.
For example, run the following command from the repository root:

```sh
./examples/oddball/oddball-with-viewer.sh
```

This starts `eventlog-live` for `oddball` and starts the viewer websocket on
<ws://localhost:30179>. It also starts the viewer server on that address, but
we won't be using that.

Run the following command from `eventlog-live-viewer/` in a separate shell:

```sh
npm run watch
```

This starts a viewer server that automatically rebuilds and reloads when the
source is changed. It servers the viewer on <http://localhost:4321/>.
