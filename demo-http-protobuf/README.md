# Dockerfiles for `eventlog-live-otelcol`

This directory contains example containers for use with `eventlog-live-otelcol`.

The `docker-compose.yml` file contains a self-contained demo using the `oddball` example program.

The `docker-compose-external.yml` file contains the applications needed to store and visualise eventlog data from an external program.
This requires running the external program side-by-side with `eventlog-live-otelcol` as is done in, e.g., `examples/oddball/oddball-otelcol-with-hT.sh`.

> [!NOTE]
> These Docker Compose files pin an older version of Tempo, as the current version does not appear to respect `target: all`.
> Unfortunately, `grafana/tempo` does not pin versions, but only pins commits _with_ the architecture of the image.
> If you're on a machine with a x86_64 or AMD64 architecture, the default provided in the files works for you.
> If you're on a machine with an ARM64 or AArch64 architecture, you should run these Docker Compose files with `ARCH=arm64`.
