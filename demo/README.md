# Dockerfiles for `eventlog-live-otlp`

This directory contains example containers for use with `eventlog-live-otlp`.

The `docker-compose.yml` file contains a self-contained demo using the `oddball` example program.

The `docker-compose-external.yml` file contains the applications needed to store and visualise eventlog data from an external program.
This requires running the external program side-by-side with `eventlog-live-otlp` as is done in, e.g., `examples/oddball/oddball-otlp-with-hT.sh`.
