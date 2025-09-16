# Dockerfiles for `eventlog-live-honeycomb`

This directory contains example containers for use with `eventlog-live-honeycomb`.

The `docker-compose.yml` file contains a self-contained demo using the `oddball` example program.

The `docker-compose-external.yml` file contains the applications needed to forward the eventlog data from an external program to Honeycomb. This requires running the external program side-by-side with `eventlog-live-otelcol` as is done in, e.g., the `oddball-otelcol-hT.sh` script. Furthermore, this requires that you create an `.env` file that defines the `X_HONEYCOMB_TEAM` and `X_HONEYCOMB_DATASET` variables.
