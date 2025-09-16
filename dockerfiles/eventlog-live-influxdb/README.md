# Dockerfiles for `eventlog-live-influxdb`

This directory contains example containers for use with `eventlog-live-influxdb`.

The `docker-compose.yml` file contains a self-contained demo using the `oddball` example program.

The `docker-compose-external.yml` file contains the applications needed to store and visualise eventlog data from an external program. This requires running the external program side-by-side with `eventlog-live-influxdb` as is done in, e.g., the `oddball-influxdb.sh` script.
