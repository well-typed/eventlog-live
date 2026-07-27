{
  withControl ? false,
  callCabal2nixWithOptions,
  lib,
  ...
}:
callCabal2nixWithOptions "eventlog-live" (lib.cleanSource ../eventlog-live)
  (lib.optionalString withControl "-fcontrol")
  { }
