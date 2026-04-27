{ callCabal2nix, lib, ... }: callCabal2nix "eventlog-live" (lib.cleanSource ../eventlog-live) { }
