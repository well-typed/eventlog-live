{ callCabal2nix, lib, ... }: callCabal2nix "oddball" (lib.cleanSource ../../examples/oddball) { }
