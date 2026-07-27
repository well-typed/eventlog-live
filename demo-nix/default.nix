{ pkgs, all-cabal-hashes, ... }:
let
  haskellPackages = pkgs.haskell.packages.ghc9103.override {
    inherit all-cabal-hashes;
    overrides = pkgs.callPackage ./overlay.nix { };
  };
in
rec {
  inherit (haskellPackages) oddball eventlog-live;
  eventlog-live-control = eventlog-live.override { withControl = true; };
}
