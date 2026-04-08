{pkgs, all-cabal-hashes, ...}: let 
  haskellPackages = pkgs.haskell.packages.ghc9103.override {
    inherit all-cabal-hashes;
    overrides = import ./overlay.nix { hlib = pkgs.haskell.lib.compose; };
  };
in rec {
  inherit (haskellPackages) oddball eventlog-live eventlog-live-otelcol;
  eventlog-live-otelcol-control = eventlog-live-otelcol.override { withControl = true; };
}
