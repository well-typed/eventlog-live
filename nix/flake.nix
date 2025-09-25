{
  description = "Eventlog Live NixOS VM Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    all-cabal-hashes.url = "github:commercialhaskell/all-cabal-hashes/hackage";
    all-cabal-hashes.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, all-cabal-hashes }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        nixosConfig = nixpkgs.lib.nixosSystem {
           inherit system;
           modules = [
             ./configuration.nix
           ];
           specialArgs = { all-cabal-hashes = all-cabal-hashes.outPath; };
         };
      in
      {
        pkgs = pkgs;
        packages = {
          # Build the VM
          vm = nixosConfig.config.system.build.vm;
        };

        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nixos-rebuild
            qemu
          ];
        };
      });
}
