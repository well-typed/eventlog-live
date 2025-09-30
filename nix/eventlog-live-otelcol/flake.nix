{
  description = "Eventlog Live NixOS VM Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    all-cabal-hashes.url = "github:commercialhaskell/all-cabal-hashes/hackage";
    all-cabal-hashes.flake = false;
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      all-cabal-hashes,
      nixos-generators,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        systemConfig = {
          inherit system;
          modules = [ ./configuration.nix ];
          specialArgs = {
            all-cabal-hashes = all-cabal-hashes.outPath;
          };
        };
        generator = format: nixos-generators.nixosGenerate (systemConfig // { inherit format; });
        nixosConfig = nixpkgs.lib.nixosSystem systemConfig;
      in
      {
        pkgs = pkgs;
        packages = {
          # Build the VM
          config = systemConfig;
          standalone-vm = generator "qcow";
          vm = nixosConfig.config.system.build.vm;
        };

        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            nixos-rebuild
            qemu
          ];
        };
      }
    );
}
