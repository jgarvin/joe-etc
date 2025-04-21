{
  description = "flake for my-host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ self, nixpkgs, unstable, ... }:
    let
      system = "x86_64-linux";
      pkgs   = import nixpkgs {
        system = "x86_64-linux";
        config = { allowUnfree = true; };
      };
      unstablePkgs = import unstable {
        inherit system;
        config = { allowUnfree = true; };
      };
    in {
      nixosConfigurations.eruv2 = nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = {
          inherit inputs;
          inherit unstablePkgs;
        };
        modules = [
          ./configuration.nix
        ];
      };
    };
}
