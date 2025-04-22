{
  description = "flake for my-host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, unstable, home-manager, ... }:
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
          home-manager.nixosModules.home-manager {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.prophet = import ./home.nix;
            };
          }
        ];
      };
    };
}
