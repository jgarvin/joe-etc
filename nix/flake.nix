{
  description = "flake for my-host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gdb-src = {
      url = "git+https://sourceware.org/git/binutils-gdb.git";
      flake = false; # Not a flake itself
    };

    # nix package search
    nps.url = "github:OleMussmann/nps";
    nps.inputs.nixpkgs.follows = "nixpkgs";

    centered-master.url = "path:../sway/centered_master";
    centered-master.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, unstable, gdb-src, home-manager, ... }:
    let
      third-party-packages = final: prev: {
        nps = inputs.nps.packages.${prev.system}.default;
        # other third party flakes could go here
      };
      system = "x86_64-linux";
      pkgs   = import nixpkgs {
        system = "x86_64-linux";
        config = { allowUnfree = true; };
      };
      unstablePkgs = import unstable {
        inherit system;
        config = { allowUnfree = true; };
      };
      # Added this because gdb fails to access TLS vars on versions
      # before ~April 2025.
      #
      # https://sourceware.org/bugzilla/show_bug.cgi?id=24548
      gdb-git = pkgs.gdb.overrideAttrs (oldAttrs: {
        pname = "gdb-git";
        version = "git";
        src = gdb-src;

        # Fix preConfigure to handle the different directory structure
        # in git
        # Completely replace preConfigure with a simplified version
        preConfigure = ''
          # Skip file removal entirely - those files don't exist in git

          # GDB has to be built out of tree.
          mkdir -p _build
          cd _build
        '';

        # Not sure why this was needed? I'd have expected the existing
        # package to bring these in. Maybe they build from a tarball
        # that already has everything generated?
        nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
          pkgs.bison
          pkgs.flex
          pkgs.texinfo
          pkgs.perl
        ];
      });
    in {
      nixosConfigurations.eruv2 = nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = {
          inherit inputs;
          inherit unstablePkgs;
          inherit gdb-git;
        };
        modules = [
          ./configuration.nix
          ({ config, pkgs, ... }: { nixpkgs.overlays = [ third-party-packages ]; })
          home-manager.nixosModules.home-manager {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.prophet = import ./home.nix;
              extraSpecialArgs = { inherit inputs; inherit unstablePkgs; };
            };
          }
        ];
      };
    };
}
