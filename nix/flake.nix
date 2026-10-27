{
  description = "flake for my-host";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    gdb-src = {
      url = "git+https://sourceware.org/git/binutils-gdb.git";
      flake = false; # Not a flake itself
    };

    # Branch of unmerged tmux PR 5615, which adds kitty keyboard protocol
    # support: https://github.com/tmux/tmux/pull/5615
    #
    # This is what makes Ctrl+Backspace work in Claude Code inside tmux.
    # Released tmux only asks Ghostty for xterm's modifyOtherKeys mode, and
    # in that mode Ghostty sends Ctrl+Backspace as 0x08, the same byte as
    # Ctrl+H. tmux then forwards it as C-h, which Claude Code treats as plain
    # Backspace. With this branch tmux turns on the kitty protocol in
    # Ghostty, which sends Ctrl+Backspace as \e[127;5u, and tmux passes it
    # on as Ctrl+Backspace. Programs that never ask for extended keys, like
    # bash, also get \e[127;5u (because of extended-keys always in
    # tmux.conf); programs.readline in home.nix makes bash understand it.
    #
    # Switch back to nixpkgs' tmux once a release includes kitty support.
    tmux-kitty-src = {
      url = "github:tmux/tmux/kitty-keys-autodetect";
      flake = false;
    };

    # nix package search
    nps.url = "github:OleMussmann/nps";
    nps.inputs.nixpkgs.follows = "nixpkgs";

    nix-index-database.url = "github:nix-community/nix-index-database";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";

    centered-master.url = "path:../sway/centered_master";
    centered-master.inputs.nixpkgs.follows = "nixpkgs";

    llm-agents.url = "github:numtide/llm-agents.nix";
    llm-agents-claude.url = "github:numtide/llm-agents.nix";
    llm-agents-opencode.url = "github:numtide/llm-agents.nix";
  };

  outputs = inputs@{ self, nixpkgs, unstable, gdb-src, home-manager, ... }:
    let
      third-party-packages = final: prev: {
        nps = inputs.nps.packages.${prev.stdenv.hostPlatform.system}.default;
        # other third party flakes could go here
      };
      system = "x86_64-linux";
      pkgs   = import nixpkgs {
        system = "x86_64-linux";
        # cudaSupport must be set here, not in nvidia.nix: nixosSystem receives this pre-built pkgs, so
        # module-level nixpkgs.config would not reach it. Binary cache for CUDA builds is in nvidia.nix.
        config = { allowUnfree = true; cudaSupport = true; };
        overlays = [
          (final: prev: {
            # bees 0.11 added --throttle-factor, which keeps it from
            # saturating Btrfs deferred-work queues. Use unstable until the
            # stable package has that feature, then automatically use stable.
            bees = if prev.lib.versionAtLeast prev.bees.version "0.11"
                   then prev.bees
                   else unstablePkgs.bees;
            # Firefox links onnxruntime for its local ML features. The global
            # cudaSupport flag makes that a CUDA build, which fails to compile
            # and also forces Firefox itself to be rebuilt from source.
            onnxruntime = prev.onnxruntime.override { cudaSupport = false; };
          })
        ];
      };
      unstablePkgs = import unstable {
        inherit system;
        config = { allowUnfree = true; cudaSupport = true; };
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
      # The unstable recipe already builds from the GitHub tree with
      # autoreconf, so only the source needs replacing. The version must be
      # the one the tree reports (e.g. next-3.9) because the recipe's install
      # check requires `tmux -V` to print it.
      tmux-kitty = unstablePkgs.tmux.overrideAttrs (_: {
        pname = "tmux-kitty";
        version = builtins.head (builtins.match
          ".*AC_INIT[(][[]tmux[]], *([^)]+)[)].*"
          (builtins.readFile "${inputs.tmux-kitty-src}/configure.ac"));
        src = inputs.tmux-kitty-src;
      });
    in {
      nixosConfigurations.eruv2 = nixpkgs.lib.nixosSystem {
        inherit system pkgs;
        specialArgs = {
          inherit inputs;
          inherit unstablePkgs;
          inherit gdb-git;
          inherit tmux-kitty;
        };
        modules = [
          ./configuration.nix
          inputs.nix-index-database.nixosModules.default
          ({ config, pkgs, ... }: { nixpkgs.overlays = [
                                      third-party-packages
                                    ]; })
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
