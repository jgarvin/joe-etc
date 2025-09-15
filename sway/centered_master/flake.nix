{
  description = "centered master for sway/i3 (no flake-utils)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          py = pkgs.python3.withPackages (ps: [ ps.i3ipc ]);
        in {
          default = pkgs.writeShellScriptBin "centered_master" ''
          exec ${py}/bin/python ${./centered_master.py}
        '';
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/centered_master";
        };
      });
    };
}
