{ config, lib, pkgs, ... }:

# let
  # unstable = import <nixos-unstable> { };
# in
{
  # # Someone here says that this branch combined with
  # #    SDL_VIDEODRIVER=x11 gamescope --backend sdl
  # # made it work for them, but no dice for me
  # # https://github.com/ValveSoftware/gamescope/issues/1669
  # nixpkgs.overlays = [
  #   (final: prev: {
  #     gamescope = prev.gamescope.overrideAttrs (old: {
  #       src = prev.fetchFromGitHub {
  #         owner = "bazzite-org";
  #         repo = "gamescope";
  #         rev = "12e581596778288b9a1b1f0239bd34c9e0c283b9";
  #         fetchSubmodules = true;
  #         sha256 = "sha256-0B5inHglr0FbevSfc6ok6w1p3IXNwTZA0wUoIZ+g1Rs=";
  #       };
  #       buildInputs = (old.buildInputs or []) ++ [ prev.luajit ];
  #       postPatch = old.postPatch + ''
  #         patchShebangs default_scripts_install.sh
  #       '';
  #     });
  #   })
  # ];

  programs.steam.enable = true;

  environment.systemPackages = with pkgs; [
    #unstable.gamescope # flickers like crazy
    gamescope
    mangohud
  ];

  programs.steam.gamescopeSession.enable = true;
  programs.gamemode.enable = true;
}