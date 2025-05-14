{ config, lib, pkgs, unstablePkgs, ... }:

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
    unstablePkgs.gamescope # flickers like crazy, TODO: switch back
    # gamescope
    mangohud

    # steam streaming
    sunshine
    moonlight-qt
  ];

  programs.steam.gamescopeSession.enable = true;
  programs.gamemode.enable = true;

  # for streaming
  # https://www.reddit.com/r/NixOS/comments/1bq2bx4/beginners_guide_to_sunshine_gamedesktop_streaming/
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 47984 47989 47990 48010 ];
    allowedUDPPortRanges = [
      { from = 47998; to = 48000; }
    ];
  };
}