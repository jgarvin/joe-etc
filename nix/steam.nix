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

  # To setup the webui user/pass:
  # systemctl --user stop sunshine.service
  # sunshine -creds "<new-user>" "<new-pass>"
  # systemctl --user start sunshine.service
  services.sunshine = {
    enable = true;
    openFirewall = true; # optional: opens the Moonlight/Sunshine ports
    capSysAdmin = false; # don't want this under sway
    
    settings = {
      stream_audio = "enabled";
      output_name = 2;
    };

    applications = {
      env = { };
      apps = [
        {
          name = "Foo";
          cmd = "/tmp/foo.sh";
          # If /tmp/foo.sh exits immediately (e.g., just launches something),
          # use `detached = [ "/tmp/foo.sh" ];` instead of `cmd`.
        }
      ];
    };
  };

  security.rtkit.enable = true;      # gets rid of the portal realtime errors

  systemd.user.services.sunshine.serviceConfig = {
    LimitNOFILE = "131072";        # avoids the pulse “Too many open files” failure
  };

  # delete any hand-written user units that would shadow the module
  systemd.user.tmpfiles.rules = [
    "r %h/.config/systemd/user/sunshine.service"
    "r %h/.config/systemd/user/sunshine.service.d/override.conf"
    "R %h/.config/systemd/user/sunshine.service.d"
  ];

  # # for streaming
  # # https://www.reddit.com/r/NixOS/comments/1bq2bx4/beginners_guide_to_sunshine_gamedesktop_streaming/
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;
  services.avahi.openFirewall = true;
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 47984 47989 47990 48010 ];
    allowedUDPPortRanges = [
      { from = 47998; to = 48000; }
    ];
  };

  # systemd.user.services.sunshine = {
  #   description = "Sunshine self-hosted game stream host for Moonlight";
  #   startLimitBurst = 5;
  #   startLimitIntervalSec = 500;
  #   serviceConfig = {
  #     ExecStart = "${pkgs.sunshine}/bin/sunshine";
  #     Restart = "on-failure";
  #     RestartSec = "5s";
  #   };
  # };
}