{ config, lib, pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # sway stuff
    grim # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    mako # notification system developed by swaywm maintainer
    waybar
    wlr-randr
    kitty # terminal
    blueman # bluetooth applet
    networkmanagerapplet # internet connectivity applet
    udiskie # for auto mounting
  ];

  # enable Sway window manager
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  services.greetd = let
    sway-nvidia-wrapper = pkgs.writeShellScriptBin "sway-nvidia" ''
    exec ${pkgs.sway}/bin/sway --unsupported-gpu "$@"
  '';
  in {
    enable = true;
    settings = {
      default_session = {
        # sway needs the `--unsupported-gpu` whenever you use nvidia proprietary drivers
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd ${sway-nvidia-wrapper}/bin/sway-nvidia";
        user = "greeter";
      };
    };
  };

  fonts.packages = with pkgs; [
    font-awesome # contains waybar icons
  ];
}
