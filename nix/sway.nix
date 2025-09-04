{ config, lib, pkgs, ... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # sway stuff
    grim # screenshot functionality
    sway-contrib.grimshot # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    mako # notification system developed by swaywm maintainer
    waybar
    wlr-randr
    kitty # terminal
    blueman # bluetooth applet
    networkmanagerapplet # internet connectivity applet
    udiskie # for auto mounting
    pciutils # for lspci
    pavucontrol # for clicking on sound icon in waybar
    wdisplays # to configure monitor layout
  ];

  # enable Sway window manager
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  services.greetd = let
    # Set WLR_DRM_DEVICES to specifically be the amd card so sway
    # doesn't try to use nvidia. we use the path with an explicit
    # pci-e address rather than /dev/dri/cardN because that numbering
    # can change across boots.
    #
    # To figure out the pci address for the card use `lspci | grep -i radeon`
    sway-nvidia-wrapper = pkgs.writeShellScriptBin "sway-nvidia" ''
    WLR_DRM_DEVICES=$(realpath /dev/dri/by-path/pci-0000:c5:00.0-card) exec ${pkgs.sway}/bin/sway --unsupported-gpu "$@"
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
