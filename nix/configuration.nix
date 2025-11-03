{ config, lib, pkgs, unstablePkgs, ... }:

let my_kernel = pkgs.linuxPackages_6_17;
in {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./asus.nix
      ./nvidia.nix
      # ./disable_nvidia.nix
      ./amd_radeon.nix
       ./sway.nix
      #./i3.nix
      #./gnome.nix
      ./fonts.nix
      ./thunar.nix
      ./btrfs.nix
      ./steam.nix
      ./tailscale.nix
      ./power.nix
      ./stylus.nix
      ./printing.nix
      ./hugepages.nix
      ./debugging.nix
      ./bluetooth.nix
      ./virtualization.nix
      ./perf.nix
    ];

  # don't let things accumulate forever
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 45d";
  };

  # services.ollama = {
  #   enable = true;
  #   acceleration = "cuda";  # or "rocm" for AMD, or remove for CPU only
  #   package = unstablePkgs.ollama;
  # };

  # my syslog was totally spammed with this, so lets kill the
  # messenger and naively hope for the best
  boot.kernelParams = [
    "split_lock_detect=off"
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # this is the latest though
  boot.kernelPackages = my_kernel;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 25;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "eruv2";

  networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Enable sound.
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.prophet = {
    isNormalUser = true;
    extraGroups = [
      "input" # allow reading /dev/input, waybar needs for keyboard-state
      "networkmanager" # allow changing wifi
      "wheel"  # Enable ‘sudo’ for the user.
      "bluetooth" # allow using bluetooth
      "lp" # also for bluetooth for historical reasons
    ];
    packages = with pkgs; [
      tree
    ];
  };

  # Provides desktop integrations like file chooser dialogs, power
  # management requests, etc.
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr # for sway/wl-roots compositors
      xdg-desktop-portal-gtk
      kdePackages.xdg-desktop-portal-kde
    ];
  };

  programs.firefox.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    htop
    git
    lshw
    silver-searcher
    zsh
    gcc14
    llvmPackages_19.clang
    llvmPackages_19.llvm # w/o this clang lto doesn't work
    emacs
    dmidecode
    jq
    killall
    gnumake
    cmake
    libtool
    pciutils # lspci
    gdb
    lldb
    valgrind
    my_kernel.perf
    google-chrome
    tarsnap
    tarsnapper
    pstree
    man-pages
    man-pages-posix
    nps # this is injected into pkgs by an overlay
  ];

  # This sets up the udev rules that are needed by vial/keybard
  hardware.keyboard.qmk.enable = true;

  # install man pages for everything we install
  documentation.enable = true;
  documentation.man.enable = true;

  # Enable the gnome-keyring secrets vault.
  # Will be exposed through DBus to programs willing to store secrets.
  services.gnome.gnome-keyring.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # tool for firmware updates: https://nixos.wiki/wiki/Fwupd
  services.fwupd.enable = true;

  # need for waybar battery status?
  services.upower.enable = true;

  services.dbus.enable = true;

  # Create a directory for some backups that is outside /home so that
  # if I accidentally blow away home with a bad find/xargs that I can
  # recover easily.
  systemd.tmpfiles.rules = [
    "d /backups 0700 prophet users - -"
  ];

  security.sudo.extraConfig = ''
  # Set sudo timeout to 30 minutes
  Defaults        timestamp_timeout=30
'';

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?
}
