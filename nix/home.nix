{ config, pkgs, ... }:

{
  imports =
    [
      ./theme.nix
    ];

  # TODO please change the username & home directory to your own
  home.username = "prophet";
  home.homeDirectory = "/home/prophet";

  # Packages that should be installed to the user profile.
  home.packages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    htop
    git
    lshw
    silver-searcher
    zsh
    gcc14
    emacs-pgtk
    acpi # needed for waybar battery status?
    jq
    mesa-demos # glxgears
    vulkan-tools # vkcube
    xorg.xeyes
    xterm
    killall
    gnumake
    cmake
    libtool
    gdb
    valgrind
    linuxPackages_latest.perf # we can't easily inspect the one we're booting due to purity :(
    google-chrome
  ];

  # still managing these manually for now
  programs.bash.enable = false;
  programs.zsh.enable = false;

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}
