{ config, pkgs, unstablePkgs, ... }:

{
  imports =
    [
      ./theme.nix
      ./user_backup.nix
      ./indexing.nix
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
    direnv
    nix-direnv
    ccls
    bottom
    man-pages
    man-pages-posix
    krita
    hyperfine
    fuzzel
    obsidian
    zathura
  ];

  # Enable man pages for Home Manager packages
  manual.manpages.enable = true;
  # Add the appropriate man directories to your MANPATH
  programs.man.enable = true;

  programs.kitty = {
    enable = true;
    font = {
      name = "Iosevka";
      size = 11;
    };
    settings = {
      # Your existing settings
      cursor_shape = "block";
      shell_integration = "disabled";
      cursor_blink_interval = "0";
      disable_ligatures = "always";
      enable_audio_bell = "no";

      # Window styling to match your Materia theme
      window_padding_width = "10";
    };
  };


  # Enable direnv with nix-direnv integration
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    # Don't enable bash integration since home-manager isn't managing
    # bashrc (yet)
    enableBashIntegration = false;
  };

  systemd.user = {
    # Start new services automatically on switch
    startServices = "sd-switch";
  };

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
