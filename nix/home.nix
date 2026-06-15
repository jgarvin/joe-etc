{ config, lib, pkgs, inputs, unstablePkgs, ... }:

{
  imports =
    [
      ./theme.nix
      ./user_backup.nix
      ./indexing.nix
      ./lean.nix
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
    # gdb
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
    unstablePkgs.signal-desktop
    graphviz
    python314
    dash
    rc
    zsh
    parallel
    tokei

    # webcam stuff
    mpv
    v4l-utils
    cheese
    gst_all_1.gstreamer
    guvcview
    zoom-us

    audacity

    unzip

    mermaid-cli     # diagrams
    texliveFull
    mupdf
    ghostscript
    librsvg

    wine

    ghidra
    radare2
    rizin
    cutter

    unstablePkgs.yt-dlp

    ffmpeg-full

    sqlite

    # inputs.centered-master.packages.${pkgs.system}.default
    libreoffice-qt

    # programming
    racket

    inputs.llm-agents.packages.${pkgs.system}.claude-code
    inputs.llm-agents.packages.${pkgs.system}.codex
    inputs.llm-agents.packages.${pkgs.system}.gemini-cli
    inputs.llm-agents.packages.${pkgs.system}.opencode
    inputs.llm-agents.packages.${pkgs.system}.pi

    (unstablePkgs.rustPlatform.buildRustPackage rec {
      pname = "claude-history";
      version = "0.1.46";
      src = unstablePkgs.fetchFromGitHub {
        owner = "raine";
        repo = "claude-history";
        rev = "v${version}";
        hash = "sha256-WzP1tMlzv872baoi00D0GFdflOusMb3qZMP5yxyGXbY=";
      };
      cargoHash = "sha256-wdb0NzREoLtz83V5ZiKJRXgfHaloYvuUelWqm1J6FZ0=";
      doCheck = false; # test failure about line length?
    })
  ];

  # Pi agent settings. Keep the extension package declaration here so Pi can
  # install/load FFF on startup using its supported package mechanism.
  home.file.".pi/agent/settings.json" = {
    force = true;
    text = builtins.toJSON {
      lastChangelogVersion = "0.79.3";
      defaultProvider = "openai-codex";
      defaultModel = "gpt-5.5";
      defaultThinkingLevel = "high";
      npmCommand = [ "${pkgs.nodejs}/bin/npm" ];
      packages = [
        "npm:@ff-labs/pi-fff"
        "npm:@nehlis/pi-effort"
      ];
    };
  };

  # Enable man pages for Home Manager packages
  manual.manpages.enable = true;
  # Add the appropriate man directories to your MANPATH
  programs.man.enable = true;

  programs.nushell.enable = true;

  programs.ghostty = {
    enable = true;
    # scroll-to-bottom was added in Ghostty 1.3.0. Prefer the stable
    # package once it is new enough, otherwise use unstable.
    package =
      if lib.versionAtLeast pkgs.ghostty.version "1.3.0"
      then pkgs.ghostty
      else unstablePkgs.ghostty;

    # Custom theme: Ayu's palette/foreground/cursor with a pure black
    # background. We override via a custom theme rather than a plain
    # `background` setting because home-manager's keyValue formatter
    # writes keys alphabetically, so `theme = "Ayu"` would come after
    # `background = ...` and clobber it.
    themes.AyuBlack = {
      palette = [
        "0=#11151c"
        "1=#ea6c73"
        "2=#7fd962"
        "3=#f9af4f"
        "4=#53bdfa"
        "5=#cda1fa"
        "6=#90e1c6"
        "7=#c7c7c7"
        "8=#686868"
        "9=#f07178"
        "10=#aad94c"
        "11=#ffb454"
        "12=#59c2ff"
        "13=#d2a6ff"
        "14=#95e6cb"
        "15=#ffffff"
      ];
      background = "000000";
      foreground = "bfbdb6";
      cursor-color = "e6b450";
      cursor-text = "000000";
      selection-background = "409fff";
      selection-foreground = "000000";
    };

    settings = {
      # it prints annoying messages about copying to the clipboard
      # that block the prompt
      app-notifications = false;

      font-family = "Iosevka";
      font-size = 11;
      theme = "AyuBlack";

      # Scrollback is configured in bytes and is per terminal surface.
      # 256 MiB gives a much longer history than the 10 MB default.
      scrollback-limit = 268435456;
      scroll-to-bottom = "no-keystroke";

      # Force the classic block terminal cursor at the prompt.
      # Without shell-integration=none, the shell switches the cursor
      # to a beam when at the prompt.
      cursor-style = "block";
      cursor-style-blink = false;
      shell-integration = "none";

      confirm-close-surface = false;

      # Disable Iosevka ligatures (calt drives them; liga/dlig for safety).
      font-feature = [ "-calt" "-liga" "-dlig" ];

      window-padding-x = 10;
      window-padding-y = 10;
    };
  };

  # install vscode but let it do its own extension management instead
  # of managing with nixos
  programs.vscode = {
    enable = true;
    package = let
      vscode-fhs = pkgs.vscode.fhsWithPackages (ps: with ps; [
        rustup
        zlib
        openssl.dev
        pkg-config
        libsecret
        gnome-keyring
      ]);
    in pkgs.symlinkJoin {
      # This is all shenanigans to work around that the only way to
      # let vscode find the OS keyring when running under alternative
      # WMs like sway is to explicitly pass an argument telling it
      # what to use, doesn't seem to respect argv.json.
      name = "vscode-wrapped";
      paths = [ vscode-fhs ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
      rm $out/bin/code
      makeWrapper ${vscode-fhs}/bin/code $out/bin/code \
        --add-flags "--password-store=gnome-libsecret"
    '';
    } // {
      pname = vscode-fhs.pname or "vscode";
      version = vscode-fhs.version or "unknown";
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
