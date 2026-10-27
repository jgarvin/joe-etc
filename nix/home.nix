{ config, lib, pkgs, inputs, unstablePkgs, ... }:

{
  imports =
    [
      ./theme.nix
      ./user_backup.nix
      ./indexing.nix
      ./lean.nix
      ./opencode.nix
    ];

  # TODO please change the username & home directory to your own
  home.username = "prophet";
  home.homeDirectory = "/home/prophet";

  # Expire old Home Manager generations and collect unreferenced store paths.
  services.home-manager.autoExpire = {
    enable = true;
    frequency = "weekly";
    timestamp = "-45 days";
    store.cleanup = true;
  };

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
    xeyes
    xterm
    killall
    gnumake
    cmake
    libtool
    # gdb
    valgrind
    perf
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
    sox

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

    # GPU inference; CUDA builds come from the global cudaSupport flag in flake.nix.
    # Stable's whisper-cpp 1.7.5 builds the ggml CPU/CUDA backend modules but never
    # installs them; unstable sets GGML_BACKEND_DIR so they land in $out/lib.
    unstablePkgs.whisper-cpp

    sqlite

    # inputs.centered-master.packages.${pkgs.stdenv.hostPlatform.system}.default
    libreoffice-qt

    # programming
    racket

    inputs.llm-agents-claude.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
    inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.codex
    inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.gemini-cli
    inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.antigravity-cli
    inputs.llm-agents-opencode.packages.${pkgs.stdenv.hostPlatform.system}.opencode
    inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi
  ];

  # Pi agent settings. Keep extension package declarations here so Pi can
  # install and load them on startup using its supported package mechanism.
  home.file.".pi/agent/settings.json" = {
    force = true;
    text = builtins.toJSON {
      collapseChangelog = true;
      lastChangelogVersion = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi.version;
      defaultProvider = "openai-codex";
      defaultModel = "gpt-5.5";
      defaultThinkingLevel = "high";
      npmCommand = [ "${pkgs.nodejs}/bin/npm" ];
      packages = [
        "npm:@davidroman/pi-loop"
        "npm:@ff-labs/pi-fff"
        "npm:@nehlis/pi-effort"
        "npm:pi-goal@0.1.7"
        "npm:pi-input-history@1.1.0"
        "npm:pi-jev-auto-mode"
        "npm:pi-side-chat"
        "${config.home.homeDirectory}/pi-sanhedrin"
        "${config.home.homeDirectory}/pi-draft-recovery"
        "${config.home.homeDirectory}/pi-model-restore"
      ];
    };
  };

  home.file.".pi/agent/keybindings.json" = {
    force = true;
    text = builtins.toJSON {
      # Keep Ctrl+R for pi-input-history's reverse search.
      "app.session.rename" = "ctrl+shift+r";
      # Use Pi's native deletion actions, retaining their default shortcuts.
      # Ghostty reports these modifiers through the Kitty keyboard protocol.
      "tui.editor.deleteWordBackward" = [ "ctrl+w" "alt+backspace" "ctrl+backspace" ];
      "tui.editor.deleteToLineStart" = [ "ctrl+u" "ctrl+shift+backspace" ];
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

      # Keep independently launched windows in separate processes so a
      # Ghostty crash only takes down the windows owned by that instance.
      gtk-single-instance = false;

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
      font-feature = [
        "-calt"
        #"-liga"
        #"-dlig"
      ];

      window-padding-x = 10;
      window-padding-y = 10;
    };
  };

  # Inside tmux (extended-keys always), Ctrl+Backspace reaches bash as the
  # CSI-u sequence \e[127;5u rather than ^H, so the `stty werase ^H` in
  # shell/bashrc doesn't cover it. Without this binding readline would
  # insert "7;5u" into the line.
  programs.readline = {
    enable = true;
    bindings."\\e[127;5u" = "backward-kill-word";
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

    # All interactive coding agents and their local process trees share these
    # limits. Per-command scopes are placed below this slice by agent_run.sh,
    # so concurrent agents cannot each claim an independent 8 GiB allowance.
    slices.agents = {
      Unit.Description = "Aggregate limits for interactive coding agents";
      Slice = {
        MemoryHigh = "8G";
        MemoryMax = "8G";
        MemorySwapMax = 0;
        CPUQuota = "400%";
        CPUWeight = 10;
        IOWeight = 1;
        TasksMax = 8192;
      };
    };
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
