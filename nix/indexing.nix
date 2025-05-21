{ config, lib, pkgs, inputs, ... }:

let
  # Create the update script as a package
  update-nix-index = pkgs.writeShellScriptBin "update-nix-index" ''
    #!/bin/sh
    set -eu

    echo "Updating nix-index database..."

    # Use the default nix-index database location
    DEST_DIR="$HOME/.cache/nix-index"
    mkdir -p "$DEST_DIR"

    # Run nix-index to generate the database
    # This automatically indexes the packages from the Nix search path
    nix-index

    echo "Nix-index database updated successfully."
  '';
in
{
  # Enable nix-index in Home Manager
  programs.nix-index = {
    enable = true;
  };

  # Create a systemd user service
  systemd.user.services.nix-index-update = {
    Unit = {
      Description = "Update nix-index database";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${update-nix-index}/bin/update-nix-index";
      Environment = [
        "PATH=${lib.makeBinPath [pkgs.nix-index pkgs.coreutils pkgs.bash]}:$PATH"
      ];
    };
  };

  # Create a systemd user timer
  systemd.user.timers.nix-index-update = {
    Unit = {
      Description = "Periodically update nix-index database";
    };
    Timer = {
      OnCalendar = "weekly";
      Persistent = true;
      RandomizedDelaySec = "1h";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };

  # Ensure the nix-index package and our update script are installed
  home.packages = with pkgs; [
    nix-index
    update-nix-index
    nix-search # searches the nixos.org
  ];

  # nps is another way to search, seems to work better
  systemd.user.timers."refresh-nps-cache" = {
    Install = {
      WantedBy = [ "timers.target" ];
    };
    Timer = {
      OnCalendar = "weekly";
      Persistent = true;
    };
  };

  systemd.user.services."refresh-nps-cache" = {
    Unit = {
      Description = "Refresh nps cache";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.nps}/bin/nps -r -dddd";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}