{ pkgs, ... }:

{
  home.packages = [
    pkgs.nix-search # searches the nixos.org
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
