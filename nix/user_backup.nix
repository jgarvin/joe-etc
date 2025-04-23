{ config, lib, pkgs, ... }:

{
  systemd.user = {
    services.tarsnapper = {
      Unit = {
        Description = "Run tarsnapper backup script";
      };
      Service = {
        Type = "oneshot";
        ExecStart = "${pkgs.bash}/bin/bash /home/prophet/etc/tarsnapper.sh";
        Environment = "PATH=${lib.makeBinPath [ pkgs.tarsnapper pkgs.coreutils pkgs.bash ]}";
      };
    };
    timers.tarsnapper = {
      Unit = {
        Description = "Timer for tarsnapper backup";
      };
      Timer = {
        OnCalendar = "03:00";
        Persistent = true; # Run immediately if last run was missed
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };

    # TODO: try lifting this into a script to get it to work
    services.ssc-backup = let
      ssc-backup-script = pkgs.writeShellScript "ssc-backup-script" ''
    #! ${pkgs.bash}/bin/bash
    cp -r /home/prophet/ssc /backups/ssc_$(date +%Y%m%d_%H%M%S)
  '';
    in {
      Unit = {
        Description = "Backup ssc directory";
      };
      Service = {
        Type = "oneshot";
        ExecStart = ssc-backup-script;
        Environment = "PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.bash ]}";
      };
    };
    timers.ssc-backup = {
      Unit = {
        Description = "Timer for ssc backup";
      };
      Timer = {
        OnCalendar = "03:00";
        Persistent = true;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };

    services.etc-backup = let
      etc-backup-script = pkgs.writeShellScript "etc-backup-script" ''
    #! ${pkgs.bash}/bin/bash
    cp -r /home/prophet/etc /backups/etc_$(date +%Y%m%d_%H%M%S)
  '';
    in {
      Unit = {
        Description = "Backup etc directory";
      };
      Service = {
        Type = "oneshot";
        ExecStart = etc-backup-script;
        Environment = "PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.bash ]}";
      };
    };
    timers.etc-backup = {
      Unit = {
        Description = "Timer for etc backup";
      };
      Timer = {
        OnCalendar = "03:00";
        Persistent = true;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };

    services.ssc-remote-backup = let
      ssc-remote-backup-script = pkgs.writeShellScript "ssc-remote-backup-script" ''
    #! ${pkgs.bash}/bin/bash
    tar czf - -C ~ ssc | ssh teamslice@sliceserv "cat > /home/teamslice/backups/ssc_$(date +%d_%m_%y_%s).tar.gz"
'';
    in {
      Unit = {
        Description = "Remote backup of ssc directory";
      };
      Service = {
        Type = "oneshot";
        ExecStart = ssc-remote-backup-script;
        Environment = "PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.bash ]}";
      };
    };
    timers.ssc-remote-backup = {
      Unit = {
        Description = "Timer for remote ssc backup";
      };
      Timer = {
        OnCalendar = "02:00";
        Persistent = true;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
}