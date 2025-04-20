{ config, lib, pkgs, ... }:

{
  # hardware-configuration.nix generation is not smart enough to
  # detect compression and other mount options so we need to specify
  # them here. And we don't want to put in that file because sometimes
  # it gets regenerated.
  fileSystems = {
    "/".options = [ "compress=zstd" ];
    "/home".options = [ "compress=zstd" ];
    "/nix".options = [ "compress=zstd" "noatime" ];
  };

  # Have btrbk regularly take snapshots of /home. These are local disk
  # only, just meant as protection against `rm -rf ~` and the like.
  services.btrbk.instances = {
    "home" = {
      onCalendar = "hourly";
      settings = {
        timestamp_format = "long";
        snapshot_preserve_min = "1w";
        snapshot_preserve = "2w";
        volume = {
          "/" = {
            snapshot_dir = "/snapshots";
            subvolume = "home";
          };
        };
      };
    };
  };

  # Btrbk doesn't create snapshot directories automatically
  systemd.tmpfiles.rules = [
    "d /snapshots 0755 root root"
  ];

  # btrbk needs permission to run certain commands with sudo
  security.sudo = {
    enable = true;
    extraRules = [{
      commands = [
        {
          command = "${pkgs.coreutils-full}/bin/test";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${pkgs.coreutils-full}/bin/readlink";
          options = [ "NOPASSWD" ];
        }
        {
          command = "${pkgs.btrfs-progs}/bin/btrfs";
          options = [ "NOPASSWD" ];
        }
      ];
      users = [ "btrbk" ];
    }];
  };

  # If I ever need to recover:
  # ls -la /snapshots # decide what snapshot I want
  #
  # For a restore of one file:
  # cp -a /snapshots/home.YYYYMMDD-HHMMSS/username/path/to/file /home/username/path/to/file
  #
  # For a complete restore:
  # sudo mv /home /home.broken # if it still exists partially
  # sudo btrfs subvolume snapshot /snapshots/home.YYYYMMDD-HHMMSS /home
}