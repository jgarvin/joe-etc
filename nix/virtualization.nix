{ config, lib, pkgs, options, ... }:

{
  virtualisation = {
    # normal settings go here
    libvirtd.enable = true;
  } // lib.optionalAttrs (options.virtualisation ? cores) {
    # settings that only apply when using `nixos-rebuild build-vm` go
    # here
    memorySize = 4096;
    cores = 2;
    diskSize = 40960;
  };

  users.users.prophet.extraGroups = [ "libvirtd" ];

  environment.systemPackages = with pkgs; [
    virt-manager
    qemu
  ];
}
