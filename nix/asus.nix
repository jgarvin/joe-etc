{ config, lib, pkgs, ... }:

{
  services.asusd.enable = true;

  systemd.services.asusd = {
    wantedBy = [ "multi-user.target" ];
  };

  environment.systemPackages = with pkgs; [
    asusctl
  ];
}
