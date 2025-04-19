{ config, lib, pkgs, ... }:

{
  services.asusd = {
    enable = true;
    enableUserService = true;
  };

  systemd.services.asusd = {
    wantedBy = [ "multi-user.target" ];
  };

  environment.systemPackages = with pkgs; [
    asusctl
  ];
}
