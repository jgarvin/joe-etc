{ config, lib, pkgs, ... }:

{
  services.thermald.enable = true;
  services.tlp.enable = true;
  services.power-profiles-daemon.enable = false; # we prefer tlp over gnome's manager
}