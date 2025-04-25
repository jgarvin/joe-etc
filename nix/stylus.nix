{ config, lib, pkgs, ... }:

{
  services.libinput.enable = true;
  services.xserver.wacom.enable = true;
}
