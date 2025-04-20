{ config, lib, pkgs, ... }:

{
  programs.thunar.enable = true;

  # without this preference changes will be discarded unless you're
  # running the xfce desktop
  programs.xfconf.enable = true;

  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin # handle zip etc
    thunar-volman # handle removable media
  ];
  services.gvfs.enable = true; # Mount, trash, and other functionalities
  services.tumbler.enable = true; # Thumbnail support for images
}