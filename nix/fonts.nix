{ config, lib, pkgs, ... }:

{
  fonts.packages = with pkgs; [
    nerdfonts # install alllllll the fonts
    iosevka # my preferred font
  ];

  # Make sure apps have access to fonts
  fonts.fontconfig.enable = true;
}
