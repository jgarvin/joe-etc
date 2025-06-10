{ config, lib, pkgs, ... }:

{
  fonts.packages = with pkgs; [
    nerdfonts # install alllllll the fonts
    iosevka # my preferred font
    symbola # for emacs emojis
  ];

  # Make sure apps have access to fonts
  fonts.fontconfig.enable = true;
}
