{ config, lib, pkgs, ... }:

{
  fonts.packages = with pkgs; [
    nerdfonts # install alllllll the fonts
    iosevka # my preferred font
  ];
}
