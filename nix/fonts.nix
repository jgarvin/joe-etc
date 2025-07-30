{ config, lib, pkgs, ... }:

{
  fonts.packages = with pkgs; [
    iosevka # my preferred font
    symbola # for emacs emojis
  ]
  # install alllllll the fonts
  ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  # Make sure apps have access to fonts
  fonts.fontconfig.enable = true;
}
