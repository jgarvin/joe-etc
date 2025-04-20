{ config, lib, pkgs, ... }:

{
  programs.steam.enable = true;

  environment.systemPackages = with pkgs; [
    gamescope
    mangohud
  ];

  programs.steam.gamescopeSession.enable = true;
  programs.gamemode.enable = true;
}