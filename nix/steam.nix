{ config, lib, pkgs, ... }:

let
  unstable = import <nixos-unstable> { };
in {
  programs.steam.enable = true;

  environment.systemPackages = with pkgs; [
    #unstable.gamescope # flickers like crazy
    gamescope
    mangohud
  ];

  programs.steam.gamescopeSession.enable = true;
  programs.gamemode.enable = true;
}