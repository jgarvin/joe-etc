{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Most projects expect you to have elan which is like rustup, it
    # takes care of downloading the particular toolchain needed by a
    # given project.
    elan
  ];
}