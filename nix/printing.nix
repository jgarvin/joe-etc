{ config, lib, pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable discovery of network printers.
  # go to http://localhost:631 to configure
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
}