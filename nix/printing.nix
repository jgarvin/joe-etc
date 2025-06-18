{ config, lib, pkgs, ... }:

{
  # Enable CUPS to print documents.
  services.printing.enable = true;

  # install epson drivers
  services.printing.drivers = with pkgs; [
    epson-escpr
    epson-escpr2
    gutenprint
    gutenprintBin
  ];

  # setup printing by nework
  services.printing.browsing = true;
  services.printing.defaultShared = true;

  # Enable discovery of network printers.
  # go to http://localhost:631 to configure
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
}