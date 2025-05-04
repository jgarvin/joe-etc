{ config, lib, pkgs, ... }:

{
  boot.kernel.sysctl = {
    "vm.nr_hugepages" = 512;  # Adjust the number based on your needs
  };

  # /dev/hugepages is managed by systemd, putting it in fileSystems
  # will not work even though that changes /etc/fstab, explainer:
  #
  # "The issue is indeed because /dev/hugepages is an upstream systemd
  # mount unit, which overrules fstab. The reason it’s overruled is
  # because NixOS puts those upstream units in /etc while
  # systemd-fstab-generator puts its fstab-derived units in /run, and
  # /etc overrules /run in systemd logic. Other distros put these
  # upstream mount units in /usr/lib, which /run overrules, so they
  # get the opposite behavior."
  systemd.mounts = [
    {
      what = "hugetables";
      where = "/dev/hugepages";
      options = "mode=1770,gid=100";
    }
  ];
}
