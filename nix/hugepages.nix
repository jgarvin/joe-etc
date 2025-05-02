{ config, lib, pkgs, ... }:

{
  boot.kernel.sysctl = {
    "vm.nr_hugepages" = 512;  # Adjust the number based on your needs
  };

  fileSystems."/dev/hugepages" = {
    device = "hugetlbfs";
    fsType = "hugetlbfs";
    options = [ "mode=1770" "gid=100" ];  # 100 is typically the GID for the "users" group
  };
}
