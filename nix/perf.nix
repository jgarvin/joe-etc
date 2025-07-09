{ config, lib, pkgs, unstablePkgs, ... }:

{
  # without this you can't set exclude_kernel and exclude_hv in
  # perf_event_open
  boot.kernel.sysctl = {
    "kernel.perf_event_paranoid" = 1;
  };
}
