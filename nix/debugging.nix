{ config, lib, pkgs, ... }:

{
  programs.bcc.enable = true;
  programs.systemtap.enable = true;

  environment.systemPackages = with pkgs; [
    gdb
    lldb
    valgrind
  ];

  # allow ptrace of non-child pids
  boot.kernel.sysctl."kernel.yama.ptrace_scope" = 0;
}
