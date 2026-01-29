{ config, lib, pkgs, gdb-git, unstablePkgs, ... }:

let
  # Single variable to control all core dump size limits
  maxCoreSizeGB = 2;
  # Convert GB to KB for PAM limits (which use KB)
  maxCoreSizeKB = toString (maxCoreSizeGB * 1024 * 1024);
in
{
  # Enable systemd-coredump service
  systemd.coredump.enable = true;

  # Configure coredump behavior
  systemd.coredump.extraConfig = ''
    # Store coredumps persistently in the filesystem
    Storage=external
    # Compress core dumps to save space
    Compress=yes
    # Maximum total storage space for core dumps (5x our max single dump size)
    MaxUse=${toString (maxCoreSizeGB * 5)}G
    # Keep this much space free on the filesystem
    KeepFree=1G
    # Maximum size of a single core dump
    ProcessSizeMax=${toString maxCoreSizeGB}G
  '';

  # Set appropriate resource limits for core dumps
  security.pam.loginLimits = [
    # Set a reasonable core dump size for all users
    { domain = "*"; item = "core"; type = "soft"; value = maxCoreSizeKB; }
    { domain = "*"; item = "core"; type = "hard"; value = maxCoreSizeKB; }
  ];

  # Set systemd default limits for core dumps
  systemd.extraConfig = "DefaultLimitCORE=${toString maxCoreSizeGB}G";

  # Set appropriate kernel parameters
  boot.kernel.sysctl = {
    # Ensure setuid programs can create core dumps (needed for some
    # applications). 2 means that such core dumps are only readable by
    # root, in case they expose sensitive information.
    "fs.suid_dumpable" = 2;
  };

  # Install helpful debugging tools
  environment.systemPackages = with pkgs; [
    gdb-git
    lldb
    valgrind
    bloaty
    lm_sensors
    iotop
    radeontop
    bpftrace
    sysstat
    file # can't believe this isn't default
    lsof # same

    # valgrind needs this in order to find the nixseparatedebuginfod stuff
    (lib.getBin (pkgs.elfutils.override { enableDebuginfod = true; }))
  ];

  programs.bcc.enable = true;
  programs.systemtap.enable = true;

  # install debug symbols for everything
  environment.enableDebugInfo = true;

  # allow ptrace of non-child pids
  boot.kernel.sysctl."kernel.yama.ptrace_scope" = 0;

  # enable nixseparatedebuginfod service that grabs the debug info for
  # any package automagically when you run gdb
  services.nixseparatedebuginfod.enable = true;

  # let me use dmesg as a regular user
  boot.kernel.sysctl = {
    "kernel.dmesg_restrict" = 0;
  };
}
