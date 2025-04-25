{ config, lib, pkgs, ... }:

{
  services.thermald.enable = true;

  ## Steps to determine settings:
  ##
  ## - Run `sudo powertop`
  ## - Go to the "Tunables" tab and copy pasta into LLM
  ## - Ask for the settings to turn all Bad -> Good
  ##
  ## This left 3 usb devices in bad state, but they were all marked as
  ## "on" instead of "auto" when running this command, suggesting the
  ## kernel has decided they shouldn't suspend:
  ##
  ## grep . /sys/bus/usb/devices/*/power/control

  services.tlp = {
    enable = true;
    settings = {
      # Enable USB autosuspend
      USB_AUTOSUSPEND = 1;

      # CPU power management settings
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      # PCI Runtime Power Management
      RUNTIME_PM_ON_AC = "auto";
      RUNTIME_PM_ON_BAT = "auto";

      # GPU power management
      RADEON_DPM_STATE_ON_AC = "performance";
      RADEON_DPM_STATE_ON_BAT = "battery";

      # NVMe power management
      AHCI_RUNTIME_PM_ON_AC = "on";
      AHCI_RUNTIME_PM_ON_BAT = "auto";
    };
  };

  environment.systemPackages = with pkgs; [
    acpi
    powertop
  ];

  services.power-profiles-daemon.enable = false; # we prefer tlp over gnome's manager
}