{ config, lib, pkgs, options, ... }:

{
  virtualisation.libvirtd.enable = true;

  users.users.prophet = {
    extraGroups = [ "libvirtd" ];
  };

  environment.systemPackages = with pkgs; [
    virt-manager
    qemu
  ];

  virtualisation.vmVariant = {
    users.users.root.password = "vm";
    users.users.prophet.password = "vm";
    virtualisation = {
      memorySize = 4096;
      diskSize = 40960;
      cores = 2;

      forwardPorts = [{
        from = "host";
        host.port = 2222;
        guest.port = 22;
      }];
    };

    virtualisation.qemu.options = [
      # use the gpu
      "-vga none"
      "-device virtio-vga-gl"
      "-display gtk,gl=on"
      # Wire up pipewire audio
      "-audiodev pipewire,id=audio0"
      "-device intel-hda"
      "-device hda-output,audiodev=audio0"
    ];

    services.tlp = lib.mkForce {
      enable = false;
    };
    services.thermald = lib.mkForce {
      enable = false;
    };

    # different hostname so we don't pick up the same sway output
    # config
    networking.hostName = lib.mkVMOverride "eruv2vm";

    environment.sessionVariables = lib.mkVMOverride {
      # prevent upside down cursor
      WLR_NO_HARDWARE_CURSORS = "1";
      SWAY_IS_VM = "1";
    };

    services.greetd.settings.default_session.command = lib.mkForce "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd ${pkgs.sway}/bin/sway";

    # stuff to get copy/paste working? still doesn't
    services.spice-vdagentd.enable = true;

    # # Disable your normal Wayland/Sway config
    # services.xserver.displayManager.sddm.wayland.enable = lib.mkForce  false;
    # programs.sway.enable = lib.mkForce false;

    # services.greetd.enable = lib.mkForce false;

    # # Enable X11 with XFCE
    # services.xserver = {
    #   enable = true;
    #   displayManager.lightdm.enable = true;
    #   desktopManager.xfce.enable = true;
    # };

    programs.steam.enable = lib.mkForce false;
  };
}
