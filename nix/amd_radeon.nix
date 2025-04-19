{ config, lib, pkgs, ... }:

# let
#   linux-firmware-git = pkgs.stdenv.mkDerivation {
#     name = "linux-firmware-git";

#     # Use fetchzip to get the specific tagged version
#     src = pkgs.fetchzip {
#       url = "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git/snapshot/linux-firmware-20250410.tar.gz";
#       # You'll need to replace this with the actual hash after first attempt
#       hash = "sha256-aQdEl9+7zbNqWSII9hjRuPePvSfWVql5u5TIrGsa+Ao=";
#     };

#     installPhase = ''
#       mkdir -p $out/lib/firmware
#       cp -r * $out/lib/firmware
#       # Remove unnecessary files
#       find $out/lib/firmware -name "WHENCE" -or -name "LICENSE*" -or -name "LICENCE*" | xargs rm -f
#     '';
#   };
# in
{
  # boot.kernelParams = [ "amdgpu.dcdebugmask=0x600" "amdgpu.dc=0" ];
  hardware.enableRedistributableFirmware = true;
  services.xserver.videoDrivers = [ "amdgpu" ];
  hardware.opengl.enable = true;
  # hardware.firmware = [ linux-firmware-git ];
#  hardware.firmware = [ (import <nixos-unstable> {}).linux-firmware ];
#  hardware.firmware = [ pkgs.linux-firmware ];
}
