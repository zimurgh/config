{ lib, ... }:

{
  imports = [ ./desktop/hardware-configuration.nix ];

  networking.hostName = "desktop";
  networking.useDHCP = lib.mkDefault true;
}
