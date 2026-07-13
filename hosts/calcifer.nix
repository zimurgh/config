{ lib, ... }:

{
  imports = [ ./calcifer/hardware-configuration.nix ];

  networking.hostName = "calcifer";

  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";
  networking.wireless.iwd = {
    enable = true;
    settings = {
      Settings = {
        AutoConnect = true;
      };
    };
  };

  users.users.michael.extraGroups = lib.mkAfter [ "networkmanager" ];

  services.power-profiles-daemon.enable = true;
  services.upower.enable = true;
}
