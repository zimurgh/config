{ lib, ... }:

{
  imports = [ ./anansi/hardware-configuration.nix ];

  networking.hostName = "anansi";
}
