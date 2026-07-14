{ lib, ... }:

{
  imports = [ ./raven/hardware-configuration.nix ];

  networking.hostName = "raven";
}
