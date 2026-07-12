{ config, pkgs, inputs, ... }:
{
  imports = [
    inputs.niri.homeModules.niri
  ];
  home.username = "michael";
  home.homeDirectory = "/home/michael";
  home.stateVersion = "26.05";

  home.packages = with pkgs; [
    htop
    fortune
  ];


  programs.home-manager.enable = true;
  programs.emacs.enable = true;


}
