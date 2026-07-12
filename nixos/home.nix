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

  home.file."${config.xdg.configHome}/starship.toml".source =
    ./config/starship.toml;

  programs.starship = {
    enable = true;
    enableNushellIntegration = true;
  };

  programs.nushell = {
    enable = true;
    configFile.source = ./config/nushell/config.nu;
    envFile.source = ./config/nushell/env.nu;
  };
}
