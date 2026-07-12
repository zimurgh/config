{ config, pkgs, inputs, ... }:

let
  rust = import ./rust.nix inputs.fenix.packages.${pkgs.system};
in

{
  imports = [
    inputs.niri.homeModules.niri
  ];
  home.username = "michael";
  home.homeDirectory = "/home/michael";
  home.stateVersion = "26.05";

  home.sessionVariables = {
    RUST_SRC_PATH = rust.rustSrcPath;
    OPENSSL_NO_VENDOR = "1";
  };

  home.packages = with pkgs; [
    htop
    fortune
    rust.toolchain
    rust.analyzer
    taplo
    tokei
    lldb
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
