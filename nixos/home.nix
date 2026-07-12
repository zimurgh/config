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
    mpc
  ];


  programs.home-manager.enable = true;
  programs.emacs.enable = true;

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };

  services.mpd = {
    enable = true;
    network.listenAddress = "127.0.0.1";
    network.port = 6600;
    extraConfig = ''
      audio_output {
        type "pipewire"
        name "PipeWire Output"
      }
    '';
  };

  services.mpd-mpris.enable = true;

  programs.ncmpcpp = {
    enable = true;
    settings = {
      ncmpcpp_directory = "${config.xdg.dataHome}/ncmpcpp";
      mpd_host = "127.0.0.1";
      mpd_port = 6600;
      ignore_leading_the = true;
    };
  };

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
