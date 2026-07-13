{ config, lib, pkgs, inputs, ... }:

let
  rust = import ./rust.nix {
    inherit pkgs;
    fenixPkgs = inputs.fenix.packages.${pkgs.stdenv.hostPlatform.system};
  };
in

{
  imports = [
    inputs.nixvim.homeModules.nixvim
    inputs.niri.homeModules.niri
    ./noctalia.nix
    ./nvim.nix
  ];
  home.username = "michael";
  home.homeDirectory = "/home/michael";
  home.stateVersion = "26.05";

  home.sessionVariables = {
    RUST_SRC_PATH = rust.rustSrcPath;
    JAVA_HOME = "${pkgs.jdk25.home}";
    OPENSSL_NO_VENDOR = "1";
  };

  home.activation.removeLegacyNvimConfigLink =
    lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
      if [ -L "$HOME/.config/nvim" ]; then
        rm "$HOME/.config/nvim"
      fi
    '';

  home.packages = with pkgs; [
    htop
    fortune
    rust.toolchain
    rust.analyzer
    jdk25
    jdt-language-server
    google-java-format
    vscode-extensions.vscjava.vscode-java-debug
    vscode-extensions.vscjava.vscode-java-test
    vscode-extensions.vadimcn.vscode-lldb.adapter
    postgresql
    mariadb
    tree-sitter
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
