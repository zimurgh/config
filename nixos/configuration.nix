{ config, pkgs, inputs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

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

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak-programmer";
  };

  users.users."michael" = {
    isNormalUser = true;
    description = "Michael";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.nushell;
    packages = with pkgs; [];
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [

    # ASM
    nasm
    fasm
    yasm
    binutils
    asm-lsp
    gcc

    # Rust
    rustc
    cargo
    taplo
    tokei
    rust-analyzer
    lldb

    # Java
    jdk25
    jdt-language-server

    # C/C++
    clang-tools
    
    # Zig
    zig

    # Typst
    typst
    tinymist

    # Typescript / JS
    nodejs
    vscode-langservers-extracted
    typescript-language-server

    # Lua
    lua-language-server

    # Julia
    julia

    # Nix
    nil

    # Yaml
    yaml-language-server

    # Bash
    bash-language-server

    # Awk
    awk-language-server

    # Wasm/Wat
    wasm-language-tools

    # Web GPU / WGSL
    wgsl-analyzer

    # Lean
    lean

    # Markdown
    marksman

    # Nim
    nimlangserver

    # LaTeX
    texlab 

    # Ruby
    ruby-lsp

    # Fish
    fish-lsp

    # Fortran
    fortls

    # Go
    gopls

    # Terraform
    terraform-ls

    # Erlang
    erlang-language-platform

    # Clojure
    clojure-lsp

    # Crystal
    crystalline

    # Systemd
    systemd-lsp

    ghidra
    nmap

    # Devtools
    git
    vim
    helix
    neovim
    gradle
    btop
    gdb
    bpftrace
    nushell
    wget
    starship
    yazi
    ripgrep
    tokei
    unzip

    # Niri stuff
    niri
    fuzzel
    waybar
    swaylock

    # Desktop Apps    
    alacritty
    kitty
    brave
    bitwig-studio
    xwayland-satellite
    openmw
    renderdoc
    prismlauncher
    onlyoffice-desktopeditors

    code-cursor
  ];

  fonts.packages = with pkgs; [
    font-awesome
    nerd-fonts.fira-code
    corefonts # Onlyoffice
  ];

  programs.thunderbird.enable = true;
  programs.java.enable = true;
  programs.java.package = pkgs.jdk25;
  programs.niri.enable = true;
  programs.wireshark = {
    enable = true;
  };
  
  programs.git = {
    enable = true;
    config = {
      user = {
        name = "mcarpenter";
        email = "mcarpenter.dev@gmail.com";
      };
      init.defaultBranch = "main";
    };
  };

  programs.steam = {
    enable = true;
  };

 
  security.rtkit.enable = true;
 
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  services.xserver.xkb = {
    layout = "us";
    variant = "dvp";
  };

  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };

  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.enable = true;
  };

  services.minecraft-server = {
    enable = true;
    eula = true;
  };

  services.printing.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      PermitRootLogin = "no";
      AllowUsers = [ "michael" ];
      MaxAuthTries = 3;
    };
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 25565 443 80 22 ];
  networking.firewall.allowedUDPPorts = [];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  system.stateVersion = "26.05"; 
}
