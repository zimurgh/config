{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
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

  services.xserver.xkb = {
    layout = "us";
    variant = "dvp";
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
    git
    vim
    helix
    btop
    bpftrace
    nushell
    wget
    rustc
    cargo
    zig
    neovim
    yazi
    ripgrep
    gradle
    typst
    jdk25
    alacritty
    kitty
    fuzzel
    waybar
    swaylock
    brave
  ];

  fonts.packages = with pkgs; [
    font-awesome_4
  ];

  programs.java.enable = true;
  programs.java.package = pkgs.jdk25;

  programs.niri.enable = true;

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

  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };
  
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "26.05"; # Did you read the comment?

}
