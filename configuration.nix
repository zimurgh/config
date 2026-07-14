{ config, pkgs, inputs, ... }:

{
  imports = [
    # ./databases.nix
  ];

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
    extraSpecialArgs = { inherit inputs; };
    users.michael = import ./home.nix;
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.networkmanager.enable = true;

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
    extraGroups = [ "wheel" ];
    shell = pkgs.nushell;
    packages = with pkgs; [];
  };

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    eza
    fd
    pciutils
    mesa-demos # glxinfo, eglinfo, etc.
    qemu

    # ASM
    nasm
    binutils
    gcc

    # C/C++
    clang-tools
    
    # Nix
    nil



    ghidra
    nmap

    # Devtools
    git
    helix
    btop
    gdb
    bpftrace
    nushell
    wget
    starship
    yazi
    ripgrep
    unzip
    gnuplot
    curl

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
    csound
    code-cursor
  ];

  fonts.packages = with pkgs; [
    font-awesome
    nerd-fonts.fira-code
    corefonts # Onlyoffice
  ];

  programs.thunderbird.enable = true;
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
    package = pkgs.steam.override {
      # -system-composer alone still black on Niri + xwayland-satellite + Intel iGPU
      extraArgs = "-cef-disable-gpu-compositing";
    };
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

  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    substituters = [
      "https://noctalia.cachix.org"
      "https://fenix.cachix.org"
      "https://cache.nixos.org"
    ];
    trusted-public-keys = [
      "noctalia.cachix.org-1:pCOR47nnMEo5thcxNDtzWpOxNFQsBRglJzxWPp3dkU4="
      "fenix.cachix.org-1:2si4EVhRc/a5SVoymnQIyHck3Gtik+pkEUA1K338sqE="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbJsOco2x5NxHjr9c/acys/vT0="
    ];
  };
  system.stateVersion = "26.05"; 
}
