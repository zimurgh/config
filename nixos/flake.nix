{
  description = "My NixOS configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    niri = {
      url = "github:sodiboo/niri-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, niri, fenix, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      fenixPkgs = fenix.packages.${system};
      rust = import ./rust.nix fenixPkgs;
    in
    {
      nixosConfigurations.calcifer = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit inputs; };
        modules = [
          home-manager.nixosModules.home-manager
          ./configuration.nix
        ];
      };

      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          rust.toolchain
          rust.analyzer
          pkg-config
          openssl
          clang
          lldb
          taplo
        ];

        shellHook = ''
          export RUST_SRC_PATH="${rust.rustSrcPath}"
          export OPENSSL_NO_VENDOR=1
        '';
      };
    };
}
