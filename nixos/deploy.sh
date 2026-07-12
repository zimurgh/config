sudo cp configuration.nix /etc/nixos/configuration.nix
sudo cp home.nix /etc/nixos/home.nix
sudo cp flake.nix /etc/nixos/flake.nix
sudo nixos-rebuild switch --flake /etc/nixos#calcifer
