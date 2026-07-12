#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
nixos_dir="$repo_root/nixos"

sudo cp "$nixos_dir/configuration.nix" /etc/nixos/configuration.nix
sudo cp "$nixos_dir/home.nix" /etc/nixos/home.nix
sudo cp "$nixos_dir/flake.nix" /etc/nixos/flake.nix
sudo cp "$nixos_dir/flake.lock" /etc/nixos/flake.lock
for module in \
  rust \
  java \
  node \
  haskell \
  python \
  zig \
  typst \
  go \
  lua \
  julia \
  cpp \
  noctalia
do
  sudo cp "$nixos_dir/$module.nix" "/etc/nixos/$module.nix"
done
sudo mkdir -p /etc/nixos/config
sudo cp -r "$nixos_dir/config/." /etc/nixos/config/
sudo nixos-rebuild switch --flake /etc/nixos#calcifer
