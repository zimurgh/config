#!/usr/bin/env bash
set -euo pipefail

host="${1:-calcifer}"

repo_root="$(cd "$(dirname "$0")" && pwd)"
nixos_dir="$repo_root"

if [ ! -f "$nixos_dir/hosts/${host}.nix" ]; then
  echo "Unknown host: ${host}" >&2
  echo "Available hosts:" >&2
  ls "$nixos_dir/hosts/"*.nix 2>/dev/null | xargs -n1 basename | sed 's/.nix$//' >&2
  exit 1
fi

sudo cp "$nixos_dir/configuration.nix" /etc/nixos/configuration.nix
sudo cp "$nixos_dir/databases.nix" /etc/nixos/databases.nix
sudo cp "$nixos_dir/home.nix" /etc/nixos/home.nix
sudo cp "$nixos_dir/nvim.nix" /etc/nixos/nvim.nix
sudo cp "$nixos_dir/flake.nix" /etc/nixos/flake.nix
sudo cp "$nixos_dir/flake.lock" /etc/nixos/flake.lock

sudo mkdir -p "/etc/nixos/hosts/${host}"
sudo cp "$nixos_dir/hosts/${host}.nix" /etc/nixos/hosts/
if [ -d "$nixos_dir/hosts/${host}" ]; then
  sudo cp -r "$nixos_dir/hosts/${host}/." "/etc/nixos/hosts/${host}/"
fi

for module in \
  rust \
  java \
  node \
  haskell \
  python \
  zig \
  typst \
  latex \
  go \
  lua \
  julia \
  cpp \
  fortran \
  agda \
  odin \
  noctalia \
  nix
do
  sudo cp "$nixos_dir/$module.nix" "/etc/nixos/$module.nix"
done
sudo rm -rf /etc/nixos/config
sudo mkdir -p /etc/nixos/config
sudo cp -r "$nixos_dir/config/." /etc/nixos/config/
sudo nixos-rebuild switch --flake "/etc/nixos#${host}"
