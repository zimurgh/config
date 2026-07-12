pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    go
    gopls
    delve
    gotools
    pkg-config
  ];
}
