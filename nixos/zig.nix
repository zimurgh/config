pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    zig
    zls
  ];
}
