pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    lean4 # lean, lake, leanc (~4.x from nixpkgs)
  ];
}
