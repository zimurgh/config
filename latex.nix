pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    texlive.combined.scheme-medium
    texlab
  ];
}
