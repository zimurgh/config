pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    typst
    tinymist
  ];
}
