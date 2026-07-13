pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    odin
  ];

  shellHook = ''
    export ODIN_ROOT="${pkgs.odin}/share"
  '';
}
