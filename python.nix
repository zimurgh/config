pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    (python3.withPackages (pythonPackages: with pythonPackages; [
      ipython
      pip
      pytest
      virtualenv
    ]))
    black
    pyright
    ruff
  ];

  shellHook = ''
    export PYTHONNOUSERSITE=1
  '';
}
