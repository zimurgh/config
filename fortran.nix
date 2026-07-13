pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    gfortran
    fortls
    gnumake
    cmake
    gdb
  ];
}
