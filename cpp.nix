pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    gcc
    clang
    clang-tools
    cmake
    gnumake
    ninja
    pkg-config
    gdb
    lldb
  ];
}
