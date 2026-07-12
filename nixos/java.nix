pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    jdk25
    gradle
    jdt-language-server
  ];

  shellHook = ''
    export JAVA_HOME="${pkgs.jdk25.home}"
  '';
}
