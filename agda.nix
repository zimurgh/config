pkgs:

let
  agda = pkgs.agdaPackages.agda.withPackages (p: [ p.standard-library ]);
in
pkgs.mkShell {
  packages = [
    agda
  ];
}
