pkgs:

pkgs.mkShell {
  packages = with pkgs; [
    ghc
    cabal-install
    haskell-language-server
    haskellPackages.hoogle
    stack
    zlib
    libffi
    gmp
    pkg-config
  ];
}
