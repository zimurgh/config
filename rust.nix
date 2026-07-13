{ pkgs, fenixPkgs }:

let
  stable = fenixPkgs.fromToolchainName {
    name = "1.97.0";
    sha256 = "sha256-OATSZm98Es5kIFuqaba+UvkQtFsVgJEBMmS+t6od5/U=";
  };
  toolchain = stable.withComponents [
    "rustc"
    "cargo"
    "clippy"
    "rust-src"
    "rustfmt"
  ];
  analyzer = fenixPkgs.rust-analyzer;
  rustSrcPath = "${stable.rust-src}/lib/rustlib/src/rust/library";
in
{
  inherit toolchain analyzer rustSrcPath;

  devShell = pkgs.mkShell {
    packages = with pkgs; [
      toolchain
      analyzer
      pkg-config
      openssl
      clang
      lldb
      taplo
    ];

    shellHook = ''
      export RUST_SRC_PATH="${rustSrcPath}"
      export OPENSSL_NO_VENDOR=1
    '';
  };
}
