fenixPkgs:

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
in
{
  inherit toolchain;
  analyzer = fenixPkgs.rust-analyzer;
  rustSrcPath = "${stable.rust-src}/lib/rustlib/src/rust/library";
}
