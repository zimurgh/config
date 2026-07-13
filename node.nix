pkgs:

let
  ng = pkgs.writeShellScriptBin "ng" ''
    exec ${pkgs.pnpm}/bin/pnpm dlx @angular/cli "$@"
  '';
in
pkgs.mkShell {
  packages = with pkgs; [
    nodejs_22
    pnpm
    ng
    typescript-language-server
    python3
    pkg-config
  ];

  shellHook = ''
    export COREPACK_ENABLE_DOWNLOAD_PROMPT=0
    export NPM_CONFIG_PREFIX="$HOME/.local/share/npm-global"
    export PNPM_HOME="$HOME/.local/share/pnpm"
    export PATH="$PNPM_HOME:$NPM_CONFIG_PREFIX/bin:$PATH"
  '';
}
