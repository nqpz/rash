with (import ./nix/common.nix (pkgs: pkgs));
pkgs.mkShell {
  buildInputs = [
    haskell.ghc
    pkgs.cabal-install
    pkgs.zlib.dev
    pkgs.coreutils
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.zlib}/lib";
  '';
}
