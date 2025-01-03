with import ./nix/common.nix;
pkgs.mkShell {
  buildInputs = [
    haskell.ghc
    pkgs.cabal-install
    pkgs.zlib.dev
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.zlib}/lib";
  '';
}
