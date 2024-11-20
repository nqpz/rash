with import ./nix/common.nix;
pkgs.haskell.lib.overrideCabal
  (haskell.callCabal2nix "rash" ./. { })
  (_: {
    configureFlags = [
      "--ghc-option=-Werror"
      "--ghc-option=-O2"
    ];
    doHaddock = false;
  })
