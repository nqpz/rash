with (import ./nix/common.nix (pkgs: pkgs));
pkgs.haskell.lib.overrideCabal
  (haskell.callCabal2nix "rash" ./. { })
  (old: {
    configureFlags = (old.configureFlags or []) ++ [
      "--ghc-option=-Werror"
      "--ghc-option=-O2"
    ];
    doHaddock = false;
  })
