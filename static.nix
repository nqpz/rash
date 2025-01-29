let
  fixGHC = pkg: pkg.override {
    enableRelocatedStaticLibs = true;
    enableShared = false;
    enableDwarf = false;
  };
  fixHaskell = haskell:
    haskell.override (oldHaskell: {
      ghc = fixGHC oldHaskell.ghc;
      buildHaskellPackages = oldHaskell.buildHaskellPackages.override (oldBuildHaskellPackages: {
        ghc = fixGHC oldBuildHaskellPackages.ghc;
      });
    });
in
with (import ./nix/common.nix (pkgs: pkgs.pkgsMusl));
pkgs.haskell.lib.overrideCabal
  ((fixHaskell haskell).callCabal2nix "rash" ./. { })
  (old: {
    configureFlags = (old.configureFlags or []) ++ [
      "--ghc-option=-Werror"
      "--ghc-option=-O2"
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ];
    doHaddock = false;
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    isLibrary = false;
    isExecutable = true;
    enableLibraryProfiling = false;
  })
