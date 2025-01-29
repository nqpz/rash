pkgsMod:
let
  sources = import ./sources.nix;
  pkgs = pkgsMod (import sources.nixpkgs {});
  haskell = pkgs.haskell.packages.ghc98;
in
{
  pkgs = pkgs;
  haskell = haskell;
}
