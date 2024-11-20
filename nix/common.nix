let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs {};
  haskell = pkgs.haskell.packages.ghc98;
in
{
  pkgs = pkgs;
  haskell = haskell;
}
