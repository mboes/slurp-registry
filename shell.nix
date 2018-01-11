{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "slurp-repository";
  inherit ghc;
  buildInputs = with pkgs; [ zlib ];
  LANG = "en_GB.UTF-8";
}
