{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    cabal-install
    hlint
    haskell-language-server
    ormolu
    containers
  ]);
in
  pkgs.stdenv.mkDerivation {
    name = "thinking-with-types";
    buildInputs = [ ghc ];
  }
