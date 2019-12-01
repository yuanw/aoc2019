{ nixpkgs ? import <nixpkgs> {} , compiler ? "ghc865" }:
let
  inherit (nixpkgs) haskellPackages;

  bootstrap = import <nixpkgs> { };

  nixpgs-19-03-beta = builtins.fromJSON (builtins.readFile ./nixpkgs-19-03-beta.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpgs-19-03-beta) rev sha256;
  };

  pinnedPkgs = import src { };
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = with nixpkgs.haskellPackages;
    [ hlint
      ghcid
      stylish-haskell
      ormolu
      hoogle
      (all-hies.selection {selector = p: {inherit (p) ghc865; };})
    ] ++ [pinnedPkgs.cabal-install
          nixpkgs.ghc];
}
