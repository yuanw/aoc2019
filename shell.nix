{ nixpkgs ? import <nixpkgs> {} , compiler ? "ghc865" }:
let
  inherit (nixpkgs) haskellPackages; 
  bootstrap = import <nixpkgs> { };
  nixpgs-19-03-beta = builtins.fromJSON (builtins.readFile ./nix/nixpkgs-19-03-beta.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpgs-19-03-beta) rev sha256;
  };

  pinnedPkgs = import src { };
  myPackages = pinnedPkgs.haskellPackages.callCabal2nix "project" ./aoc2019.cabal {};
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
 haskellPackages.shellFor {
   withHoogle = true;
   packages = p: [myPackages];
   buildInputs = with nixpkgs.haskellPackages;
    [ hlint
      ghcid
      stylish-haskell
      hoogle
      (all-hies.selection {selector = p: {inherit (p) ghc865; };})
    ] ++ [pinnedPkgs.cabal-install];
}
