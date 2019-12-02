{ nixpkgs ? import <nixpkgs> {} , compiler ? "ghc865" }:
let

  inherit (nixpkgs) haskellPackages;
  myPackages = haskellPackages.callCabal2nix "project" ./aoc2019.cabal {};

  bootstrap = import <nixpkgs> { };

  easy-ps = import (
    nixpkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "b2efbe30c55ffa16dd6f85cb7c71f77ac3136aa0";
      sha256 = "0mmjvqpkns964sjkbw0waqb47vfhg9r0fp9y0b5pizpikmw3fbp2";
    }
  ) { };

  nixpgs-19-03-beta = builtins.fromJSON (builtins.readFile ./nix/nixpkgs-19-03-beta.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpgs-19-03-beta) rev sha256;
  };

  pinnedPkgs = import src { };
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
    ] ++ [pinnedPkgs.cabal-install
          easy-ps.purs 
          easy-ps.spago 
          easy-ps.spago2nix 
          nixpkgs.cacert 
         ];
}
