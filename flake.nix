{
  description = "Advent of Code Development Shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.simpleFlake {
      inherit self nixpkgs;

      name = "aoc-2023";

      shell = { pkgs ? import <nixpkgs> }: 
        pkgs.mkShell {
          packages = with pkgs; [ 
            ghc
            cabal-install
            haskell-language-server
          ];
        };
    };
}
