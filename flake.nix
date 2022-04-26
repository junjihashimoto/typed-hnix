{
  description = "Typed Haskell Nix";

  nixConfig = {
    substituters = [
      https://cache.nixos.org
      https://iohk.cachix.org
      https://hydra.iohk.io
    ];
    trusted-public-keys = [
      hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
    ];
    bash-prompt = "\\[\\033[1m\\][dev-typed-hnix]\\[\\033\[m\\]\\040\\w$\\040";
  };

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs";
      follows = "haskellNix/nixpkgs-unstable";
    };
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };  

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
#    flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [ haskellNix.overlay
                     (final: prev: {
                       # This overlay adds our project to pkgs
                       typed-hnix-project =
                         final.haskell-nix.project' {
                           src = ./.;
                           compiler-nix-name = "ghc922";
                           # This is used by `nix develop .` to open a shell for use with
                           # `cabal`, `hlint` and `haskell-language-server`
                           shell.tools = {
                             cabal = {};
                             hlint = {};
                             haskell-language-server = {};
                           };
                           # Non-Haskell shell tools go here
                           shell.buildInputs = with pkgs; [
                             nixpkgs-fmt
                           ];
                           # This adds `js-unknown-ghcjs-cabal` to the shell.
                           # shell.crossPlatforms = p: [p.ghcjs];
                         };
                     })
                   ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.typed-hnix-project.flake {
          # This adds support for `nix build .#js-unknown-ghcjs-cabal:hello:exe:hello`
          # crossPlatforms = p: [p.ghcjs];
        };
        haskellToNix = code :
          let
            file = builtins.toFile "haskell-expr" code;
            drv = pkgs.stdenv.mkDerivation {
              name = "haskell-expr-as-nix";
              buildCommand = ''
                set -x
                echo hello
                runghc "${file}"
                runghc "${file}" > $out
              '';

              buildInputs = [ (pkgs.typed-hnix-project.ghcWithPackages (p: with p; [typed-hnix data-default-class])) ];
            };
          in
            import "${drv}" ;
          hello = haskellToNix ''
{-# LANGUAGE OverloadedStrings #-}
import Nix.Typed
import Prelude hiding (pure)
import Data.Default.Class

main :: IO ()
main = do
  dump $
    lambdaWithSet ["pkgs"] $ runCommand "hello" def "mkdir $out ; echo hello > $out/hello.txt"
'' {inherit pkgs;};
      in flake // {
        # Built by `nix build .`
        defaultPackage = flake.packages."typed-hnix:exe:hello";
        packages = {
          inherit hello;
        };
        lib =  {
          inherit haskellToNix;
        };
      });
}
