# Typed HNix

Typed HNix is a library to generate nix expression from haskell-code.
This project is inspired by both dhall-nix and purenix.

# Usage

This package uses an `Applicative` of `NixE` to type Nix Expression.
Generate a Nix Expression from a NixE Applicative with dump-function as follows.

```
$ cabal repl
Build profile: -w ghc-9.2.2 -O1
In order, the following will be built (use -v for more details):
- typed-hnix-0.1.0.0 (lib) (ephemeral targets)
Preprocessing library for typed-hnix-0.1.0.0..
GHCi, version 9.2.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Nix.Typed        ( src/Nix/Typed.hs, interpreted )
Ok, one module loaded.
ghci> :set -XOverloadedStrings
ghci> :set -XScopedTypeVariables
ghci> dump $ lambda "x" (\(x::NixE Int) -> x + 1)
x:
  x + 1
```

You can use the following haskellToNix-function to translate Haskell source code to the corresponding Nix expression directly within Nix.

```
{pkgs}:
haskellToNix ''
{-# LANGUAGE OverloadedStrings #-}
import Nix.Typed
import Prelude hiding (pure)
import Data.Default.Class

main = dump $ lambdaWithSet ["pkgs"] $ runCommand "hello" def "mkdir $out ; echo hello > $out/hello.txt"
'' {inherit pkgs;};
```

