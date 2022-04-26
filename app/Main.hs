{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Nix.Typed
import Prelude hiding (pure)

main :: IO ()
main = do
  dump $
    lambdaWithSet ["pkgs"] $ mkDerivation $
    DerivationParam
    { pname = Just "hello"
    , version = Just "0.1"
    , src = Just "./."
    , buildInputs = []
    , buildPhase = Nothing
    , installPhase = Nothing
    , builder = Nothing
    , shellHook = Nothing
    }
