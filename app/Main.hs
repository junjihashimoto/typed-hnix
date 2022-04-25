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
    mkDerivation $
    DerivationParam
    { pname = "hello"
    , version = "0.1"
    , src = "./."
    , buildInputs = []
    , buildPhase = Nothing
    , installPhase = Nothing
    , builder = Nothing
    , shellHook = Nothing
    }
