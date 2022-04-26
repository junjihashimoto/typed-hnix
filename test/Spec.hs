{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Nix.Typed
import Test.Hspec
import Prelude hiding (pure)
import InlineText

main :: IO ()
main = hspec $ do
  describe "Test Typed Nix" $ do
    it "lambda" $ do
      dumpToText (lambda "b" (\(b :: NixE Int) -> 3 + b))
        `shouldBe`
          [inline|b:
                 |  3 + b
                 |]
    it "if" $ do
      dumpToText (lambda "b" (\(b :: NixE Int) -> if' (pure' True) (1 + b) (3 + b)))
        `shouldBe`
          [inline|b:
                 |  if true then 1 + b else 3 + b
                 |]
    it "mkDerivation" $ do
      dumpToText (
        mkDerivation $
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
        )
        `shouldBe` 
          [inline|stdenv.mkDerivation {
                 |  pname = "hello";
                 |  version = "0.1";
                 |  src = ./.;
                 |  buildInputs = [];
                 |  }
                 |]
