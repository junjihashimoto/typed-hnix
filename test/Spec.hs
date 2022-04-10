{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nix.Typed
import Test.Hspec
import Prelude hiding (pure)

main :: IO ()
main = hspec $ do
  describe "Test Typed Nix" $ do
    it "lambda" $ do
      dumpToText (lambda "b" (\(b :: NixE Int) -> 3 + b)) `shouldBe` "b:\n  3 + b"
    it "if" $ do
      dumpToText (lambda "b" (\(b :: NixE Int) -> if' (pure True) (1 + b) (3 + b))) `shouldBe` "b:\n  if true then 1 + b else 3 + b"
