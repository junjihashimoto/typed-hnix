{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module Nix.Typed where

import qualified Nix as N
import Nix (NExpr)
import Nix.TH (nix)
import Data.Fix
import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Default.Class
import GHC.Generics

data NixE a = NixE
  { unNixE :: NExpr
  } deriving Show

class NixExpr a where
  toExpr :: a -> NixE a

class NixApplicative f where
  pure' :: NixExpr a => a -> f a
  ($<*>) :: (NixExpr a, NixExpr b) => f (a -> b) -> f a -> f b 

class NixEq a where
  ($==) :: NixE a -> NixE a -> NixE Bool
  ($/=) :: NixE a -> NixE a -> NixE Bool

instance NixEq Int where
  ($==) = fromBinaryOp (N.$==)
  ($/=) = fromBinaryOp (N.$!=)

class NixEq a => NixOrd a where
  ($<) ::NixE a -> NixE a -> NixE Bool
  ($<=) ::NixE a -> NixE a -> NixE Bool
  ($>) ::NixE a -> NixE a -> NixE Bool
  ($>=) ::NixE a -> NixE a -> NixE Bool

instance NixOrd Int where
  ($<) = fromBinaryOp (N.$<)
  ($<=) = fromBinaryOp (N.$<=)
  ($>) = fromBinaryOp (N.$>)
  ($>=) = fromBinaryOp (N.$>=)

instance NixApplicative NixE where
  pure' = toExpr
  ($<*>) (NixE m1) (NixE m2) = NixE $ m1 N.@@ m2

if' :: NixExpr a => NixE Bool -> NixE a -> NixE a -> NixE a
if' (NixE ncond) (NixE na) (NixE nb) = NixE (N.mkIf ncond na nb)

lambda :: Text -> (NixE a -> NixE b) -> NixE (a -> b)
lambda symbol func = NixE $ N.mkFunction (N.Param (fromString $ T.unpack symbol)) (unNixE $ func (NixE (N.mkSym symbol)))

lambdaWithSet :: [Text] -> NixE b -> NixE b
lambdaWithSet symbols func = NixE $ N.mkFunction (N.mkParamSet (map (\v -> (v,Nothing)) symbols)) (unNixE $ func )
instance NixExpr Int where
  toExpr = NixE . N.mkInt . fromIntegral

instance NixExpr Integer where
  toExpr = NixE . N.mkInt

instance NixExpr Float where
  toExpr = NixE . N.mkFloat

instance NixExpr Bool where
  toExpr = NixE . N.mkBool

instance NixExpr Text where
  toExpr = NixE . N.mkStr

newtype Path = Path {unPath :: FilePath}

instance IsString Path where
  fromString = Path

instance NixExpr Path where
  toExpr = NixE . N.mkPath False . unPath

instance NixExpr (NixE a) where
  toExpr = NixE . unNixE

instance NixExpr a => NixExpr [a] where
  toExpr = NixE . N.mkList . map (unNixE . toExpr)

join' :: NixE (NixE a) -> NixE a
join' = NixE . unNixE

fromBinaryOp
  :: (NixExpr a, NixExpr b)
  => (NExpr -> NExpr -> NExpr)
  -> NixE a
  -> NixE a
  -> NixE b
fromBinaryOp fmapExpr a b = NixE (fmapExpr (unNixE a) (unNixE b))

fromUnaryOp
  :: (NixExpr a, NixExpr b)
  => (NExpr -> NExpr)
  -> NixE a
  -> NixE b
fromUnaryOp fmapExpr a = NixE (fmapExpr (unNixE a))

instance Num (NixE Int) where
  fromInteger a = NixE $ N.mkInt a
  (+) = fromBinaryOp (N.$+)
  (-) = fromBinaryOp (N.$-)
  (*) = fromBinaryOp (N.$*)
  abs a =
    let zero = N.mkInt 0
    in fromUnaryOp (\v -> N.mkIf ((N.$<) zero v) v (zero N.$- v)) a
  signum a = 
    let zero = N.mkInt 0
    in fromUnaryOp (\v ->
                      N.mkIf ((N.$<) zero v)
                        (N.mkInt (-1))
                        (N.mkIf ((N.$>) zero v)
                          (N.mkInt (1))
                          zero)
                   ) a

instance Num (NixE Float) where
  fromInteger a = NixE $ N.mkFloat (fromIntegral a)
  (+) = fromBinaryOp (N.$+)
  (-) = fromBinaryOp (N.$-)
  (*) = fromBinaryOp (N.$*)
  abs a =
    let zero = N.mkFloat 0
    in fromUnaryOp (\v -> N.mkIf ((N.$<) zero v) v (zero N.$- v)) a
  signum a = 
    let zero = N.mkFloat 0
    in fromUnaryOp (\v ->
                      N.mkIf ((N.$<) zero v)
                        (N.mkFloat (-1))
                        (N.mkIf ((N.$>) zero v)
                          (N.mkFloat (1))
                          zero)
                   ) a


class NixSet a where
  toSet :: a -> [N.Binding NExpr]

data Derivation

data DerivationParam = DerivationParam
  { pname :: Maybe Text
  , version :: Maybe Text
  , src :: Maybe Path
  , buildInputs :: [NixE Derivation]
  , buildPhase :: Maybe Text
  , installPhase :: Maybe Text
  , builder :: Maybe Text
  , shellHook :: Maybe Text
  } deriving (Generic, Default)

instance NixSet DerivationParam where
  toSet param =
    (  opt "pname" param.pname
      ++ opt "version" param.version
      ++ opt "src" param.src
      ++ set "buildInputs" param.buildInputs
      ++ opt "buildPhase" param.buildPhase
      ++ opt "installPhase" param.installPhase
      ++ opt "builder" param.builder
      ++ opt "shellHook" param.shellHook
    )

set :: NixExpr a => Text -> a -> [N.Binding NExpr]
set key v = [ key N.$= unNixE (toExpr v) ]

opt :: NixExpr a => Text -> Maybe a -> [N.Binding NExpr]
opt key (Just v) = [ key N.$= unNixE (toExpr v) ]
opt _ Nothing = []

mkDerivation :: DerivationParam -> NixE Derivation
mkDerivation param = NixE $ [nix|stdenv.mkDerivation|] N.@@ N.mkNonRecSet (toSet param)

runCommand :: Text -> DerivationParam -> Text -> NixE Derivation
runCommand name param code = NixE $ [nix|pkgs.runCommand|] N.@@ unNixE (toExpr name) N.@@ N.mkNonRecSet (toSet param) N.@@ unNixE (toExpr code)

dump :: NixE a -> IO ()
dump e = print . N.prettyNix . unNixE $ e

dumpToText :: NixE a -> Text
dumpToText e = T.pack . show . N.prettyNix . unNixE $ e
