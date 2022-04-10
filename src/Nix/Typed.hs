{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances#-}

module Nix.Typed where

import qualified Nix as N
import Nix (NExpr)
import Data.Fix
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

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
  ($<*>) (NixE m1) (NixE m2) = NixE (Fix ((N.NBinary N.NApp) m1 m2))

if' :: NixExpr a => NixE Bool -> NixE a -> NixE a -> NixE a
if' (NixE ncond) (NixE na) (NixE nb) = NixE (N.mkIf ncond na nb)

lambda :: Text -> (NixE a -> NixE b) -> NixE (a -> b)
lambda symbol func = NixE $ N.mkFunction (N.Param (fromString $ T.unpack symbol)) (unNixE $ func (NixE (N.mkSym symbol)))

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

dump :: NixE a -> IO ()
dump e = print . N.prettyNix . unNixE $ e

dumpToText :: NixE a -> String
dumpToText e = show . N.prettyNix . unNixE $ e

