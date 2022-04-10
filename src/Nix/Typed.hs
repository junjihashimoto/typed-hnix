{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances#-}

module Nix.Typed where

import Nix
import Data.Fix
import Prelude hiding ((<*>), pure)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

data NixE a = NixE
  { unNixE :: NExpr
  } deriving Show

class NixExpr a where
  toExpr :: a -> NixE a

class NixApplicative f where
  pure :: NixExpr a => a -> f a
  (<*>) :: (NixExpr a, NixExpr b) => f (a -> b) -> f a -> f b 

instance NixApplicative NixE where
  pure = toExpr
  (<*>) (NixE m1) (NixE m2) = NixE (Fix ((NBinary NApp) m1 m2))

if' :: NixExpr a => NixE Bool -> NixE a -> NixE a -> NixE a
if' (NixE ncond) (NixE na) (NixE nb) = NixE (mkIf ncond na nb)

lambda :: Text -> (NixE a -> NixE b) -> NixE (a -> b)
lambda symbol func = NixE $ mkFunction (Param (fromString $ T.unpack symbol)) (unNixE $ func (NixE (mkSym symbol)))

instance NixExpr Int where
  toExpr i = NixE (mkInt (fromIntegral i))

instance NixExpr Bool where
  toExpr i = NixE (mkBool i)

instance NixExpr Text where
  toExpr i = NixE (mkStr i)

fromBinaryOp
  :: (NixExpr a)
  => (NExpr -> NExpr -> NExpr)
  -> NixE a
  -> NixE a
  -> NixE a
fromBinaryOp fmapExpr a b = NixE (fmapExpr (unNixE a) (unNixE b))

fromUnaryOp
  :: (NixExpr a)
  => (NExpr -> NExpr)
  -> NixE a
  -> NixE a
fromUnaryOp fmapExpr a = NixE (fmapExpr (unNixE a))

instance Num (NixE Int) where
  fromInteger a = NixE $ mkInt a
  (+) = fromBinaryOp ($+)
  (-) = fromBinaryOp ($-)
  (*) = fromBinaryOp ($*)
  abs a =
    let zero = mkInt 0
    in fromUnaryOp (\v -> mkIf (($<) zero v) v (zero $- v)) a
  signum a = 
    let zero = mkInt 0
    in fromUnaryOp (\v ->
                      mkIf (($<) zero v)
                        (mkInt (-1))
                        (mkIf (($>) zero v)
                          (mkInt (1))
                          zero)
                   ) a

dump :: NixE a -> IO ()
dump e = print . prettyNix $ unNixE e

dumpToText :: NixE a -> String
dumpToText e = show $ prettyNix $ unNixE e

