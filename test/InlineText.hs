{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module InlineText where

import Text.Shakespeare
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (toLazyText)


settings :: Q ShakespeareSettings
settings = do
  toTExp <- [|toText|]
  wrapExp <- [|id|]
  unWrapExp <- [|id|]
  return $ defaultShakespeareSettings { toBuilder = toTExp
                                      , wrap = wrapExp
                                      , unwrap = unWrapExp
                                      }


dropBar :: [TL.Text] -> [TL.Text]
dropBar [] = []
dropBar (c:cx) = c:dropBar' cx
  where
    dropBar' txt = reverse $ drop 1 $ map (TL.drop 1 . TL.dropWhile (/= '|')) $ reverse txt

inline :: QuasiQuoter
inline = QuasiQuoter { quoteExp = \s -> do
                      rs <- settings
                      render <- [|TL.toStrict . TL.dropEnd 1 . TL.unlines . dropBar . TL.lines . toLazyText|]
                      rendered <- shakespeareFromString rs { justVarInterpolation = True } s
                      return (render `AppE` rendered)
                  }
