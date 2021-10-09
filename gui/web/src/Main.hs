{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (pack, unpack)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Sheet

import Reflex.Dom hiding (display)

evalOneExpr xstr = do
    x <- parseMaybe pExpr xstr
    let c = Cell "test" Nothing x Invalidated
        s = Sheet $ Map.singleton 0 c
        Sheet s' = evalSheet s
    cellValue <$> Map.lookup 0 s'

main :: IO ()
main = mainWidget $ el "div" $ do
    t <- textInput def

    -- adapted from https://qfpl.io/posts/reflex/basics/dom/
    let bValue    = current $ _textInput_value t
        eKeypress = _textInput_keypress t
        isKey k   = (== k) . keyCodeLookup . fromIntegral
        eEnter    = ffilter (isKey Enter) eKeypress
        eAtEnter  = bValue <@ eEnter

    inputText <- holdDyn "" eAtEnter

    dynText $ (maybe "" (pack . display) . evalOneExpr . unpack) <$> inputText
