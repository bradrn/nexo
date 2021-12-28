module Nexo.Test where

import qualified Data.Map.Strict as Map
import Data.Traversable (for)

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Interpret
import Nexo.Sheet

testEvalExpr :: String -> Maybe (PType, Value GlobalEnv)
testEvalExpr xstr = do
    x <- parseMaybe pExpr xstr
    let c = Cell "test" Nothing x Invalidated
        s = Sheet $ Map.singleton 0 c
        Sheet s' = evalSheet s
    val <- cellValue <$> Map.lookup 0 s'
    case val of
        ValuePresent t v -> Just (t, v)
        _ -> Nothing

testEvalExprs :: [(String, String)] -> Maybe (PType, Value GlobalEnv)
testEvalExprs xstrs = do
    cs <- for xstrs $ \(n, xstr) -> do
        x <- parseMaybe pExpr xstr
        pure $ Cell n Nothing x Invalidated
    let s = Sheet $ Map.fromList $ zip [0..] cs
        Sheet s' = evalSheet s
    val <- cellValue <$> Map.lookup 0 s'
    case val of
        ValuePresent t v -> Just (t, v)
        _ -> Nothing


