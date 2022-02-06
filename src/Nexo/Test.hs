module Nexo.Test where

import qualified Data.Map.Strict as Map
import Data.Traversable (for)

import Nexo.Error (Error(..))
import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Expr.Type.Annotated (delocalise)
import Nexo.Interpret
import Nexo.Sheet

testEvalExpr :: String -> Maybe (PType, Value GlobalEnv)
testEvalExpr = either (const Nothing) Just . testEvalExpr'

testEvalExprs :: [(String, String)] -> Maybe (PType, Value GlobalEnv)
testEvalExprs = either (const Nothing) Just . testEvalExprs'

testEvalExpr' :: String -> Either Error (PType, Value GlobalEnv)
testEvalExpr' xstr = do
    x <- maybe (Left ParseError) Right $ delocalise <$> parseMaybe pExpr xstr
    let c = Cell "test" Nothing (ValueCell "") x Invalidated
        s = Sheet [] Nothing $ Map.singleton 0 c
    Sheet _ _ s' <- evalSheet undefined s
    case cellValue (s' Map.! 0) of
        ValuePresent t v -> Right (t, v)
        ValueError _ e -> Left e
        Invalidated -> error "testEvalExpr': bug in evaluator"

testEvalExprs' :: [(String, String)] -> Either Error (PType, Value GlobalEnv)
testEvalExprs' xstrs = do
    cs <- for xstrs $ \(n, xstr) -> do
        x <- maybe (Left ParseError) Right $ delocalise <$> parseMaybe pExpr xstr
        pure $ Cell n Nothing (ValueCell "") x Invalidated
    let s = Sheet [] Nothing $ Map.fromList $ zip [0..] cs
    Sheet _ _ s' <- evalSheet undefined s
    case cellValue (s' Map.! 0) of
        ValuePresent t v -> Right (t, v)
        ValueError _ e -> Left e
        Invalidated -> error "testEvalExpr': bug in evaluator"
