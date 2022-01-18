{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Nexo.Sheet.Parse where

import Control.Applicative.Combinators (many)
import Data.Fix
import Data.Functor ((<&>))
import Data.Traversable (for)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Expr.Type.Annotated hiding (span)
import qualified Nexo.Expr.Type.Annotated as Ann
import Nexo.Sheet

mkDef :: String -> ASTLoc -> Maybe Cell
mkDef s (Fix (AnnLocF _ (ASTFun f [Fix (AnnLocF _ var), Fix xloc]))) =
    let varInfo = case var of
            ASTVar v -> Just (v, Nothing)
            ASTTApp (Fix (AnnLocF _ (ASTVar v))) t -> Just (v, Just t)
            _ -> Nothing
    in varInfo >>= \(name, type_) -> case f of
        "DefValue" ->
            let raw = extractSpan (Ann.span xloc) s
            in Just Cell
                { cellName = name
                , cellType = type_
                , cellWidget = ValueCell raw
                , cellExpr =  Fix $ delocalise <$> spanExpr xloc
                , cellValue = Invalidated
                }
        "DefList" -> case xloc of
            AnnLocF _ (ASTList xs) ->
                let texts = xs <&> \(Fix (AnnLocF xspan _)) -> extractSpan xspan s
                in Just Cell
                    { cellName = name
                    , cellType = type_
                    , cellWidget = InputList texts
                    , cellExpr = Fix $ delocalise <$> spanExpr xloc
                    , cellValue = Invalidated
                    }
            _ -> Nothing
        "DefTable" -> case xloc of
            AnnLocF _ (ASTFun "Table" [Fix (AnnLocF _ (ASTRecord Recursive r order))]) ->
                let raw :: Maybe [(String, Either String [String])]
                    raw = for order $ \k -> case Map.lookup k r of
                        Just (Fix (AnnLocF _ (ASTList xs))) -> Just
                            (k, Right $ xs <&> \(Fix (AnnLocF xspan _)) -> extractSpan xspan s)
                        Just (Fix (AnnLocF xspan _)) -> Just
                            (k, Left $ extractSpan xspan s)
                        _ -> Nothing
                in raw <&> \raw' -> Cell
                    { cellName = name
                    , cellType = type_
                    , cellWidget = Table raw'
                    , cellExpr = Fix $ delocalise <$> spanExpr xloc
                    , cellValue = Invalidated
                    }
            _ -> Nothing
        _ -> Nothing
mkDef _ _ = Nothing
    
parseCells :: String -> Maybe [Cell]
parseCells s = parseMaybe (many pExprInner) s >>= traverse (mkDef s)

parseSheet :: String -> Maybe Sheet
parseSheet = fmap (Sheet . toMap) . parseCells
  where
    toMap :: [a] -> Map.Map Int a
    toMap = Map.fromList . zip [0..]
