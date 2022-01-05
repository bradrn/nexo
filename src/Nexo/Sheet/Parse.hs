{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Nexo.Sheet.Parse where

import Control.Applicative.Combinators (many)
import Control.Monad.Free
import Data.Fix
import Data.Functor ((<&>))
import Data.Traversable (for)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Expr.Type.Annotated hiding (span)
import qualified Nexo.Expr.Type.Annotated as Ann
import Nexo.Sheet

mkDef :: String -> ExprLoc -> Maybe Cell
mkDef s (Fix (ExprLocF _ (XFun f [Fix (ExprLocF _ var), Fix xloc]))) =
    let varInfo = case var of
            XVar v -> Just (v, Nothing)
            XTApp (Fix (ExprLocF _ (XVar v))) t -> Just (v, Just t)
            _ -> Nothing
    in varInfo >>= \(name, type_) -> case f of
        "DefValue" ->
            let raw = extractSpan (Ann.span xloc) s
            in Just Cell
                { cellName = name
                , cellType = type_
                , cellRaw = Just $ Pure raw
                , cellExpr =  Fix $ delocalise <$> spanExpr xloc
                , cellValue = Invalidated
                , cellWidget = ValueCell
                }
        "DefList" -> case xloc of
            ExprLocF _ (XList xs) ->
                let texts = xs <&> \(Fix (ExprLocF xspan _)) -> extractSpan xspan s
                in Just Cell
                    { cellName = name
                    , cellType = type_
                    , cellRaw = Just $ Free $ XList $ Pure <$> texts
                    , cellExpr = Fix $ delocalise <$> spanExpr xloc
                    , cellValue = Invalidated
                    , cellWidget = InputList
                    }
            _ -> Nothing
        "DefTable" -> case xloc of
            ExprLocF _ (XTable (Fix (ExprLocF _ (XRecord rec r order)))) ->
                let rawR :: Maybe (Map.Map String (Free ExprF String))
                    rawR = for r $ \case
                        Fix (ExprLocF _ (XList xs)) -> Just $ Free $ XList $
                            xs <&> \(Fix (ExprLocF xspan _)) ->
                                Pure $ extractSpan xspan s
                        _ -> Nothing
                in rawR <&> \rawR' -> Cell
                    { cellName = name
                    , cellType = type_
                    , cellRaw = Just $ Free $ XTable $ Free $ XRecord rec rawR' order
                    , cellExpr = Fix $ delocalise <$> spanExpr xloc
                    , cellValue = Invalidated
                    , cellWidget = Table
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
