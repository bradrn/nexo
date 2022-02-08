{-# LANGUAGE LambdaCase #-}

module Nexo.Sheet.Parse where

import Control.Applicative.Combinators (many)
import Data.Bifunctor (second)
import Data.Either (partitionEithers)
import Data.Fix
import Data.Functor ((<&>))
import Data.Traversable (for)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Expr.Type.Annotated hiding (span)
import qualified Nexo.Expr.Type.Annotated as Ann
import Nexo.Sheet

data Def = CellDef Cell | ImportDef [String]
    deriving (Show)

mkDef :: String -> ASTLoc -> Maybe Def
mkDef _ (Fix (AnnLocF _ (ASTFun "Import" imports))) =
    fmap ImportDef $ for imports $ \case
        Fix (AnnLocF _ (ASTVar v)) -> Just v
        _ -> Nothing
mkDef s (Fix (AnnLocF _ (ASTFun f [Fix (AnnLocF _ var), Fix xloc]))) =
    let varInfo = case var of
            ASTVar v -> Just (v, Nothing)
            ASTTApp (Fix (AnnLocF _ (ASTVar v))) t -> Just (v, Just t)
            _ -> Nothing
    in varInfo >>= \(name, type_) -> case f of
        "DefValue" ->
            let raw = extractSpan (Ann.span xloc) s
            in Just $ CellDef Cell
                { cellName = name
                , cellType = type_
                , cellWidget = ValueCell raw
                , cellExpr =  Fix $ delocalise <$> spanExpr xloc
                , cellValue = Invalidated
                }
        "DefList" -> case xloc of
            AnnLocF _ (ASTFun "List" xs) ->
                let texts = xs <&> \(Fix (AnnLocF xspan _)) -> extractSpan xspan s
                in Just $ CellDef Cell
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
                        Just (Fix (AnnLocF _ (ASTFun "List" xs))) -> Just
                            (k, Right $ xs <&> \(Fix (AnnLocF xspan _)) -> extractSpan xspan s)
                        Just (Fix (AnnLocF xspan _)) -> Just
                            (k, Left $ extractSpan xspan s)
                        _ -> Nothing
                in raw <&> \raw' -> CellDef $ Cell
                    { cellName = name
                    , cellType = type_
                    , cellWidget = Table raw'
                    , cellExpr = Fix $ delocalise <$> spanExpr xloc
                    , cellValue = Invalidated
                    }
            _ -> Nothing
        _ -> Nothing
mkDef _ _ = Nothing
    
parseCells :: String -> Maybe [Def]
parseCells s = parseMaybe (many pExprInner) s >>= traverse (mkDef s)

parseSheet :: String -> Maybe Sheet
parseSheet = fmap (mkSheet . partitionDefs) . parseCells
  where
    defToEither :: Def -> Either Cell [String]
    defToEither (CellDef c) = Left c
    defToEither (ImportDef is) = Right is

    partitionDefs :: [Def] -> ([Cell], [String])
    partitionDefs = second concat . partitionEithers . fmap defToEither

    mkSheet :: ([Cell], [String]) -> Sheet
    mkSheet (cells, imports) = Sheet imports Nothing (toMap cells)

    toMap :: [a] -> Map.Map Int a
    toMap = Map.fromList . zip [0..]
