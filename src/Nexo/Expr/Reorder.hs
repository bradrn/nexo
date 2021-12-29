{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module Nexo.Expr.Reorder where

import Data.Fix (Fix(..))
import Data.Functor.Foldable (zygo)
import Data.Set (Set, (\\))

import Data.Foldable (fold)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Nexo.Expr.Type

depends :: ExprF (Set String) -> Set String
depends (XLit _lit) = Set.empty
depends (XList sets) = fold sets
depends (XRecord Nonrecursive r _ss) = fold r
depends (XRecord Recursive r _ss) = fold r \\ Map.keysSet r
depends (XTable r) = r
depends (XVar v) = Set.singleton v
depends (XLet v _ xv x) = xv <> (Set.delete v x)
depends (XLam args x) = x \\ Set.fromList args
depends (XField x _) = x
depends (XFun _ args) = fold args
depends (XOp _ x1 x2) = x1 <> x2
depends (XUnit x _) = x
depends (XTApp x _) = x
depends XNull = Set.empty

topoSort :: ExprF (Set String, Expr) -> Expr
topoSort = \case
    XRecord rty r _ -> Fix $ XRecord rty (snd <$> r) (go $ restrictToTable $ fst <$> r)
    x -> Fix $ snd <$> x
  where
    -- preliminary step: remove all dependencies from outside table
    restrictToTable :: Ord a => Map.Map a (Set a) -> Map.Map a (Set a)
    restrictToTable t =
        let ks = Map.keysSet t
        in Set.filter (`Set.member` ks) <$> t

    go :: Ord a => Map.Map a (Set a) -> [a]
    go m
        | Map.null m = []
        | otherwise =
            let (Map.keysSet -> noDeps, deps) = Map.partition Set.null m
                deps' = (\\ noDeps) <$> deps
            in Set.toList noDeps ++ go deps'

reorder :: Expr -> Expr
reorder = zygo depends topoSort
