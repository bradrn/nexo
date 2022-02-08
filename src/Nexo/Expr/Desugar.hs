{-# LANGUAGE ViewPatterns #-}

module Nexo.Expr.Desugar (desugar) where

import Data.Fix (Fix(..))
import Data.Functor.Foldable (zygo)
import Data.Set (Set, (\\))

import Data.Foldable (fold)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Nexo.Expr.Type

depends :: ASTF (Set String) -> Set String
depends (ASTLit _lit) = Set.empty
depends (ASTRecord Nonrecursive r _ss) = fold r
depends (ASTRecord Recursive r _ss) = fold r \\ Map.keysSet r
depends (ASTVar v) = Set.singleton v
depends (ASTLet v _ xv x) = xv <> Set.delete v x
depends (ASTLam args x) = x \\ Set.fromList args
depends (ASTField x _) = x
depends (ASTFun _ args) = fold args
depends (ASTOp _ x1 x2) = x1 <> x2
depends (ASTUnit x _) = x
depends (ASTTApp x _) = x
depends ASTNull = Set.empty

topoSort :: Ord a => Map.Map a (Set a) -> [a]
topoSort = go . restrictToTable
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

desugarStep :: ASTF (Set String, Expr) -> Expr
desugarStep (ASTLit lit) = Fix $ XAtom $ Lit lit
desugarStep (ASTRecord rty r _ss) = Fix $ XRecord rty (snd <$> r) (topoSort $ fst <$> r)
desugarStep (ASTVar s) = Fix $ XAtom $ Var s
desugarStep (ASTLet v t (_, xv) (_, x)) = Fix $ XFunApp
    (Fix $ XNamedFunApp "Lambda" [Fix $ XAtom $ Var v, x])
    [maybe xv (Fix . XTypeApp xv) t]
desugarStep (ASTLam args (_, x)) = Fix $ XNamedFunApp "Lambda" (fmap (Fix . XAtom . Var) args ++ [x])
desugarStep (ASTField (_, x) f) = Fix $ XNamedFunApp "GetField" [x, Fix $ XAtom $ Var f]
desugarStep (ASTFun f args) = Fix $ XNamedFunApp f (snd <$> args)
desugarStep (ASTOp op (_, x1) (_, x2)) = Fix $ XNamedFunApp op [x1, x2]
desugarStep (ASTUnit (_, x) u) = Fix $ XUnitApp x u
desugarStep (ASTTApp (_, x) t) = Fix $ XTypeApp x t
desugarStep ASTNull = Fix $ XAtom Null

desugar :: AST -> Expr
desugar = zygo depends desugarStep
