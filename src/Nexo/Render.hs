{-# LANGUAGE LambdaCase #-}

module Nexo.Render
       ( renderMonomorphicType
       , renderCoreType
       , renderType
       , renderExpr
       , renderPartialExpr
       ) where

import Control.Monad.Free
import Data.Functor.Foldable (cata)
import Data.List (intercalate)

import qualified Control.Monad.Trans.Free as TF
import qualified Data.Map as Map

import qualified Nexo.Core.Type as Core
import qualified Nexo.Expr.Type as Expr

renderStep :: Expr.ASTF String -> String
renderStep (Expr.ASTLit lit) = renderLit lit
renderStep (Expr.ASTRecord r rec ss) =
    let r' = case r of
            Expr.Recursive -> "rec"
            Expr.Nonrecursive -> ""
    in r' ++ '(' : intercalate ", " (pickElFromMap rec <$> ss) ++ ")"
  where
    pickElFromMap :: Map.Map String String -> String -> String
    pickElFromMap m k =
        let v = m Map.! k
        in k ++ ": " ++ v
renderStep (Expr.ASTVar s) = s
renderStep (Expr.ASTLet k t' v x) = case t' of
    Just t  -> "Let(" ++ k ++ " : " ++ renderMonomorphicType t ++ ", " ++ v ++ ", " ++ x ++ ")"
    Nothing -> "Let(" ++ k ++                          ", " ++ v ++ ", " ++ x ++ ")"
renderStep (Expr.ASTLam vs x) = "(" ++ intercalate ", " vs ++ ") -> " ++ x
renderStep (Expr.ASTField s str) = '(' : s ++ ")." ++ str
renderStep (Expr.ASTFun s ss) = s ++ "(" ++ intercalate ", " ss ++ ")"
renderStep (Expr.ASTOp op x1 x2) = '(' : x1 ++ ") " ++ op ++ " (" ++ x2 ++ ")"
renderStep (Expr.ASTUnit s u) = "(" ++ s ++ ") " ++ renderUnit u
renderStep (Expr.ASTTApp s t) = "(" ++ s ++ ") : " ++ renderMonomorphicType t
renderStep Expr.ASTNull = "Null"

renderUnit :: Expr.Unit -> String
renderUnit (Expr.ULeaf s) = s
renderUnit (Expr.UFactor x) = show x
renderUnit (Expr.UMul u1 u2) = '(' : renderUnit u1 ++ ' ' : renderUnit u2 ++ ")"
renderUnit (Expr.UDiv u1 u2) = '(' : renderUnit u1 ++ '/' : renderUnit u2 ++ ")"
renderUnit (Expr.UExp u@(Expr.ULeaf _) n)   = renderUnit u ++ '^' : show n
renderUnit (Expr.UExp u@(Expr.UFactor _) n) = renderUnit u ++ '^' : show n
renderUnit (Expr.UExp u@(Expr.UVar _) n)    = renderUnit u ++ '^' : show n
renderUnit (Expr.UExp u n) = '(' : renderUnit u ++ ")^" ++ show n
renderUnit (Expr.UVar v) = '\'' : v

renderCoreUnit :: Core.Unit -> String
renderCoreUnit (f, ds) =
    let rendered = Map.foldMapWithKey renderDimension ds
    in unwords $
       if f == 1 then rendered else show f : rendered
  where
    renderDimension :: Either String Core.TVar -> Int -> [String]
    renderDimension (Left u ) 1                     = [       u                ]
    renderDimension (Left u ) x                     = [       u ++ '^' : show x]
    renderDimension (Right (Core.Rigid v)) 1        = ['\'' : v                ]
    renderDimension (Right (Core.Rigid v)) x        = ['\'' : v ++ '^' : show x]
    renderDimension (Right (Core.Undetermined v)) 1 = ['\'' : v                ]
    renderDimension (Right (Core.Undetermined v)) x = ['\'' : v ++ '^' : show x]

renderType :: Core.PType -> String
renderType (Core.Forall _ts t) = renderCoreType t

renderCoreType :: Core.Type -> String
renderCoreType = \case
    Core.TNum t -> "Num<" ++ renderCoreType t ++ ">"
    Core.TBool -> "Bool"
    Core.TText -> "Text"
    Core.TVar (Core.Rigid v) -> '\'' : v
    Core.TVar (Core.Undetermined v) -> '\'' : v
    Core.TFun vs x -> "(" ++ intercalate ", " (renderCoreType <$> vs) ++ ") -> " ++ renderCoreType x
    Core.TList a -> "List(" ++ renderCoreType a ++ ")"
    Core.TRecord rec -> '(' : intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderCoreType v]) rec) ++ ")"
    Core.TTable rec -> "Table(" ++ intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderCoreType v]) rec) ++ ")"
    Core.TUnit u -> renderCoreUnit u

renderMonomorphicType :: Expr.Type -> String
renderMonomorphicType = \case
    Expr.TNum t -> "Num<" ++ renderUnit t ++ ">"
    Expr.TBool -> "Bool"
    Expr.TText -> "Text"
    Expr.TVar v -> '\'' : v
    Expr.TFun vs x -> "(" ++ intercalate ", " (renderMonomorphicType <$> vs) ++ ") -> " ++ renderMonomorphicType x
    Expr.TList a -> "List(" ++ renderMonomorphicType a ++ ")"
    Expr.TRecord rec -> '(' : intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderMonomorphicType v]) rec) ++ ")"
    Expr.TTable rec -> "Table(" ++ intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderMonomorphicType v]) rec) ++ ")"

renderLit :: Expr.Literal -> String
renderLit (Expr.Num x) = show x
renderLit (Expr.Bool b) = show b
renderLit (Expr.Text s) = show s

renderExpr :: Expr.AST -> String
renderExpr = cata renderStep

renderPartialExpr :: Free Expr.ASTF String -> String
renderPartialExpr = cata $ \case
   TF.Pure s -> s
   TF.Free x -> renderStep x
