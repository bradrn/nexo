{-# LANGUAGE LambdaCase #-}

module Nexo.Render (renderMonomorphicType, renderType, renderExpr, renderPartialExpr) where

import Control.Monad.Free
import Data.Functor.Foldable (cata)
import Data.List (intercalate)

import qualified Control.Monad.Trans.Free as TF
import qualified Data.Map as Map

import Nexo.Expr.Type

renderStep :: ASTF String -> String
renderStep (ASTLit lit) = renderLit lit
renderStep (ASTList ss) = '[' : intercalate ", " ss ++ "]"
renderStep (ASTRecord r rec ss) =
    let r' = case r of
            Recursive -> "rec"
            Nonrecursive -> ""
    in r' ++ '(' : intercalate ", " (pickElFromMap rec <$> ss) ++ ")"
  where
    pickElFromMap :: Map.Map String String -> String -> String
    pickElFromMap m k =
        let v = m Map.! k
        in k ++ ": " ++ v
renderStep (ASTVar s) = s
renderStep (ASTLet k t' v x) = case t' of
    Just t  -> "Let(" ++ k ++ " : " ++ renderType t ++ ", " ++ v ++ ", " ++ x ++ ")"
    Nothing -> "Let(" ++ k ++                          ", " ++ v ++ ", " ++ x ++ ")"
renderStep (ASTLam vs x) = "(" ++ intercalate ", " vs ++ ") -> " ++ x
renderStep (ASTField s str) = '(' : s ++ ")." ++ str
renderStep (ASTFun s ss) = s ++ "(" ++ intercalate ", " ss ++ ")"
renderStep (ASTOp op x1 x2) = '(' : x1 ++ ") " ++ op ++ " (" ++ x2 ++ ")"
renderStep (ASTUnit s u) = "(" ++ s ++ ") " ++ renderUnit u
renderStep (ASTTApp s t) = "(" ++ s ++ ") : " ++ renderType t
renderStep ASTNull = "Null"

renderUnit :: UnitDef -> String
renderUnit (ULeaf s) = s
renderUnit (UFactor x) = show x
renderUnit (UMul u1 u2) = '(' : renderUnit u1 ++ ' ' : renderUnit u2 ++ ")"
renderUnit (UDiv u1 u2) = '(' : renderUnit u1 ++ '/' : renderUnit u2 ++ ")"
renderUnit (UExp u@(ULeaf _) n)   = renderUnit u ++ '^' : show n
renderUnit (UExp u@(UFactor _) n) = renderUnit u ++ '^' : show n
renderUnit (UExp u@(UVar _) n)    = renderUnit u ++ '^' : show n
renderUnit (UExp u n) = '(' : renderUnit u ++ ")^" ++ show n
renderUnit (UVar (Rigid v)) = '\'' : v
renderUnit (UVar (Undetermined v)) = '\'' : v

renderType :: PType -> String
renderType (Forall _ts _us t) = renderMonomorphicType t

renderMonomorphicType :: Type -> String
renderMonomorphicType = \case
    TNum u -> "Num<" ++ renderUnit u ++ ">"
    TBool -> "Bool"
    TText -> "Text"
    TVar (Rigid v) -> '\'' : v
    TVar (Undetermined v) -> '\'' : v
    TFun vs x -> "(" ++ intercalate ", " (renderMonomorphicType <$> vs) ++ ") -> " ++ renderMonomorphicType x
    TList a -> "List(" ++ renderMonomorphicType a ++ ")"
    TRecord rec -> '(' : intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderMonomorphicType v]) rec) ++ ")"
    TTable rec -> "Table(" ++ intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderMonomorphicType v]) rec) ++ ")"

renderLit :: Literal -> String
renderLit (LNum x) = show x
renderLit (LBool b) = show b
renderLit (LText s) = show s

renderExpr :: AST -> String
renderExpr = cata renderStep

renderPartialExpr :: Free ASTF String -> String
renderPartialExpr = cata $ \case
   TF.Pure s -> s
   TF.Free x -> renderStep x
