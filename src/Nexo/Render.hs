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

import Nexo.Core.Type
import Nexo.Expr.Type

renderStep :: ASTF String -> String
renderStep (ASTLit lit) = renderLit lit
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
    Just t  -> "Let(" ++ k ++ " : " ++ renderMonomorphicType t ++ ", " ++ v ++ ", " ++ x ++ ")"
    Nothing -> "Let(" ++ k ++                          ", " ++ v ++ ", " ++ x ++ ")"
renderStep (ASTLam vs x) = "(" ++ intercalate ", " vs ++ ") -> " ++ x
renderStep (ASTField s str) = '(' : s ++ ")." ++ str
renderStep (ASTFun s ss) = s ++ "(" ++ intercalate ", " ss ++ ")"
renderStep (ASTOp op x1 x2) = '(' : x1 ++ ") " ++ op ++ " (" ++ x2 ++ ")"
renderStep (ASTUnit s u) = "(" ++ s ++ ") " ++ renderUnit u
renderStep (ASTTApp s t) = "(" ++ s ++ ") : " ++ renderMonomorphicType t
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
renderUnit (UVar v) = '\'' : v

renderCoreUnit :: Unit -> String
renderCoreUnit (f, ds) =
    let rendered = Map.foldMapWithKey renderDimension ds
    in unwords $
       if f == 1 then rendered else show f : rendered
  where
    renderDimension :: Either String CoreVar -> Int -> [String]
    renderDimension (Left u ) 1                = [       u                ]
    renderDimension (Left u ) x                = [       u ++ '^' : show x]
    renderDimension (Right (Rigid v)) 1        = ['\'' : v                ]
    renderDimension (Right (Rigid v)) x        = ['\'' : v ++ '^' : show x]
    renderDimension (Right (Undetermined v)) 1 = ['\'' : v                ]
    renderDimension (Right (Undetermined v)) x = ['\'' : v ++ '^' : show x]

renderType :: PType -> String
renderType (Forall _ts t) = renderCoreType t

renderCoreType :: CoreType -> String
renderCoreType = \case
    CNum t -> "Num<" ++ renderCoreType t ++ ">"
    CBool -> "Bool"
    CText -> "Text"
    CTVar (Rigid v) -> '\'' : v
    CTVar (Undetermined v) -> '\'' : v
    CFun vs x -> "(" ++ intercalate ", " (renderCoreType <$> vs) ++ ") -> " ++ renderCoreType x
    CList a -> "List(" ++ renderCoreType a ++ ")"
    CRecord rec -> '(' : intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderCoreType v]) rec) ++ ")"
    CTable rec -> "Table(" ++ intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ renderCoreType v]) rec) ++ ")"
    CUnit u -> renderCoreUnit u

renderMonomorphicType :: Type -> String
renderMonomorphicType = \case
    TNum t -> "Num<" ++ renderUnit t ++ ">"
    TBool -> "Bool"
    TText -> "Text"
    TVar v -> '\'' : v
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
