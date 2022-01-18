{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Nexo.Render (renderType, renderExpr, renderPartialExpr, renderCell, renderSheet) where

import Control.Monad.Free
import Data.Functor.Foldable (cata)
import Data.List (intercalate)

import qualified Control.Monad.Trans.Free as TF
import qualified Data.Map as Map

import Nexo.Expr.Type
import Nexo.Sheet

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
renderType (Forall _ts _us t) = go t
  where
    go (TNum u) = "Num<" ++ renderUnit u ++ ">"
    go TBool = "Bool"
    go TText = "Text"
    go (TVar (Rigid v)) = '\'' : v
    go (TVar (Undetermined v)) = '\'' : v
    go (TFun vs x) = "(" ++ intercalate ", " (go <$> vs) ++ ") -> " ++ go x
    go (TList a) = "List(" ++ go a ++ ")"
    go (TRecord rec) = '(' : intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ go v]) rec) ++ ")"
    go (TTable rec) = "Table(" ++ intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ go v]) rec) ++ ")"

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

renderCell :: Cell -> String
renderCell Cell{..} =
    defName ++ '(' : cellName ++ optionalCellType ++ ", " ++ renderPartialExpr cellRaw ++ ")"
  where
    (defName, cellRaw) = case cellWidget of
        ValueCell s -> ("DefValue", Pure s)
        InputList ss -> ("DefList", Free $ ASTList $ Pure <$> ss)
        Table ss ->
            ("DefTable"
            , Free $ ASTFun "Table" [Free $ ASTRecord Recursive (fromFormulaOrList <$> Map.fromList ss) (fst <$> ss)]
            )

    optionalCellType = case cellType of
        Nothing -> ""
        Just t -> ' ' : ':' : ' ' : renderType t

    fromFormulaOrList :: Either String [String] -> Free ASTF String
    fromFormulaOrList (Left f) = Pure f
    fromFormulaOrList (Right l) = Free $ ASTList $ Pure <$> l

renderSheet :: Sheet -> String
renderSheet (Sheet s) = intercalate "\n" $ renderCell <$> Map.elems s
