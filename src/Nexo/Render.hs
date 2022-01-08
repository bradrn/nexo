{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Nexo.Render (renderExpr, renderPartialExpr, renderCell, renderSheet) where

import Control.Monad.Free
import Data.Functor.Foldable (cata)
import Data.List (intercalate)

import qualified Control.Monad.Trans.Free as TF
import qualified Data.Map as Map

import Nexo.Expr.Type
import Nexo.Sheet

renderStep :: ExprF String -> String
renderStep (XLit lit) = renderLit lit
renderStep (XList ss) = '[' : concatMultilineList ss ++ "\n]"
renderStep (XRecord r rec ss) =
    let r' = case r of
            Recursive -> "rec"
            Nonrecursive -> ""
    in r' ++ '(' : concatMultilineList (pickElFromMap rec <$> ss) ++ "\n)"
  where
    pickElFromMap :: Map.Map String String -> String -> String
    pickElFromMap m k =
        let v = m Map.! k
        in k ++ ": " ++ v
renderStep (XTable s) = "Table(" ++ s ++ ")"
renderStep (XVar s) = s
renderStep (XLet k t' v x) = case t' of
    Just t  -> "Let(" ++ k ++ " : " ++ renderType t ++ ", " ++ v ++ ", " ++ x ++ ")"
    Nothing -> "Let(" ++ k ++                          ", " ++ v ++ ", " ++ x ++ ")"
renderStep (XLam vs x) = "(" ++ intercalate ", " vs ++ ") -> " ++ x
renderStep (XField s str) = s ++ "." ++ str
renderStep (XFun s ss) = s ++ "(" ++ intercalate ", " ss ++ ")"
renderStep (XOp op x1 x2) = '(' : x1 ++ ") " ++ renderOp op ++ " (" ++ x2 ++ ")"
renderStep (XUnit s u) = "(" ++ s ++ ") " ++ renderUnit u
renderStep (XTApp s t) = "(" ++ s ++ ") : " ++ renderType t
renderStep XNull = "Null"

renderUnit :: UnitDef -> String
renderUnit (UName s) = s
renderUnit (UPrefix s) = s
renderUnit (UFactor x) = show x
renderUnit (UMul u1 u2) = renderUnit u1 ++ ' ' : renderUnit u2
renderUnit (UDiv u1 u2) = renderUnit u1 ++ '/' : renderUnit u2
renderUnit (UExp u n) = renderUnit u ++ '^' : show n
renderUnit (UVar (Rigid v)) = '\'' : v
renderUnit (UVar (Undetermined v)) = '\'' : v

renderOp :: Op -> String
renderOp OPlus  = "+"
renderOp OMinus = "-"
renderOp OTimes = "*"
renderOp ODiv   = "/"
renderOp OEq    = "="
renderOp ONeq   = "<>"
renderOp OGt    = ">"
renderOp OLt    = "<"
renderOp OAnd   = "&&"
renderOp OOr    = "||"

renderType :: PType -> String
renderType (Forall _ts _us t) = go t
  where
    go (TNum u) = "Num<" ++ renderUnit u ++ ">"
    go TBool = "Bool"
    go TText = "Text"
    go (TVar (Rigid v)) = '\'' : v
    go (TVar (Undetermined v)) = '\'' : v
    go (TFun vs x) = "(" ++ intercalate ", " (go <$> vs) ++ ") -> " ++ go x
    go (TList a) = '[' : go a ++ "]"
    go (TRecord rec) = '(' : intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ go v]) rec) ++ ")"
    go (TTable rec) = "Table(" ++ intercalate "," (Map.foldMapWithKey (\k v -> [k ++ ": " ++ go v]) rec) ++ ")"

concatMultilineList :: [String] -> String
concatMultilineList = intercalate "," . fmap ("\n    "++)

renderLit :: Literal -> String
renderLit (LNum x) = show x
renderLit (LBool b) = show b
renderLit (LText s) = show s

renderExpr :: Expr -> String
renderExpr = cata renderStep

renderPartialExpr :: Free ExprF String -> String
renderPartialExpr = cata $ \case
   TF.Pure s -> s
   TF.Free x -> renderStep x

renderCell :: Cell -> String
renderCell Cell{..} =
    defName ++ '(' : cellName ++ optionalCellType ++ ", " ++ renderPartialExpr cellRaw ++ ")"
  where
    (defName, cellRaw) = case cellWidget of
        ValueCell s -> ("DefValue", Pure s)
        InputList ss -> ("DefList", Free $ XList $ Pure <$> ss)
        Table ss ->
            ("DefTable"
            , Free $ XTable $ Free $ XRecord Recursive (Free . XList . fmap Pure <$> Map.fromList ss) (fst <$> ss)
            )

    optionalCellType = case cellType of
        Nothing -> ""
        Just t -> ' ' : ':' : ' ' : renderType t

renderSheet :: Sheet -> String
renderSheet (Sheet s) = intercalate "\n" $ renderCell <$> Map.elems s
