{-# LANGUAGE RecordWildCards #-}

module Nexo.Sheet.Render where

import Control.Monad.Free
import Data.List (intercalate)

import qualified Data.Map as Map

import Nexo.Expr.Type
import Nexo.Render
import Nexo.Sheet

renderCell :: Cell -> String
renderCell Cell{..} =
    defName ++ '(' : cellName ++ optionalCellType ++ ", " ++ renderPartialExpr cellRaw ++ ")"
  where
    (defName, cellRaw) = case cellWidget of
        ValueCell s -> ("DefValue", Pure s)
        InputList ss -> ("DefList", Free $ ASTFun "List" $ Pure <$> ss)
        Table ss ->
            ("DefTable"
            , Free $ ASTFun "Table" [Free $ ASTRecord Recursive (fromFormulaOrList <$> Map.fromList ss) (fst <$> ss)]
            )

    optionalCellType = case cellType of
        Nothing -> ""
        Just t -> ' ' : ':' : ' ' : renderType t

    fromFormulaOrList :: Either String [String] -> Free ASTF String
    fromFormulaOrList (Left f) = Pure f
    fromFormulaOrList (Right l) = Free $ ASTFun "List" $ Pure <$> l

renderImports :: [String] -> String
renderImports is = "Import(" ++ intercalate ", " is ++ ")"

renderSheet :: Sheet -> String
renderSheet (Sheet [] _ s) = intercalate "\n" $                     renderCell <$> Map.elems s
renderSheet (Sheet is _ s) = intercalate "\n" $ renderImports is : (renderCell <$> Map.elems s)
