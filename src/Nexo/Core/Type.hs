{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Nexo.Core.Type where

import Data.Functor.Foldable.TH (makeBaseFunctor)

import Nexo.Expr.Type

data CoreExpr
    = CLit Literal
    | CVar String
    | CLet String CoreExpr CoreExpr
    | CLam [String] CoreExpr
    | CRec Recursivity [(String, CoreExpr)]
    | CTab CoreExpr
    | CApp (Either Op String) [(Int, CoreExpr)]
    | CNull
    deriving (Show)

makeBaseFunctor ''CoreExpr
