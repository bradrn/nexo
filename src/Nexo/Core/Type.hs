{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Nexo.Core.Type where

import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Type

data CoreExpr
    = CLit Literal
    | CVar String
    | CRec (Map.Map String CoreExpr)
    | CApp (Either Op String) [(Int, CoreExpr)]
    deriving (Show)

makeBaseFunctor ''CoreExpr
