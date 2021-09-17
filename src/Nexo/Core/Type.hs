{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Nexo.Core.Type where

import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Type

data CoreExpr
    = CLit Value
    | CVar String
    | CRec (Map.Map String CoreExpr)
    | CApp (Either Op String) [((Int, Maybe Double), CoreExpr)]
    deriving (Show)

makeBaseFunctor ''CoreExpr
