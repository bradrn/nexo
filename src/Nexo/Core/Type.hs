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
    | CApp CoreExpr [(Int, CoreExpr)]
    | CNull
    deriving (Show, Eq)

makeBaseFunctor ''CoreExpr

data TypeError
    = UnknownName String
    | WrongNumberOfArguments String
    | RecordFieldAbsent String
    | TableColumnsUnknown Type
    | LambdaArgumentNotVariable
    | TypeMismatch Context Mismatch
    | KindMismatch
    deriving (Show, Eq)

data Context
    = TypeSpecification
    | ArgumentOfFunction CoreExpr
    | ListElement
    | RecordField String
    | TableColumnNotList String
    | UnitAp
    deriving (Show, Eq)

data Mismatch = Mismatch
    { tSupplied :: Type
    , tDeclared :: Type
    }
    deriving (Show, Eq)
