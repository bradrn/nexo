{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Nexo.Core.Type where

import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.Map.Strict as Map

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

data CoreVar
    = Rigid String
    | Undetermined String  -- used only internally in type inference algorithm
                           -- ideally this would be in a separate type, but this seems easier
    deriving (Show, Eq, Ord)

-- | A unit in simplified representation: a factor multiplied by a map
-- from base units ('Left' case) or type variables ('Right' case) to
-- exponents.
type Unit = (Double, Map.Map (Either String CoreVar) Int)

uVarR :: String -> Unit
uVarR v = (1, Map.singleton (Right $ Rigid v) 1)

data CoreType
    = CNum CoreType
    | CBool
    | CText
    | CTVar CoreVar
    | CFun [CoreType] CoreType
    | CList CoreType
    | CRecord (Map.Map String CoreType)
    | CTable (Map.Map String CoreType)
    | CUnit Unit
    deriving (Show, Eq, Ord)

pattern CVarR :: String -> CoreType
pattern CVarR a = CTVar (Rigid a)

cUno :: CoreType
cUno = CUnit (1, Map.empty)

cULeaf :: String -> CoreType
cULeaf s = CUnit (1, Map.singleton (Left s) 1)

data PType = Forall [String] CoreType
    deriving (Show)

-- | Warning: only use this in tests! This does not check for type
-- variable equivalence
deriving instance Eq PType

data TypeError
    = UnknownName String
    | WrongNumberOfArguments String
    | RecordFieldAbsent String
    | TableColumnsUnknown CoreType
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
    { tSupplied :: CoreType
    , tDeclared :: CoreType
    }
    deriving (Show, Eq)
