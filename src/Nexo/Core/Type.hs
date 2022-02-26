{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Nexo.Core.Type where

import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.Map.Strict as Map

import qualified Nexo.Expr.Type as Expr

data Expr
    = Lit Expr.Literal
    | Var String
    | Let String Expr Expr
    | Lam [String] Expr
    | Rec Expr.Recursivity [(String, Expr)]
    | Tab Expr
    | App Expr [(Int, Expr)]
    | Null
    deriving (Show, Eq)

makeBaseFunctor ''Expr

data TVar
    = Rigid String
    | Undetermined String  -- used only internally in type inference algorithm
                           -- ideally this would be in a separate type, but this seems easier
    deriving (Show, Eq, Ord)

data Kind
    = Type
    | Unit
    | KVar String
    deriving (Show, Eq, Ord)

-- | A unit in simplified representation: a factor multiplied by a map
-- from base units ('Left' case) or type variables ('Right' case) to
-- exponents.
type Unit = (Double, Map.Map (Either String TVar) Int)

uVarR :: String -> Unit
uVarR v = (1, Map.singleton (Right $ Rigid v) 1)

data Type
    = TNum Type
    | TBool
    | TText
    | TVar TVar
    | TFun [Type] Type
    | TList Type
    | TRecord (Map.Map String Type)
    | TTable (Map.Map String Type)
    | TUnit Unit
    deriving (Show, Eq, Ord)

pattern TVarR :: String -> Type
pattern TVarR a = TVar (Rigid a)

tUno :: Type
tUno = TUnit (1, Map.empty)

tULeaf :: String -> Type
tULeaf s = TUnit (1, Map.singleton (Left s) 1)

data PType = Forall [String] Type
    deriving (Show)

-- | Warning: only use this in tests! This does not check for type
-- variable equivalence
deriving instance Eq PType

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
    | ArgumentOfFunction Expr
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
