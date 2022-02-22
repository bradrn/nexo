{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Nexo.Expr.Type where

import Control.Monad.Free (Free)
import Control.Monad.Trans.Free (FreeF(Free))
import Data.Deriving (deriveShow1, deriveEq1)
import Data.Fix (Fix(..))
import Data.Functor.Foldable (Recursive, Corecursive, Base, hoist)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Maybe (listToMaybe)

import qualified Data.Map.Strict as Map

type TVar = String

data Unit
    = ULeaf String
    | UFactor Double
    | UMul Unit Unit
    | UDiv Unit Unit
    | UExp Unit Int
    | UVar TVar
    deriving (Show, Ord)

pattern Uno :: Unit
pattern Uno = UFactor 1

-- | Warning! Unless you know what you’re doing, it’s probably better
-- to use 'concords' (from 'Nexo.Expr.Unit') when comparing units
deriving instance Eq Unit

makeBaseFunctor ''Unit

-- | Data type listing all the types in Nexo
data Type
    = TNum Unit
    | TBool
    | TText
    | TVar TVar
    | TFun [Type] Type
    | TList Type
    | TRecord (Map.Map String Type)
    | TTable (Map.Map String Type)
    deriving (Show, Eq, Ord)

meet :: Type -> Type -> Maybe Type
meet t u | t == u = Just t
meet (TList t) u = meet t u
meet t (TList u) = meet t u
meet _ _ = Nothing

meets :: [Type] -> Maybe Type
meets = foldr (\val acc -> acc >>= meet val) =<< listToMaybe

data Literal
    = Num Double
    | Bool Bool
    | Text String
    deriving (Show, Eq)

data Recursivity = Nonrecursive | Recursive
    deriving (Show, Eq)

data ASTF r
    = ASTLit Literal
    | ASTRecord Recursivity (Map.Map String r) [String]
    | ASTVar String
    | ASTLet String (Maybe Type) r r
    | ASTLam [String] r
    | ASTField r String
    | ASTFun String [r]
    | ASTOp String r r
    | ASTUnit r Unit
    | ASTTApp r Type
    | ASTNull
    deriving (Show, Functor)
deriveShow1 ''ASTF
deriveEq1 ''ASTF

type AST = Fix ASTF

data Atom
    = Lit Literal
    | Var String
    | Null
    deriving (Show, Eq)

data ExprF r
    = Atom Atom
    | Record Recursivity (Map.Map String r) [String]
    | FunApp r [r]
    | TypeApp r Type
    | UnitApp r Unit
    deriving (Show, Functor)
deriveShow1 ''ExprF
deriveEq1 ''ExprF

pattern NamedFunApp :: String -> [Fix ExprF] -> ExprF (Fix ExprF)
pattern NamedFunApp f args = FunApp (Fix (Atom (Var f))) args

type Expr = Fix ExprF

partialise :: (Recursive t, Corecursive t) => t -> Free (Base t) a
partialise = hoist Free
