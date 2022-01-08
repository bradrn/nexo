{-# LANGUAGE DeriveFunctor      #-}
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

data TVar
    = Rigid String
    | Undetermined String  -- used only internally in type inference algorithm
                           -- ideally this would be in a separate type, but this seems easier
    deriving (Show, Eq, Ord)

data UnitDef
    = UName String
    | UPrefix String
    | UFactor Double
    | UMul UnitDef UnitDef
    | UDiv UnitDef UnitDef
    | UExp UnitDef Int
    | UVar TVar
    deriving (Show, Ord)

pattern Uno :: UnitDef
pattern Uno = UFactor 1

-- | Warning! Unless you know what you’re doing, it’s probably better
-- to use 'concords' (from 'Nexo.Expr.Unit') when comparing units
deriving instance Eq UnitDef

makeBaseFunctor ''UnitDef

-- | Data type listing all the types in Nexo
data Type
    = TNum UnitDef
    | TBool
    | TText
    | TVar TVar
    | TFun [Type] Type
    | TList Type
    | TRecord (Map.Map String Type)
    | TTable (Map.Map String Type)
    deriving (Show, Eq, Ord)

pattern TVarR :: String -> Type
pattern TVarR a = TVar (Rigid a)

pattern UVarR :: String -> UnitDef
pattern UVarR a = UVar (Rigid a)

data PType = Forall [String] [String] Type
    deriving (Show)

-- | Warning: only use this in tests! This does not check for type
-- variable equivalence
deriving instance Eq PType

meet :: Type -> Type -> Maybe Type
meet t u | t == u = Just t
meet (TList t) u = meet t u
meet t (TList u) = meet t u
meet _ _ = Nothing

meets :: [Type] -> Maybe Type
meets = foldr (\val acc -> acc >>= meet val) =<< listToMaybe

data Literal
    = LNum Double
    | LBool Bool
    | LText String
    deriving (Show, Eq)

-- | Operators
data Op
    = OPlus     -- ^ Plus
    | OMinus    -- ^ Minus
    | OTimes    -- ^ Times
    | ODiv      -- ^ Division
    | OEq       -- ^ Equals
    | ONeq      -- ^ Not Equals
    | OGt       -- ^ Greater Than
    | OLt       -- ^ Less Than
    | OAnd      -- ^ Logical And
    | OOr       -- ^ Logical Or
    deriving (Show, Eq, Enum, Bounded)

data Recursivity = Nonrecursive | Recursive
    deriving (Show, Eq)

data ExprF r
    = XLit Literal
    | XList [r]
    | XRecord Recursivity (Map.Map String r) [String]
    | XTable r
    | XVar String
    | XLet String (Maybe PType) r r
    | XLam [String] r
    | XField r String
    | XFun String [r]
    | XOp Op r r
    | XUnit r UnitDef
    | XTApp r PType
    | XNull
    deriving (Show, Functor)
deriveShow1 ''ExprF
deriveEq1 ''ExprF

type Expr = Fix ExprF

partialise :: (Recursive t, Corecursive t) => t -> Free (Base t) a
partialise = hoist Free
