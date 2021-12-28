{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Nexo.Expr.Type where

import Data.Deriving (deriveShow1)
import Data.Fix (Fix(..))
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Unit

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

type TVar = String

data PType = Forall [TVar] [TVar] Type
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
    deriving (Show)

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
    deriving (Show)

data ExprF r
    = XLit Literal
    | XList [r]
    | XRecord (Map.Map String r)
    | XTable (Map.Map String r) [String]
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

type Expr = Fix ExprF
