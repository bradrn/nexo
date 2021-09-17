{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

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
    -- | TFun FunType
    | TList Type
    | TRecord (Map.Map String Type)
    deriving (Show, Eq, Ord)

type TVar = String
data FunType = FunType [Type] Type
    deriving (Show, Eq, Ord)

meet :: Type -> Type -> Maybe Type
meet t u | t == u = Just t
meet (TList t) u = meet t u
meet t (TList u) = meet t u
meet _ _ = Nothing

meets :: [Type] -> Maybe Type
meets = foldr (\val acc -> acc >>= meet val) =<< listToMaybe

data Value
    = VNum Double
    | VBool Bool
    | VText String
    | VList [Value]
    | VRecord (Map.Map String Value)
    deriving (Show, Eq)

render :: Value -> String
render (VNum n) = show n
render (VBool b) = show b
render (VText s) = show s
render (VList vs) = "[" ++ intercalate "," (render <$> vs) ++ "]"
render (VRecord vs) =
    "(" ++ intercalate "," (renderField <$> Map.toList vs) ++ ")"
  where
    renderField (k,v) = k ++ ":" ++ render v

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
    = XLit Value
    | XList [r]
    | XRecord (Map.Map String r)
    | XVar String
    | XField r String
    | XFun String [r]
    | XOp Op r r
    | XUnit r UnitDef
    deriving (Show, Functor)
deriveShow1 ''ExprF

type Expr = Fix ExprF

-- | An expression which returns zero. Useful when you need some Expr
-- but don’t care which one (e.g. for C interop).
zeroExpr :: Expr
zeroExpr = Fix $ XLit $ VNum 0

