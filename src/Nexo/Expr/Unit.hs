{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}

module Nexo.Expr.Unit where

import Control.Applicative (liftA2)
import Data.Bifunctor (second)
import Data.Functor.Foldable (cata)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Type

-- | If the first unit can be converted to the second unit, returns
-- the conversion factor from the second to the first; else returns
-- 'Nothing'.
concord :: UnitDef -> UnitDef -> Maybe Double
concord u v
    | u == v = Just 1
    | Just (f, u') <- simplify u
    , Just (g, v') <- simplify v
    , u' == v' = Just (f/g)
    | otherwise = Nothing

-- | Return the first unit in the list only if they all concord.
concords :: [UnitDef] -> Maybe UnitDef
concords [] = Nothing
concords (u:us) = u <$ traverse (concord u) us

-- | A unit in simplified representation: a factor multiplied by a map
-- from base units ('Left' case) or type variables ('Right' case) to
-- exponents.
type Unit = (Double, Map.Map (Either String TVar) Int)

unitToDef :: Unit -> UnitDef
unitToDef (f, u) =
    if | f==1, (u0:us) <- Map.toList u
         -> foldr (UMul . term) (term u0) us
       | otherwise
         -> foldr (UMul . term) (UFactor f) (Map.toList u)
  where
    term :: (Either String TVar, Int) -> UnitDef
    term (Left name, 1) = UName name
    term (Right var, 1) = UVar var
    term (Left name, n) = UExp (UName name) n
    term (Right var, n) = UExp (UVar var) n

simplify :: UnitDef -> Maybe Unit
simplify = fmap (second $ Map.filter (/=0)) . cata \case
    UNameF n -> expandName n
    UPrefixF p -> (, Map.empty) <$> lookupPrefix p
    UFactorF f -> Just (f, Map.empty)
    UMulF u v -> liftA2 mul u v
    UDivF u v -> liftA2 div' u v
    UExpF u x -> exp' x <$> u
    UVarF v -> Just (1, Map.singleton (Right v) 1)
  where
    mul (f, u) (g, v) = (f*g, Map.unionWith (+) u v)
    div' (f, u) (g, v) = (f/g, Map.unionWith (+) u $ negate <$> v)
    exp' x (f, u) = (f^^x, (*x) <$> u)

expandName :: String -> Maybe (Double, Map.Map (Either String TVar) Int)
expandName "s"   = Just (1, Map.singleton (Left "s") 1)
expandName "m"   = Just (1, Map.singleton (Left "m") 1)
expandName "g"   = Just (1, Map.singleton (Left "g") 1)
expandName "A"   = Just (1, Map.singleton (Left "A") 1)
expandName "mol" = Just (1, Map.singleton (Left "mol") 1)
expandName "cd"  = Just (1, Map.singleton (Left "cd") 1)
expandName "rad" = Just (1, Map.empty)
expandName "Hz"  = Just (1, Map.singleton (Left "s") (-1))
expandName "N"   = Just (1000, Map.fromList [(Left "g", 1), (Left "m", 1), (Left "s", -2)])
expandName "Pa"  = Just (1000, Map.fromList [(Left "g", 1), (Left "m", -1), (Left "s", -2)])
expandName "J"   = Just (1000, Map.fromList [(Left "g", 1), (Left "m", 2), (Left "s", -2)])
expandName "W"   = Just (1000, Map.fromList [(Left "g", 1), (Left "m", 2), (Left "s", -3)])
expandName "C"   = Just (1, Map.fromList [(Left "s", 1), (Left "A", 1)])
expandName "V"   = Just (1000, Map.fromList [(Left "g", 1), (Left "m", 2), (Left "s", -3), (Left "A", -1)])
expandName "F"   = Just (0.001, Map.fromList [(Left "g", -1), (Left "m", -2), (Left "s", 4), (Left "A", 2)])
expandName "Ω"   = Just (1000, Map.fromList [(Left "g", 1), (Left "m", 2), (Left "s", -3), (Left "A", -2)])
expandName "min" = Just (60, Map.singleton (Left "s") 1)
expandName "h"   = Just (3600, Map.singleton (Left "s") 1)
expandName "d"   = Just (86400, Map.singleton (Left "s") 1)
expandName "deg" = Just (pi/180, Map.empty)
expandName "ha"  = Just (10000, Map.singleton (Left "m") 2)
expandName "L"   = Just (0.001, Map.singleton (Left "m") 3)
expandName "t"   = Just (1000000, Map.singleton (Left "g") 1)
expandName "in"  = Just (0.0254, Map.singleton (Left "m") 1)
expandName "ft"  = Just (0.3048, Map.singleton (Left "m") 1)
expandName "yd"  = Just (0.9144, Map.singleton (Left "m") 1)
expandName "mi"  = Just (1609.344, Map.singleton (Left "m") 1)
expandName "lb"  = Just (453.59237, Map.singleton (Left "g") 1)
expandName _     = Nothing

lookupPrefix :: String -> Maybe Double
lookupPrefix "Y" = Just 1000000000000000000000000
lookupPrefix "Z" = Just 1000000000000000000000
lookupPrefix "E" = Just 1000000000000000000
lookupPrefix "P" = Just 1000000000000000
lookupPrefix "T" = Just 1000000000000
lookupPrefix "G" = Just 1000000000
lookupPrefix "M" = Just 1000000
lookupPrefix "k" = Just 1000
lookupPrefix "h" = Just 100
lookupPrefix "da"= Just 10
lookupPrefix "d" = Just 0.1
lookupPrefix "c" = Just 0.01
lookupPrefix "m" = Just 0.001
lookupPrefix "μ" = Just 0.000001
lookupPrefix "u" = Just 0.000001
lookupPrefix "n" = Just 0.000000001
lookupPrefix "p" = Just 0.000000000001
lookupPrefix "f" = Just 0.000000000000001
lookupPrefix "a" = Just 0.000000000000000001
lookupPrefix "z" = Just 0.000000000000000000001
lookupPrefix "y" = Just 0.000000000000000000000001
lookupPrefix _   = Nothing
