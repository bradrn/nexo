{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}

module Brassica.Unit where

import Control.Applicative (liftA2)
import Data.Bifunctor (second)
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)

import qualified Data.Map.Strict as Map

data UnitDef
    = UName String
    | UPrefix String
    | UFactor Double
    | UMul UnitDef UnitDef
    | UDiv UnitDef UnitDef
    | UExp UnitDef Int
    | UVar String
    deriving (Show, Ord)

pattern Uno :: UnitDef
pattern Uno = UFactor 1

-- | Warning! Unless you know what you’re doing, it’s probably better
-- to use 'concords' when comparing units
deriving instance Eq UnitDef

makeBaseFunctor ''UnitDef

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

type Unit = (Double, Map.Map String Int)

simplify :: UnitDef -> Maybe Unit
simplify = fmap (second $ Map.filter (/=0)) . cata \case
    UNameF n -> expandName n
    UPrefixF p -> (, Map.empty) <$> lookupPrefix p
    UFactorF f -> Just (f, Map.empty)
    UMulF u v -> liftA2 mul u v
    UDivF u v -> liftA2 div' u v
    UExpF u x -> exp' x <$> u
  where
    mul (f, u) (g, v) = (f*g, Map.unionWith (+) u v)
    div' (f, u) (g, v) = (f/g, Map.unionWith (+) u $ negate <$> v)
    exp' x (f, u) = (f^x, (*x) <$> u)

expandName :: String -> Maybe (Double, Map.Map String Int)
expandName "s"   = Just (1, Map.singleton "s" 1)
expandName "m"   = Just (1, Map.singleton "m" 1)
expandName "g"   = Just (1, Map.singleton "g" 1)
expandName "A"   = Just (1, Map.singleton "A" 1)
expandName "mol" = Just (1, Map.singleton "mol" 1)
expandName "cd"  = Just (1, Map.singleton "cd" 1)
expandName "rad" = Just (1, Map.empty)
expandName "Hz"  = Just (1, Map.singleton "s" (-1))
expandName "N"   = Just (1000, Map.fromList [("g", 1), ("m", 1), ("s", -2)])
expandName "Pa"  = Just (1000, Map.fromList [("g", 1), ("m", -1), ("s", -2)])
expandName "J"   = Just (1000, Map.fromList [("g", 1), ("m", 2), ("s", -2)])
expandName "W"   = Just (1000, Map.fromList [("g", 1), ("m", 2), ("s", -3)])
expandName "C"   = Just (1, Map.fromList [("s", 1), ("A", 1)])
expandName "V"   = Just (1000, Map.fromList [("g", 1), ("m", 2), ("s", -3), ("A", -1)])
expandName "F"   = Just (0.001, Map.fromList [("g", -1), ("m", -2), ("s", 4), ("A", 2)])
expandName "Ω"   = Just (1000, Map.fromList [("g", 1), ("m", 2), ("s", -3), ("A", -2)])
expandName "min" = Just (60, Map.singleton "s" 1)
expandName "h"   = Just (3600, Map.singleton "s" 1)
expandName "d"   = Just (86400, Map.singleton "s" 1)
expandName "deg" = Just (pi/180, Map.empty)
expandName "ha"  = Just (10000, Map.singleton "m" 2)
expandName "L"   = Just (0.001, Map.singleton "m" 3)
expandName "t"   = Just (1000000, Map.singleton "g" 1)
expandName "in"  = Just (0.0254, Map.singleton "m" 1)
expandName "ft"  = Just (0.3048, Map.singleton "m" 1)
expandName "yd"  = Just (0.9144, Map.singleton "m" 1)
expandName "mi"  = Just (1609.344, Map.singleton "m" 1)
expandName "lb"  = Just (453.59237, Map.singleton "g" 1)
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
