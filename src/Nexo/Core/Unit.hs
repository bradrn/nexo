{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TypeFamilies   #-}

module Nexo.Core.Unit where

import Control.Applicative (liftA2)
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata)

import qualified Data.Map.Strict as Map

import qualified Nexo.Expr.Type as Expr
import qualified Nexo.Core.Type as Core

-- | If the first unit can be converted to the second unit, returns
-- the conversion factor from the second to the first; else returns
-- 'Nothing'.
concord :: Core.Unit -> Core.Unit -> Maybe Double
concord u@(f, u') v@(g, v')
    | u == v = Just 1
    | u' == v' = Just (f/g)
    | otherwise = Nothing

simplify :: Expr.Unit -> Either Core.TypeError Core.Unit
simplify = fmap (second $ Map.filter (/=0)) . cata \case
    Expr.ULeafF l -> case expandName l of
        Just x -> Right x
        Nothing -> case lookupPrefix l of
            Just (l', f) -> maybe (Left $ Core.UnknownName l) Right $
                expandName l' <&> \(g, v) -> (f*g, v)
            Nothing -> Left $ Core.UnknownName l
    Expr.UFactorF f -> Right (f, Map.empty)
    Expr.UMulF u v -> liftA2 mul u v
    Expr.UDivF u v -> liftA2 div' u v
    Expr.UExpF u x -> exp' x <$> u
    Expr.UVarF v -> Right (1, Map.singleton (Right $ Core.Rigid v) 1)
  where
    mul (f, u) (g, v) = (f*g, Map.unionWith (+) u v)
    div' (f, u) (g, v) = (f/g, Map.unionWith (+) u $ negate <$> v)
    exp' x (f, u) = (f^^x, (*x) <$> u)

expandName :: String -> Maybe (Double, Map.Map (Either String Core.TVar) Int)
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

lookupPrefix :: String -> Maybe (String, Double)
lookupPrefix ('Y':s) = Just (s, 1000000000000000000000000)
lookupPrefix ('Z':s) = Just (s, 1000000000000000000000)
lookupPrefix ('E':s) = Just (s, 1000000000000000000)
lookupPrefix ('P':s) = Just (s, 1000000000000000)
lookupPrefix ('T':s) = Just (s, 1000000000000)
lookupPrefix ('G':s) = Just (s, 1000000000)
lookupPrefix ('M':s) = Just (s, 1000000)
lookupPrefix ('k':s) = Just (s, 1000)
lookupPrefix ('h':s) = Just (s, 100)
lookupPrefix ('d':'a':s) = Just (s, 10)
lookupPrefix ('d':s) = Just (s, 0.1)
lookupPrefix ('c':s) = Just (s, 0.01)
lookupPrefix ('m':s) = Just (s, 0.001)
lookupPrefix ('μ':s) = Just (s, 0.000001)
lookupPrefix ('u':s) = Just (s, 0.000001)
lookupPrefix ('n':s) = Just (s, 0.000000001)
lookupPrefix ('p':s) = Just (s, 0.000000000001)
lookupPrefix ('f':s) = Just (s, 0.000000000000001)
lookupPrefix ('a':s) = Just (s, 0.000000000000000001)
lookupPrefix ('z':s) = Just (s, 0.000000000000000000001)
lookupPrefix ('y':s) = Just (s, 0.000000000000000000000001)
lookupPrefix _   = Nothing
