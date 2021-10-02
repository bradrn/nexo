{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Nexo.Interpret (evalExpr) where

import Data.Functor.Foldable (cata)

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.List (transpose, sort)

import Nexo.Core.Type
import Nexo.Expr.Type

extractNum :: Value -> Double
extractNum (VNum n) = n
extractNum _ = error "unexpected value"

mean :: [Double] -> Double
mean list = sum list / fromIntegral (length list)  -- same as VNum (sum (map extractNum list) / fromIntegral (length list))

popStdDev :: [Double] -> Double
popStdDev list =
    let mu = mean list
        diffSquared = map (square . subtract mu) list
        meanSquared = mean diffSquared
    in sqrt meanSquared
  where
    -- could just do (^2), but that gives a defaulting warning
    square x = x * x

median :: [Double] -> Double
median list = 
    let sortList = sort list
        x = sortList !! (length sortList `quot` 2)
        y = sortList !! ((length sortList `quot` 2) - 1)
    in
        if odd (length sortList)
        then x
        else (x + y) / 2

mode :: Ord a => [a] -> a
mode = getMostFrequent . foldr (\val -> Map.insertWith (+) val 1) Map.empty
  where
    getMostFrequent :: Ord a => Map.Map a Int -> a
    getMostFrequent freqs =
        let highestFreq = maximum $ Map.elems freqs
        in head $ Map.keys $ Map.filter (==highestFreq) freqs

evalApp :: Either Op String -> [Value] -> Value
evalApp (Right "If") [cond, tcase, fcase] = case cond of -- If logical Function
    VBool True -> tcase
    VBool False -> fcase
    _ -> error "evalFun: bug in typechecker"
evalApp (Right "Mean") [VList list] = VNum $ mean (map extractNum list)
evalApp (Right "Avg") [VList list] = VNum $ mean (map extractNum list)
evalApp (Right "PopStdDev") [VList list] = VNum $ popStdDev (map extractNum list)
evalApp (Right "Median") [VList list] = VNum $ median (map extractNum list)
evalApp (Right "Mode") [VList list] = VNum $ mode (map extractNum list)
evalApp (Right "Sin") [VNum n] = VNum $ sin n
evalApp (Right "Cos") [VNum n] = VNum $ cos n
evalApp (Right "Tan") [VNum n] = VNum $ tan n
evalApp (Right "InvSin") [VNum n] = VNum $ asin n
evalApp (Right "InvCos") [VNum n] = VNum $ acos n
evalApp (Right "InvTan") [VNum n] = VNum $ atan n
evalApp (Right "Root") [VNum n1, VNum n2] = VNum $ n1**(1/n2)
evalApp (Right "Power") [VNum n1, VNum n2] = VNum $ n1**n2
evalApp (Right "List") vs = VList vs                    -- List function used by Haskell for making lists
evalApp (Right "GetField") [VRecord r, VText f]
    | Just v <- Map.lookup f r = v
evalApp (Left OPlus ) [VNum i1, VNum i2] = VNum $ i1 + i2
evalApp (Left OMinus) [VNum i1, VNum i2] = VNum $ i1 - i2
evalApp (Left OTimes) [VNum i1, VNum i2] = VNum $ i1 * i2
evalApp (Left ODiv  ) [VNum i1, VNum i2] = VNum $ i1 / i2
evalApp (Left OEq   ) [v1     , v2     ] = VBool $ v1 == v2
evalApp (Left ONeq  ) [v1     , v2     ] = VBool $ v1 /= v2
evalApp (Left OGt   ) [VNum i1, VNum i2] = VBool $ i1 > i2
evalApp (Left OLt   ) [VNum i1, VNum i2] = VBool $ i1 < i2
evalApp (Left OAnd  ) [VBool p, VBool q] = VBool $ p && q
evalApp (Left OOr   ) [VBool p, VBool q] = VBool $ p || q
evalApp _ _ = error "evalApp: bug in typechecker"

broadcast :: MonadFail f => ([Value] -> Value) -> [(Int, Value)] -> f Value
broadcast fn args
    | all ((0==) . fst) args = pure $ fn $ snd <$> args
    | otherwise = fmap VList $ traverse (broadcast fn) =<< unliftSplit args
  where
    unliftSplit :: MonadFail f => [(Int, Value)] -> f [[(Int, Value)]]
    unliftSplit args' =
        let levels = fst <$> args'
            maxlevel = if null levels then 0 else maximum levels
            fills = catMaybes $ zipWith
                (\level x -> if level == maxlevel then Just x else Nothing)
                levels args'
            placeholders = zipWith
                (\level x -> if level == maxlevel then Nothing else Just x)
                levels args'
        in fmap (replaceIn placeholders) <$> transposeVLists fills

    transposeVLists :: MonadFail f => [(Int, Value)] -> f [[(Int, Value)]]
    transposeVLists = transpose' . fmap extractVList
      where
        extractVList (i, VList l) = (i-1,) <$> l
        extractVList _ = error "broadcast: bug in typechecker"

        transpose' [] = pure []
        transpose' ls =
            let lens = length <$> ls in
                if all (==head lens) lens
                then pure $ transpose ls
                else fail "#LENGTH"

    replaceIn :: [Maybe a] -> [a] -> [a]
    replaceIn [] _ = []
    replaceIn (Nothing:is) (r:rs) = r : replaceIn is rs
    replaceIn (Nothing:_) _ = error "broadcast: bug in unlifter"
    replaceIn (Just i:is) rs = i : replaceIn is rs

evalExpr :: MonadFail f => (String -> f Value) -> CoreExpr -> f Value
evalExpr lookupName = cata \case
    CLitF v -> pure v
    CVarF name -> lookupName name
    CRecF xs -> VRecord <$> sequenceA xs
    CAppF fn es -> broadcast (evalApp fn) =<< traverse liftTuple es
  where
    liftTuple :: Functor f => (a, f b) -> f (a, b)
    liftTuple (a, b) = (a,) <$> b
