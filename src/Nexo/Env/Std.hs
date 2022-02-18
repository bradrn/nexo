{-# LANGUAGE LambdaCase #-}

module Nexo.Env.Std where

import Nexo.Expr.Type
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Nexo.Interpret
import Data.Bifunctor (second)

stdFns :: Map.Map String (PType, Value e)
stdFns = Map.fromList $ fmap (second (second (VPrimClosure . PrimClosure)))
    [ ("If",
        ( Forall ["a"] [] $ TFun [TBool, TVarR "a", TVarR "a"] (TVarR "a")
        , \[cond, tcase, fcase] -> case cond of
            VBool True -> tcase
            VBool False -> fcase
            VNull -> VNull
            _ -> error "evalFun: bug in typechecker"))
    , ("Mean",
       ( Forall [] ["u"] $ TFun [TList (TUNum $ UVarR "u")] (TUNum $ UVarR "u")
       , handleNull $ \[VList list]-> VNum $ mean (map extractNum list)))
    , ("Avg",
       ( Forall [] ["u"] $ TFun [TList (TUNum $ UVarR "u")] (TUNum $ UVarR "u")
       , handleNull $ \[VList list]-> VNum $ mean (map extractNum list)))
    , ("PopStdDev" ,
       ( Forall [] ["u"] $ TFun [TList (TUNum $ UVarR "u")] (TUNum $ UVarR "u")
       , handleNull $ \[VList list]-> VNum $ popStdDev (map extractNum list)))
    , ("Median",
       ( Forall [] ["u"] $ TFun [TList (TUNum $ UVarR "u")] (TUNum $ UVarR "u")
       , handleNull $ \[VList list]-> VNum $ median (map extractNum list)))
    , ("Mode",
       ( Forall [] ["u"] $ TFun [TList (TUNum $ UVarR "u")] (TUNum $ UVarR "u")
       , handleNull $ \[VList list]-> VNum $ mode (map extractNum list)))
    , ("Sin",
       ( Forall [] [] $ TFun [TUNum (ULeaf "rad")] (TUNum Uno)
       , handleNull $ \[VNum n]-> VNum $ sin n))
    , ("Cos",
       ( Forall [] [] $ TFun [TUNum (ULeaf "rad")] (TUNum Uno)
       , handleNull $ \[VNum n]-> VNum $ cos n))
    , ("Tan",
       ( Forall [] [] $ TFun [TUNum (ULeaf "rad")] (TUNum Uno)
       , handleNull $ \[VNum n]-> VNum $ tan n))
    , ("InvSin",
       ( Forall [] [] $ TFun [TUNum Uno] (TUNum (ULeaf "rad"))
       , handleNull $ \[VNum n]-> VNum $ asin n))
    , ("InvCos",
       ( Forall [] [] $ TFun [TUNum Uno] (TUNum (ULeaf "rad"))
       , handleNull $ \[VNum n]-> VNum $ acos n))
    , ("InvTan",
       ( Forall [] [] $ TFun [TUNum Uno] (TUNum (ULeaf "rad"))
       , handleNull $ \[VNum n]-> VNum $ atan n))
    , ("Root",
       ( Forall [] [] $ TFun [TUNum Uno, TUNum Uno] (TUNum Uno)
       , handleNull $ \[VNum n1, VNum n2] -> VNum $ n1**(1/n2)))
    , ("Power",
       ( Forall [] [] $ TFun [TUNum Uno , TUNum Uno] (TUNum Uno)
       , handleNull $ \[VNum n1, VNum n2] -> VNum $ n1**n2))
    , ("List",
       (error "std: bug in typechecker: attempted to get type of List()", VList))
    , ("GetField" ,
       (error "std: bug in typechecker: attempted to get type of GetField(,)", handleNull $ \case
          [VRecord r, VText f] | Just v <- Map.lookup f r -> v
          [VTable r, VText f] | Just v <- Map.lookup f r -> VList v
          _ -> error "GetField(,): bug in typechecker"
      ))
    , ("="  , (Forall [] ["a"]      $ TFun [TVarR "a", TVarR "a"] (TVarR "a")                                        , handleNull $ \[v1     , v2     ] -> VBool $ v1 == v2))
    , ("<>" , (Forall [] ["a"]      $ TFun [TVarR "a", TVarR "a"] (TVarR "a")                                        , handleNull $ \[v1     , v2     ] -> VBool $ v1 /= v2))
    , ("+"  , (Forall [] ["u"]      $ TFun [TUNum $ UVarR "u", TUNum $ UVarR "u"] (TUNum $ UVarR "u")                   , handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 + i2))
    , ("-"  , (Forall [] ["u"]      $ TFun [TUNum $ UVarR "u", TUNum $ UVarR "u"] (TUNum $ UVarR "u")                   , handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 - i2))
    , ("*"  , (Forall [] ["u", "v"] $ TFun [TUNum $ UVarR "u", TUNum $ UVarR "v"] (TUNum $ UMul (UVarR "u") (UVarR "v")), handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 * i2))
    , ("/"  , (Forall [] ["u", "v"] $ TFun [TUNum $ UVarR "u", TUNum $ UVarR "v"] (TUNum $ UDiv (UVarR "u") (UVarR "v")), handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 / i2))
    , (">"  , (Forall [] ["u"]      $ TFun [TUNum $ UVarR "u", TUNum $ UVarR "u"] TBool                                , handleNull $ \[VNum i1, VNum i2] -> VBool $ i1 > i2))
    , ("<"  , (Forall [] ["u"]      $ TFun [TUNum $ UVarR "u", TUNum $ UVarR "u"] TBool                                , handleNull $ \[VNum i1, VNum i2] -> VBool $ i1 < i2))
    , ("&&" , (Forall [] []         $ TFun [TBool, TBool] TBool                                                      , handleNull $ \[VBool p, VBool q] -> VBool $ p && q))
    , ("||" , (Forall [] []         $ TFun [TBool, TBool] TBool                                                      , handleNull $ \[VBool p, VBool q] -> VBool $ p || q))
    ]

handleNull :: ([Value e] -> Value e) -> [Value e] -> Value e
handleNull f = \case
    v | any (\case VNull -> True; _ -> False) v -> VNull
      | otherwise -> f v

extractNum :: Value e -> Double
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
