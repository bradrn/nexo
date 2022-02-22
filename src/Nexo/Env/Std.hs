{-# LANGUAGE LambdaCase #-}

module Nexo.Env.Std where

import Data.Bifunctor (second)
import Data.List (sort)

import qualified Data.Map.Strict as Map

import Nexo.Core.Type
import Nexo.Interpret

stdFns :: Map.Map String (PType, Value e)
stdFns = Map.fromList $ fmap (second (second (VPrimClosure . PrimClosure)))
    [ ("If",
        ( Forall ["a"] $ CFun [CBool, CVarR "a", CVarR "a"] (CVarR "a")
        , \[cond, tcase, fcase] -> case cond of
            VBool True -> tcase
            VBool False -> fcase
            VNull -> VNull
            _ -> error "evalFun: bug in typechecker"))
    , ("Mean",
       ( Forall ["u"] $ CFun [CList (CNum $ CVarR "u")] (CNum $ CVarR "u")
       , handleNull $ \[VList list]-> VNum $ mean (map extractNum list)))
    , ("Avg",
       ( Forall ["u"] $ CFun [CList (CNum $ CVarR "u")] (CNum $ CVarR "u")
       , handleNull $ \[VList list]-> VNum $ mean (map extractNum list)))
    , ("PopStdDev" ,
       ( Forall ["u"] $ CFun [CList (CNum $ CVarR "u")] (CNum $ CVarR "u")
       , handleNull $ \[VList list]-> VNum $ popStdDev (map extractNum list)))
    , ("Median",
       ( Forall ["u"] $ CFun [CList (CNum $ CVarR "u")] (CNum $ CVarR "u")
       , handleNull $ \[VList list]-> VNum $ median (map extractNum list)))
    , ("Mode",
       ( Forall ["u"] $ CFun [CList (CNum $ CVarR "u")] (CNum $ CVarR "u")
       , handleNull $ \[VList list]-> VNum $ mode (map extractNum list)))
    , ("Sin",
       ( Forall [] $ CFun [CNum (cULeaf "rad")] (CNum cUno)
       , handleNull $ \[VNum n]-> VNum $ sin n))
    , ("Cos",
       ( Forall [] $ CFun [CNum (cULeaf "rad")] (CNum cUno)
       , handleNull $ \[VNum n]-> VNum $ cos n))
    , ("Tan",
       ( Forall [] $ CFun [CNum (cULeaf "rad")] (CNum cUno)
       , handleNull $ \[VNum n]-> VNum $ tan n))
    , ("InvSin",
       ( Forall [] $ CFun [CNum cUno] (CNum (cULeaf "rad"))
       , handleNull $ \[VNum n]-> VNum $ asin n))
    , ("InvCos",
       ( Forall [] $ CFun [CNum cUno] (CNum (cULeaf "rad"))
       , handleNull $ \[VNum n]-> VNum $ acos n))
    , ("InvTan",
       ( Forall [] $ CFun [CNum cUno] (CNum (cULeaf "rad"))
       , handleNull $ \[VNum n]-> VNum $ atan n))
    , ("Root",
       ( Forall [] $ CFun [CNum cUno, CNum cUno] (CNum cUno)
       , handleNull $ \[VNum n1, VNum n2] -> VNum $ n1**(1/n2)))
    , ("Power",
       ( Forall [] $ CFun [CNum cUno , CNum cUno] (CNum cUno)
       , handleNull $ \[VNum n1, VNum n2] -> VNum $ n1**n2))
    , ("List",
       (error "std: bug in typechecker: attempted to get type of List()", VList))
    , ("GetField" ,
       (error "std: bug in typechecker: attempted to get type of GetField(,)", handleNull $ \case
          [VRecord r, VText f] | Just v <- Map.lookup f r -> v
          [VTable r, VText f] | Just v <- Map.lookup f r -> VList v
          _ -> error "GetField(,): bug in typechecker"
      ))
    , ("="  , (Forall ["a"]      $ CFun [CVarR "a", CVarR "a"] (CVarR "a")                                        , handleNull $ \[v1     , v2     ] -> VBool $ v1 == v2))
    , ("<>" , (Forall ["a"]      $ CFun [CVarR "a", CVarR "a"] (CVarR "a")                                        , handleNull $ \[v1     , v2     ] -> VBool $ v1 /= v2))
    , ("+"  , (Forall ["u"]      $ CFun [CNum $ CUnit $ uVarR "u", CNum $ CUnit $ uVarR "u"] (CNum $ CUnit $ uVarR "u")                   , handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 + i2))
    , ("-"  , (Forall ["u"]      $ CFun [CNum $ CUnit $ uVarR "u", CNum $ CUnit $ uVarR "u"] (CNum $ CUnit $ uVarR "u")                   , handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 - i2))
    , ("*"  , (Forall ["u", "v"] $ CFun [CNum $ CUnit $ uVarR "u", CNum $ CUnit $ uVarR "v"] (CNum $ CUnit (1, Map.fromList [(Right (Rigid "u"), 1), (Right (Rigid "v"), 1)])),
               handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 * i2))
    , ("/"  , (Forall ["u", "v"] $ CFun [CNum $ CUnit $ uVarR "u", CNum $ CUnit $ uVarR "v"] (CNum $ CUnit (1, Map.fromList [(Right (Rigid "u"), 1), (Right (Rigid "v"), -1)])),
               handleNull $ \[VNum i1, VNum i2] -> VNum $ i1 / i2))
    , (">"  , (Forall ["u"]      $ CFun [CNum $ CUnit $ uVarR "u", CNum $ CUnit $ uVarR "u"] CBool                                , handleNull $ \[VNum i1, VNum i2] -> VBool $ i1 > i2))
    , ("<"  , (Forall ["u"]      $ CFun [CNum $ CUnit $ uVarR "u", CNum $ CUnit $ uVarR "u"] CBool                                , handleNull $ \[VNum i1, VNum i2] -> VBool $ i1 < i2))
    , ("&&" , (Forall []         $ CFun [CBool, CBool] CBool                                                      , handleNull $ \[VBool p, VBool q] -> VBool $ p && q))
    , ("||" , (Forall []         $ CFun [CBool, CBool] CBool                                                      , handleNull $ \[VBool p, VBool q] -> VBool $ p || q))
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
