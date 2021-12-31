{-# LANGUAGE LambdaCase #-}

module Nexo.Env.Std where

import Nexo.Expr.Type
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Nexo.Interpret
import Data.Bifunctor (second)

stdFnTs :: [(String, PType)]
stdFnTs =
    [ ("If"        , Forall ["a"] [] $ TFun [TBool, TVarR "a", TVarR "a"] (TVarR "a"))
    , ("Mean"      , Forall [] ["u"] $ TFun [TList (TNum $ UVarR "u")] (TNum $ UVarR "u"))
    , ("Avg"       , Forall [] ["u"] $ TFun [TList (TNum $ UVarR "u")] (TNum $ UVarR "u"))
    , ("PopStdDev" , Forall [] ["u"] $ TFun [TList (TNum $ UVarR "u")] (TNum $ UVarR "u"))
    , ("Median"    , Forall [] ["u"] $ TFun [TList (TNum $ UVarR "u")] (TNum $ UVarR "u"))
    , ("Mode"      , Forall [] ["u"] $ TFun [TList (TNum $ UVarR "u")] (TNum $ UVarR "u"))
    , ("Sin"       , Forall [] []    $ TFun [TNum (UName "rad")] (TNum Uno))
    , ("Cos"       , Forall [] []    $ TFun [TNum (UName "rad")] (TNum Uno))
    , ("Tan"       , Forall [] []    $ TFun [TNum (UName "rad")] (TNum Uno))
    , ("InvSin"    , Forall [] []    $ TFun [TNum Uno] (TNum (UName "rad")))
    , ("InvCos"    , Forall [] []    $ TFun [TNum Uno] (TNum (UName "rad")))
    , ("InvTan"    , Forall [] []    $ TFun [TNum Uno] (TNum (UName "rad")))
    , ("Root"      , Forall [] []    $ TFun [TNum Uno, TNum Uno] (TNum Uno))
    , ("Power"     , Forall [] []    $ TFun [TNum Uno, TNum Uno] (TNum Uno))
    ]

stdFnVals :: [(String, Value e)]
stdFnVals = fmap (second $ VPrimClosure . PrimClosure)
    [ ("If", \[cond, tcase, fcase] -> case cond of -- If logical Function
        VBool True -> tcase
        VBool False -> fcase
        VNull -> VNull
        _ -> error "evalFun: bug in typechecker")
    , ("Mean"     , handleNull $ \[VList list]       -> VNum $ mean (map extractNum list))
    , ("Avg"      , handleNull $ \[VList list]       -> VNum $ mean (map extractNum list))
    , ("PopStdDev", handleNull $ \[VList list]       -> VNum $ popStdDev (map extractNum list))
    , ("Median"   , handleNull $ \[VList list]       -> VNum $ median (map extractNum list))
    , ("Mode"     , handleNull $ \[VList list]       -> VNum $ mode (map extractNum list))
    , ("Sin"      , handleNull $ \[VNum n]           -> VNum $ sin n)
    , ("Cos"      , handleNull $ \[VNum n]           -> VNum $ cos n)
    , ("Tan"      , handleNull $ \[VNum n]           -> VNum $ tan n)
    , ("InvSin"   , handleNull $ \[VNum n]           -> VNum $ asin n)
    , ("InvCos"   , handleNull $ \[VNum n]           -> VNum $ acos n)
    , ("InvTan"   , handleNull $ \[VNum n]           -> VNum $ atan n)
    , ("Root"     , handleNull $ \[VNum n1, VNum n2] -> VNum $ n1**(1/n2))
    , ("Power"    , handleNull $ \[VNum n1, VNum n2] -> VNum $ n1**n2)
    , ("List"     , VList)                   -- List function used by Haskell for making lists
    , ("GetField" , handleNull $ \case
          [VRecord r, VText f] | Just v <- Map.lookup f r -> v
          [VTable  r, VText f] | Just v <- Map.lookup f r -> VList v
          _ -> error "GetField(,): bug in typechecker"
      ) ]

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
