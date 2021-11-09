{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Nexo.Interpret
       ( Value(..)
       , PrimClosure(..)
       , render
       , evalExpr
       ) where

import Data.Functor.Foldable (para)

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Foldable (for_)
import Data.List (transpose, intercalate)

import Nexo.Core.Type
import Nexo.Expr.Type
import Nexo.Env

data Value e
    = VNum Double
    | VBool Bool
    | VText String
    | VList [Value e]
    | VRecord (Map.Map String (Value e))
    | VClosure e [String] CoreExpr
    | VPrimClosure (PrimClosure e)
    deriving (Show)

newtype PrimClosure e = PrimClosure ([Value e] -> Value e)
instance Show (PrimClosure e) where
    show _ = "\\PC"

instance Eq (Value e) where
    (VNum n)    == (VNum n')    = n  == n'
    (VBool b)   == (VBool b')   = b  == b'
    (VText t)   == (VText t')   = t  == t'
    (VList vs)  == (VList vs')  = vs == vs'
    (VRecord r) == (VRecord r') = r  == r'
    -- This is a naughty instance: functions are not even equal to
    -- themselves! But I can’t see any other good way to do this
    _ == _ = False

render :: Value e -> String
render (VNum n) = show n
render (VBool b) = show b
render (VText s) = show s
render (VList vs) = "[" ++ intercalate "," (render <$> vs) ++ "]"
render (VRecord vs) =
    "(" ++ intercalate "," (renderField <$> Map.toList vs) ++ ")"
  where
    renderField (k,v) = k ++ ":" ++ render v
render VClosure{} = "λ…"
render VPrimClosure{} = "λ…"

fromLit :: Literal -> Value e
fromLit (LNum n) = VNum n
fromLit (LBool b) = VBool b
fromLit (LText t) = VText t

evalOp :: Op -> [Value e] -> Value e
evalOp OPlus  [VNum i1, VNum i2] = VNum $ i1 + i2
evalOp OMinus [VNum i1, VNum i2] = VNum $ i1 - i2
evalOp OTimes [VNum i1, VNum i2] = VNum $ i1 * i2
evalOp ODiv   [VNum i1, VNum i2] = VNum $ i1 / i2
evalOp OEq    [v1     , v2     ] = VBool $ v1 == v2
evalOp ONeq   [v1     , v2     ] = VBool $ v1 /= v2
evalOp OGt    [VNum i1, VNum i2] = VBool $ i1 > i2
evalOp OLt    [VNum i1, VNum i2] = VBool $ i1 < i2
evalOp OAnd   [VBool p, VBool q] = VBool $ p && q
evalOp OOr    [VBool p, VBool q] = VBool $ p || q
evalOp _ _ = error "evalApp: bug in typechecker"

broadcast :: MonadFail f => ([Value e] -> f (Value e)) -> [(Int, Value e)] -> f (Value e)
broadcast fn args
    | all ((0==) . fst) args = fn $ snd <$> args
    | otherwise = fmap VList $ traverse (broadcast fn) =<< unliftSplit args
  where
    unliftSplit :: MonadFail f => [(Int, Value e)] -> f [[(Int, Value e)]]
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

    transposeVLists :: MonadFail f => [(Int, Value e)] -> f [[(Int, Value e)]]
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

evalExpr :: (MonadEnv (Value e) e f, Scoped e, MonadFail f) => CoreExpr -> f (Value e)
evalExpr = para \case
    CLitF v -> pure $ fromLit v
    CVarF name -> lookupName name
    CLetF v (_, vx') (_, x') -> scope $ do
        vx <- vx'
        extend (v, vx)
        x'
    CLamF args (x, _) -> do
        env <- getEnv
        pure $ VClosure env args x
    CRecF xs -> VRecord <$> sequenceA (snd <$> xs)
    CAppF (Left  op) es -> broadcast (pure . evalOp op) =<< traverse liftTuple es
    CAppF (Right fn) es -> do
        v <- lookupName fn
        broadcast (fromClosure v) =<< traverse liftTuple es
  where
    liftTuple :: Functor f => (a, (x, f b)) -> f (a, b)
    liftTuple (a, (_, b)) = (a,) <$> b

    fromClosure
        :: ( MonadEnv (Value e) e m
           , MonadFail m
           , Scoped e)
        => Value e -> ([Value e] -> m (Value e))
    fromClosure (VClosure env' args x) vs = do
        env <- getEnv
        let innerEnv = env `addScope` env'
        withEnv innerEnv $ do
            for_ (zip args vs) extend
            evalExpr x
    fromClosure (VPrimClosure (PrimClosure f)) vs = pure $ f vs
    fromClosure _ _ = error "fromClosure: bug in typechecker"
