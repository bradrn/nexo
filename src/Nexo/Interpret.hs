{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Nexo.Interpret
       ( Value(..)
       , PrimClosure(..)
       , RuntimeError(..)
       , render
       , evalExpr
       ) where

import Control.Monad (join)
import Control.Monad.Except (MonadError(throwError))
import Data.Bifunctor (second)
import Data.Functor.Foldable (para)

import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Foldable (for_, traverse_)
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
    | VTable (Map.Map String [Value e])
    | VClosure e [String] CoreExpr
    | VPrimClosure (PrimClosure e)
    | VNull
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
    (VTable t)  == (VTable t')  = t  == t'
    VNull       == VNull        = True
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
render (VTable vs) =
    "Table(" ++ intercalate "," (renderField <$> Map.toList vs) ++ ")"
  where
    renderField (k,v) = k ++ ":" ++ render (VList v)
render VClosure{} = "λ…"
render VPrimClosure{} = "λ…"
render VNull = "—"

fromLit :: Literal -> Value e
fromLit (LNum n) = VNum n
fromLit (LBool b) = VBool b
fromLit (LText t) = VText t

data RuntimeError
    = DimensionMismatch 
    deriving (Show, Eq)

broadcast :: MonadError RuntimeError f => ([Value e] -> f (Value e)) -> [(Int, Value e)] -> f (Value e)
broadcast fn args
    | all ((0==) . fst) args = fn $ snd <$> args
    | otherwise = fmap VList $ traverse (broadcast fn) =<< unliftSplit args
  where
    unliftSplit :: MonadError RuntimeError f => [(Int, Value e)] -> f [[(Int, Value e)]]
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

    transposeVLists :: MonadError RuntimeError f => [(Int, Value e)] -> f [[(Int, Value e)]]
    transposeVLists = transpose' . fmap extractVList
      where
        extractVList (i, VList l) = (i-1,) <$> l
        extractVList _ = error "broadcast: bug in typechecker"

        transpose' [] = pure []
        transpose' ls =
            let lens = length <$> ls in
                if all (==head lens) lens
                then pure $ transpose ls
                else throwError DimensionMismatch

    replaceIn :: [Maybe a] -> [a] -> [a]
    replaceIn [] _ = []
    replaceIn (Nothing:is) (r:rs) = r : replaceIn is rs
    replaceIn (Nothing:_) _ = error "broadcast: bug in unlifter"
    replaceIn (Just i:is) rs = i : replaceIn is rs


evalExpr
    :: ( MonadEnv (f (Value e)) f
       , MonadScoped e f
       , Scoped e
       , MonadError RuntimeError f
       )
    => CoreExpr -> f (Value e)
evalExpr = para \case
    CLitF v -> pure $ fromLit v
    CVarF name -> join $ lookupName name
    CLetF v (_, vx) (_, x) -> scope $ do
        extend (v, vx)
        x
    CLamF args (x, _) -> do
        env <- getEnv
        pure $ VClosure env args x
    CRecF Nonrecursive xs -> VRecord . Map.fromList <$> sequenceA (liftTuple' <$> xs)
    CRecF Recursive xs -> scope $ do
        traverse_ extend $ second snd <$> xs
        xs' <- traverse (liftTuple . second snd) xs
        pure $ VRecord $ Map.fromList xs'
    CTabF (_, vs') ->
        vs' >>= \case
            VRecord vs -> pure $ VTable $ getList <$> vs
            _ -> error "evalExpr.CTabF: bug in typechecker"
    CAppF (_, fn) es -> do
        v <- fn
        broadcast (fromClosure v) =<< traverse liftTuple' es
    CNullF -> pure VNull
  where
    getList :: Value a -> [Value a]
    getList (VList vs) = vs
    getList _ = error "evalExpr.getList: bug in typechecker"

    liftTuple :: Functor f => (a, f b) -> f (a, b)
    liftTuple (a, b) = (a,) <$> b

    liftTuple' :: Functor f => (a, (x, f b)) -> f (a, b)
    liftTuple' (a, (_, b)) = (a,) <$> b

    fromClosure
        :: forall m e.
           ( MonadEnv (m (Value e)) m
           , MonadError RuntimeError m
           , MonadScoped e m
           , Scoped e)
        => Value e -> ([Value e] -> m (Value e))
    fromClosure (VClosure env' args x) vs = do
        env <- getEnv
        let innerEnv = env `addScope` env'
        withEnv innerEnv $ do
            for_ (zip args $ fmap (pure @m) vs) extend
            evalExpr x
    fromClosure (VPrimClosure (PrimClosure f)) vs = pure $ f vs
    fromClosure _ _ = error "fromClosure: bug in typechecker"
