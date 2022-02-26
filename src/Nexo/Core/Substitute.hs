{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

module Nexo.Core.Substitute where

import Control.Monad (replicateM)
import Control.Monad.State.Strict (StateT, MonadState (get, put))
import Data.Bifunctor (second)
import Data.List (intersect, foldl')
import Data.Maybe (mapMaybe)
import Data.Traversable (for)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Set as Set

import Nexo.Core.Type  -- most frequent, so don't qualify

type Subst = Map.Map String (Either Type Unit)

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 =
    let s2' = apply s1 <$> s2
    in Map.union s2' s1

class Substitutable a where
    apply :: Subst -> a -> a
    frees :: a -> Set.Set TVar

instance (Substitutable a, Substitutable b) => Substitutable (Either a b) where
    apply s (Left a) = Left $ apply s a
    apply s (Right b) = Right $ apply s b

    frees (Left a) = frees a
    frees (Right b) = frees b

instance Substitutable Type where
    apply s (TNum u) = TNum $ apply s u
    apply _ TBool = TBool
    apply _ TText = TText
    apply s tv@(TVar (Undetermined v)) =
        case Map.lookup v s of
            Just (Left t) -> t
            Just (Right u) -> TUnit u
            Nothing -> tv
    apply _ tv@(TVar (Rigid _)) = tv
    apply s (TFun ts r) = TFun (apply s <$> ts) (apply s r)
    apply s (TList t) = TList $ apply s t
    apply s (TRecord ts) = TRecord $ apply s <$> ts
    apply s (TTable ts) = TTable $ apply s <$> ts
    apply s (TUnit u) = TUnit $ apply s u

    frees (TNum u) = frees u
    frees TBool = Set.empty
    frees TText = Set.empty
    frees (TVar v) = Set.singleton v
    frees (TFun ts r) = Set.unions $ frees r : (frees <$> ts)
    frees (TList t) = frees t
    frees (TRecord ts) = Set.unions $ frees <$> Map.elems ts
    frees (TTable ts) = Set.unions $ frees <$> Map.elems ts
    frees (TUnit u) = frees u

instance Substitutable Unit where
    apply s (f, m) =
        let tvs = Map.keys s `intersect` mapMaybe getUndetermined (Map.keys m)
            -- remove tvars from map
            m' = m `Map.difference` Map.fromList ((,()) . Right . Undetermined <$> tvs)
            -- get unit for each tvar
            -- assume we don't try to substitute types into units
            substs = mapMaybe (either (const Nothing) Just) $ (s Map.!) <$> tvs
        in foldl' multiply (f, m') substs
      where
        getUndetermined :: Either String TVar -> Maybe String
        getUndetermined (Right (Undetermined v)) = Just v
        getUndetermined _ = Nothing

        multiply :: Unit -> Unit -> Unit
        multiply (f_, u) (g, v) =
            ( f_*g
            , Map.merge
                Map.preserveMissing
                Map.preserveMissing
                (Map.zipWithMatched $ const (*))
                u v
            )

    frees = Set.fromList . mapMaybe (either (const Nothing) Just) . Map.keys . snd

instance Substitutable PType where
    apply s (Forall vs t) = Forall vs $ apply (foldr Map.delete s vs) t

    frees (Forall vs t) = frees t `Set.difference` Set.fromList (Rigid <$> vs)

occurs :: Substitutable a => TVar -> a -> Bool
occurs a t = a `Set.member` frees t

bind :: String -> Either Type Unit -> Maybe Subst
bind v (Left (TVar v'))
    | Undetermined v == v' = pure nullSubst
bind v (Left t)
    | occurs (Undetermined v) t = Nothing
bind v (Right u@(_, m))
    | Map.size m == 1
    , Just 1 <- Map.lookup (Right $ Undetermined v) m
        = pure nullSubst
    | occurs (Undetermined v) u = Nothing
bind v t = pure $ Map.singleton v t

class Monad m => MonadFresh m where
    fresh :: m TVar

instance Monad m => MonadFresh (StateT Int m) where
    fresh = do
        s <- get
        put (s+1)
        pure $ Undetermined $ letters !! s
      where
        letters = [1..] >>= flip replicateM ['a'..'z']

instantiate :: MonadFresh m => PType -> m Type
instantiate (Forall vs t) = do
    vs' <- for vs $ \v -> (v,) <$> fresh
    pure $ derigidify vs' t
  where
    derigidify :: [(String, TVar)] -> Type -> Type
    derigidify vs' (TNum ty) = TNum $ derigidify vs' ty
    derigidify _   TBool = TBool
    derigidify _   TText = TText
    derigidify vs' (TVar (Rigid v))
        | Just v' <- lookup v vs' = TVar v'
    derigidify _   tv@(TVar _) = tv
    derigidify vs' (TFun tys ty') = TFun (derigidify vs' <$> tys) (derigidify vs' ty')
    derigidify vs' (TList ty') = TList (derigidify vs' ty')
    derigidify vs' (TRecord r) = TRecord (derigidify vs' <$> r)
    derigidify vs' (TTable r) = TTable (derigidify vs' <$> r)
    derigidify vs' (TUnit u) = TUnit $ derigidifyU vs' u

    derigidifyU :: [(String, TVar)] -> Unit -> Unit
    derigidifyU vs' = second $ Map.mapKeys $ \case
        Right (Rigid v)
            | Just v' <- lookup v vs' -> Right v'
        x -> x

generalise :: Type -> PType
generalise t =
    Forall (getName <$> Set.toList (frees t)) $ rigidify t
  where
    getName (Rigid v) = v
    getName (Undetermined v) = v

    rigidify (TNum ty) = TNum $ rigidify ty
    rigidify TBool = TBool
    rigidify TText = TText
    rigidify (TVar (Undetermined v)) = TVar $ Rigid v
    rigidify tv@(TVar (Rigid _)) = tv
    rigidify (TFun tys ty') = TFun (rigidify <$> tys) (rigidify ty')
    rigidify (TList ty') = TList (rigidify ty')
    rigidify (TRecord r) = TRecord (rigidify <$> r)
    rigidify (TTable r) = TTable (rigidify <$> r)
    rigidify (TUnit u) = TUnit (rigidifyU u)

    rigidifyU = second $ Map.mapKeys $ \case
        Right (Undetermined v) -> Right (Rigid v)
        x -> x
