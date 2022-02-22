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

import Nexo.Core.Type
import Nexo.Core.Unit (simplify)
import Nexo.Expr.Type (Type(..))

type Subst = Map.Map String (Either CoreType Unit)

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 =
    let s2' = apply s1 <$> s2
    in Map.union s2' s1

class Substitutable a where
    apply :: Subst -> a -> a
    frees :: a -> Set.Set CoreVar

instance (Substitutable a, Substitutable b) => Substitutable (Either a b) where
    apply s (Left a) = Left $ apply s a
    apply s (Right b) = Right $ apply s b

    frees (Left a) = frees a
    frees (Right b) = frees b

instance Substitutable CoreType where
    apply s (CNum u) = CNum $ apply s u
    apply _ CBool = CBool
    apply _ CText = CText
    apply s tv@(CTVar (Undetermined v)) =
        case Map.lookup v s of
            Just (Left t) -> t
            Just (Right u) -> CUnit u
            Nothing -> tv
    apply _ tv@(CTVar (Rigid _)) = tv
    apply s (CFun ts r) = CFun (apply s <$> ts) (apply s r)
    apply s (CList t) = CList $ apply s t
    apply s (CRecord ts) = CRecord $ apply s <$> ts
    apply s (CTable ts) = CTable $ apply s <$> ts
    apply s (CUnit u) = CUnit $ apply s u

    frees (CNum u) = frees u
    frees CBool = Set.empty
    frees CText = Set.empty
    frees (CTVar v) = Set.singleton v
    frees (CFun ts r) = Set.unions $ frees r : (frees <$> ts)
    frees (CList t) = frees t
    frees (CRecord ts) = Set.unions $ frees <$> Map.elems ts
    frees (CTable ts) = Set.unions $ frees <$> Map.elems ts
    frees (CUnit u) = frees u

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
        getUndetermined :: Either String CoreVar -> Maybe String
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

occurs :: Substitutable a => CoreVar -> a -> Bool
occurs a t = a `Set.member` frees t

bind :: String -> Either CoreType Unit -> Maybe Subst
bind v (Left (CTVar v'))
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
    fresh :: m CoreVar

instance Monad m => MonadFresh (StateT Int m) where
    fresh = do
        s <- get
        put (s+1)
        pure $ Undetermined $ letters !! s
      where
        letters = [1..] >>= flip replicateM ['a'..'z']

instantiate :: MonadFresh m => PType -> m CoreType
instantiate (Forall vs t) = do
    vs' <- for vs $ \v -> (v,) <$> fresh
    pure $ derigidify vs' t
  where
    derigidify :: [(String, CoreVar)] -> CoreType -> CoreType
    derigidify vs' (CNum ty) = CNum $ derigidify vs' ty
    derigidify _   CBool = CBool
    derigidify _   CText = CText
    derigidify vs' (CTVar (Rigid v))
        | Just v' <- lookup v vs' = CTVar v'
    derigidify _   tv@(CTVar _) = tv
    derigidify vs' (CFun tys ty') = CFun (derigidify vs' <$> tys) (derigidify vs' ty')
    derigidify vs' (CList ty') = CList (derigidify vs' ty')
    derigidify vs' (CRecord r) = CRecord (derigidify vs' <$> r)
    derigidify vs' (CTable r) = CTable (derigidify vs' <$> r)
    derigidify vs' (CUnit u) = CUnit $ derigidifyU vs' u

    derigidifyU :: [(String, CoreVar)] -> Unit -> Unit
    derigidifyU vs' = second $ Map.mapKeys $ \case
        Right (Rigid v)
            | Just v' <- lookup v vs' -> Right v'
        x -> x

instantiateRigid :: Type -> Either TypeError CoreType
instantiateRigid (TNum u) = CNum . CUnit <$> simplify u
instantiateRigid TBool = Right CBool
instantiateRigid TText = Right CText
instantiateRigid (TVar s) = Right $ CTVar $ Rigid s
instantiateRigid (TFun as x) = CFun <$> traverse instantiateRigid as <*> instantiateRigid x
instantiateRigid (TList t) = CList <$> instantiateRigid t
instantiateRigid (TRecord m) = CRecord <$> traverse instantiateRigid m
instantiateRigid (TTable m) = CTable <$> traverse instantiateRigid m

generalise :: CoreType -> PType
generalise t =
    Forall (getName <$> Set.toList (frees t)) $ rigidify t
  where
    getName (Rigid v) = v
    getName (Undetermined v) = v

    rigidify (CNum ty) = CNum $ rigidify ty
    rigidify CBool = CBool
    rigidify CText = CText
    rigidify (CTVar (Undetermined v)) = CTVar $ Rigid v
    rigidify tv@(CTVar (Rigid _)) = tv
    rigidify (CFun tys ty') = CFun (rigidify <$> tys) (rigidify ty')
    rigidify (CList ty') = CList (rigidify ty')
    rigidify (CRecord r) = CRecord (rigidify <$> r)
    rigidify (CTable r) = CTable (rigidify <$> r)
    rigidify (CUnit u) = CUnit (rigidifyU u)

    rigidifyU = second $ Map.mapKeys $ \case
        Right (Undetermined v) -> Right (Rigid v)
        x -> x
