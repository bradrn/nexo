{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Nexo.Core.Substitute where

import Control.Monad (replicateM)
import Control.Monad.State.Strict (StateT, MonadState (get, put))
import Data.Traversable (for)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Nexo.Expr.Type

type Subst = Map.Map String (Either Type UnitDef)

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Maybe Subst
compose s1 s2 = do
    s2' <- traverse (apply s1) s2
    pure $ Map.union s2' s1

class Substitutable a where
    apply :: Subst -> a -> Maybe a
    frees :: a -> (Set.Set TVar, Set.Set TVar)

instance (Substitutable a, Substitutable b) => Substitutable (Either a b) where
    apply s (Left a) = Left <$> apply s a
    apply s (Right b) = Right <$> apply s b

    frees (Left a) = frees a
    frees (Right b) = frees b

instance Substitutable Type where
    apply s (TNum u) = TNum <$> apply s u
    apply _ TBool = Just TBool
    apply _ TText = Just TText
    apply s tv@(TVar (Undetermined v)) =
        case Map.lookup v s of
            Just (Left t) -> Just t
            Just (Right _) -> Nothing
            Nothing -> Just tv
    apply _ tv@(TVar (Rigid _)) = Just tv
    apply s (TFun ts r) = TFun <$> traverse (apply s) ts <*> apply s r
    apply s (TList t) = TList <$> apply s t
    apply s (TRecord ts) = TRecord <$> traverse (apply s) ts
    apply s (TTable ts) = TTable <$> traverse (apply s) ts

    frees (TNum u) = frees u
    frees TBool = (Set.empty, Set.empty)
    frees TText = (Set.empty, Set.empty)
    frees (TVar v) = (Set.singleton v, Set.empty)
    frees (TFun ts r) =
        case unzip $ frees r : (frees <$> ts) of
            (vs, us) -> (Set.unions vs, Set.unions us)
    frees (TList t) = frees t
    frees (TRecord ts) =
        case unzip $ frees <$> Map.elems ts of
            (vs, us) -> (Set.unions vs, Set.unions us)
    frees (TTable ts) =
        case unzip $ frees <$> Map.elems ts of
            (vs, us) -> (Set.unions vs, Set.unions us)

instance Substitutable UnitDef where
    apply _ n@(ULeaf _)   = Just n
    apply _ f@(UFactor _) = Just f
    apply s (UMul u v) = UMul <$> apply s u <*> apply s v
    apply s (UDiv u v) = UDiv <$> apply s u <*> apply s v
    apply s (UExp u x) = (`UExp` x) <$> apply s u
    apply s tv@(UVar (Undetermined v)) =
        case Map.lookup v s of
            Just (Left _) -> Nothing
            Just (Right t) -> Just t
            Nothing -> Just tv
    apply _ tv@(UVar (Rigid _)) = Just tv

    frees (ULeaf _) = (Set.empty, Set.empty)
    frees (UFactor _) = (Set.empty, Set.empty)
    frees (UMul u v) = (Set.empty, snd (frees u) `Set.union` snd (frees v))
    frees (UDiv u v) = (Set.empty, snd (frees u) `Set.union` snd (frees v))
    frees (UExp u _) = frees u
    frees (UVar v) = (Set.empty, Set.singleton v)

instance Substitutable PType where
    apply s (Forall as us t) = Forall as us <$> apply (foldr Map.delete s (as++us)) t

    frees (Forall as us t) =
        let (vs, xs) = frees t
        in ( vs `Set.difference` Set.fromList (Rigid <$> as)
           , xs `Set.difference` Set.fromList (Rigid <$> us)
           )

occurs :: Substitutable a => TVar -> a -> Bool
occurs a t =
    let (vs, us) = frees t
    in a `Set.member` Set.union vs us

bind :: String -> Either Type UnitDef -> Maybe Subst
bind v (Left (TVar v'))
    | Undetermined v == v' = pure nullSubst
    | occurs (Undetermined v) (TVar v') = Nothing
bind v (Right (UVar v'))
    | Undetermined v == v' = pure nullSubst
    | occurs (Undetermined v) (UVar v') = Nothing
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
instantiate (Forall as us t) = do
    as' <- for as $ \a -> (a,) <$> fresh
    us' <- for us $ \u -> (u,) <$> fresh
    pure $ derigidify as' us' t
  where
    derigidify _   us' (TNum u) = TNum $ derigidifyU us' u
    derigidify _   _   TBool = TBool
    derigidify _   _   TText = TText
    derigidify as' _   (TVar (Rigid v))
        | Just v' <- lookup v as' = TVar v'
    derigidify _   _   tv@(TVar _) = tv
    derigidify as' us' (TFun tys ty') = TFun (derigidify as' us' <$> tys) (derigidify as' us' ty')
    derigidify as' us' (TList ty') = TList (derigidify as' us' ty')
    derigidify as' us' (TRecord r) = TRecord (derigidify as' us' <$> r)
    derigidify as' us' (TTable r) = TTable (derigidify as' us' <$> r)

    derigidifyU _   u@(ULeaf _) = u
    derigidifyU _   u@(UFactor _) = u
    derigidifyU us' (UMul u1 u2) = UMul (derigidifyU us' u1) (derigidifyU us' u2)
    derigidifyU us' (UDiv u1 u2) = UDiv (derigidifyU us' u1) (derigidifyU us' u2)
    derigidifyU us' (UExp u n) = UExp (derigidifyU us' u) n
    derigidifyU us' (UVar (Rigid v))
        | Just v' <- lookup v us' = UVar v'
    derigidifyU _   uv@(UVar _) = uv

-- Assumed input is well-formed i.e. all quantified type variables are 'Rigid'
instantiateRigid :: PType -> Type
instantiateRigid (Forall _ _ t) = t

generalise :: Type -> PType
generalise t =
    let (vs, us) = frees t
    in Forall (getName <$> Set.toList vs) (getName <$> Set.toList us) $ rigidify t
  where
    getName (Rigid v) = v
    getName (Undetermined v) = v

    rigidify (TNum u) = TNum $ rigidifyU u
    rigidify TBool = TBool
    rigidify TText = TText
    rigidify (TVar (Undetermined v)) = TVarR v
    rigidify tv@(TVar (Rigid _)) = tv
    rigidify (TFun tys ty') = TFun (rigidify <$> tys) (rigidify ty')
    rigidify (TList ty') = TList (rigidify ty')
    rigidify (TRecord r) = TRecord (rigidify <$> r)
    rigidify (TTable r) = TTable (rigidify <$> r)

    rigidifyU u@(ULeaf _) = u
    rigidifyU u@(UFactor _) = u
    rigidifyU (UMul u1 u2) = UMul (rigidifyU u1) (rigidifyU u2)
    rigidifyU (UDiv u1 u2) = UDiv (rigidifyU u1) (rigidifyU u2)
    rigidifyU (UExp u n) = UExp (rigidifyU u) n
    rigidifyU (UVar (Undetermined v)) = UVarR v
    rigidifyU uv@(UVar (Rigid _)) = uv
