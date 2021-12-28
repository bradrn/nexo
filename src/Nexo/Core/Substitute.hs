{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Nexo.Core.Substitute where

import Control.Monad (replicateM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State.Strict (StateT, MonadState (get, put))
import Data.Traversable (for)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Nexo.Expr.Type
import Nexo.Expr.Unit

type Subst = Map.Map TVar (Either Type UnitDef)

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
    apply s (TVar v) =
        case Map.lookup v s of
            Just (Left t) -> Just t
            Just (Right _) -> Nothing
            Nothing -> Just $ TVar v
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
    apply _ n@(UName _)   = Just n
    apply _ p@(UPrefix _) = Just p
    apply _ f@(UFactor _) = Just f
    apply s (UMul u v) = UMul <$> apply s u <*> apply s v
    apply s (UDiv u v) = UDiv <$> apply s u <*> apply s v
    apply s (UExp u x) = (`UExp` x) <$> apply s u
    apply s (UVar v) =
        case Map.lookup v s of
            Just (Left _) -> Nothing
            Just (Right t) -> Just t
            Nothing -> Just $ UVar v

    frees (UName _) = (Set.empty, Set.empty)
    frees (UPrefix _) = (Set.empty, Set.empty)
    frees (UFactor _) = (Set.empty, Set.empty)
    frees (UMul u v) = (Set.empty, snd (frees u) `Set.union` snd (frees v))
    frees (UDiv u v) = (Set.empty, snd (frees u) `Set.union` snd (frees v))
    frees (UExp u _) = frees u
    frees (UVar v) = (Set.empty, Set.singleton v)

instance Substitutable PType where
    apply s (Forall as us t) = Forall as us <$> apply (foldr Map.delete s (as++us)) t

    frees (Forall as us t) =
        let (vs, xs) = frees t
        in (vs `Set.difference` Set.fromList as, xs `Set.difference` Set.fromList us)

occurs :: Substitutable a => TVar -> a -> Bool
occurs a t =
    let (vs, us) = frees t
    in a `Set.member` Set.union vs us

bind :: MonadError String m => TVar -> Either Type UnitDef -> m Subst
bind v (Left (TVar v'))  | v == v' = pure nullSubst
                         | occurs v (TVar v') = throwError "#INFT"
bind v (Right (UVar v')) | v == v' = pure nullSubst
                         | occurs v (UVar v') = throwError "#INFT"
bind v t = pure $ Map.singleton v t

class Monad m => MonadFresh m where
    fresh :: m TVar

instance Monad m => MonadFresh (StateT Int m) where
    fresh = do
        s <- get
        put (s+1)
        pure $ letters !! s
      where
        letters = [1..] >>= flip replicateM ['a'..'z']

instantiate :: (MonadError String m, MonadFresh m) => PType -> m Type
instantiate (Forall as us t) = do
    as' <- for as $ const fresh
    us' <- for us $ const fresh
    let s = Map.fromList $
                zip as (Left  . TVar <$> as') ++
                zip us (Right . UVar <$> us')
    maybe (throwError "#TYPE") pure $ apply s t

generalise :: Type -> PType
generalise t =
    let (vs, us) = frees t
    in Forall (Set.toList vs) (Set.toList us) t
