{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Nexo.Env where

import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.State.Strict (StateT(..), MonadTrans(lift))

import Nexo.Core.Substitute

class Monad m => MonadScoped e m | m -> e where
    getEnv :: m e
    withEnv :: e -> m a -> m a

class Monad m => MonadEnv t m where
    lookupName :: String -> m t
    extend :: (String, t) -> m ()

class Monad m => MonadSubst m where
    applyToEnv :: Subst -> m ()

instance MonadScoped e m => MonadScoped e (ExceptT x m) where
    getEnv = lift getEnv
    withEnv e (ExceptT m) = ExceptT $ withEnv e m

-- instance MonadEnv t m => MonadEnv t (ExceptT x m) where
-- instance MonadSubst m => MonadSubst (ExceptT x m) where
-- can't make these instances since they conflict with cases where
-- names can be looked up /only/ in an ExceptT environment

scope :: MonadScoped e m => m a -> m a
scope m = do
    e <- getEnv
    withEnv e m

withName :: (MonadEnv t m, MonadScoped e m) => (String, t) -> m a -> m a
withName b m = do
    e <- getEnv
    withEnv e $ extend b >> m

instance MonadScoped e m => MonadScoped e (StateT s m) where
    getEnv = lift getEnv
    withEnv e (StateT m) = StateT $ withEnv e . m

instance MonadEnv t m => MonadEnv t (StateT s m) where
    lookupName = lift . lookupName
    extend = lift . extend

instance MonadSubst m => MonadSubst (StateT s m) where
    applyToEnv = lift . applyToEnv
