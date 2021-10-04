{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Nexo.Env where

import Control.Monad.State.Strict (StateT(..), MonadState(..), MonadTrans(lift), modify)
import Data.Set (unions)

import Nexo.Core.Substitute

class Monad m => MonadEnv t e m | m -> t e where
    lookupName :: String -> m t
    extend :: (String, t) -> m ()
    getEnv :: m e
    withEnv :: e -> m a -> m a

class Monad m => MonadSubst m where
    applyToEnv :: Subst -> m ()

class Scoped e where
    nullEnv :: e
    addScope :: e -> e -> e

scope :: MonadEnv t e m => m a -> m a
scope m = do
    e <- getEnv
    withEnv e m

withName :: MonadEnv t e m => (String, t) -> m a -> m a
withName b m = do
    e <- getEnv
    withEnv e $ extend b >> m

instance MonadEnv t e m => MonadEnv t e (StateT s m) where
    lookupName = lift . lookupName
    extend = lift . extend
    getEnv = lift getEnv
    withEnv e (StateT m) = StateT $ withEnv e . m

instance MonadSubst m => MonadSubst (StateT s m) where
    applyToEnv = lift . applyToEnv

data SheetEnv t m = SheetEnv
    { lookupGlobal :: String -> m t
    , locals :: [(String, t)]
    }
instance Show t => Show (SheetEnv t m) where
    show e = "\\SE[" ++ show (locals e) ++ "]"

instance MonadFail m => Scoped (SheetEnv t m) where
    nullEnv = SheetEnv (const $ fail "#NAME") []
    SheetEnv{lookupGlobal=g1, locals=l1} `addScope` SheetEnv{lookupGlobal=_g2, locals=l2}
        = SheetEnv{lookupGlobal=g1, locals=l2++l1}

instance Substitutable t => Substitutable (SheetEnv t m) where
    apply s SheetEnv{..} =
        SheetEnv lookupGlobal <$>   -- assume other cells have no frees
            traverse (\(n,t) -> (n,) <$> apply s t) locals
    frees SheetEnv{locals} =
        let (tfs, ufs) = unzip $ frees . snd <$> locals
        in (unions tfs, unions ufs)

newtype InSheetEnvT t m a = InSheetEnvT { runInSheetEnvT :: SheetEnv t m -> m (a, SheetEnv t m) }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState (SheetEnv t m)
        , MonadFail
        ) via StateT (SheetEnv t m) m
instance MonadTrans (InSheetEnvT t) where
    lift m = InSheetEnvT $ \e -> (,e) <$> m

instance MonadFail m => MonadEnv t (SheetEnv t m) (InSheetEnvT t m) where
    lookupName n = do
        env <- get
        case lookup n (locals env) of
            Nothing -> lift $ lookupGlobal env n
            Just t -> pure t
    extend b = modify $ \e@SheetEnv{locals} -> e{locals=b:locals}
    getEnv = get
    withEnv e' (InSheetEnvT m) = InSheetEnvT $ \e ->
        m e' >>= \(a, _) -> pure (a, e)

instance (MonadFail m, Substitutable t) => MonadSubst (InSheetEnvT t m) where
    applyToEnv s = do
        env <- get
        case apply s env of
            Just env' -> put env'
            Nothing -> fail "#TYPE"
