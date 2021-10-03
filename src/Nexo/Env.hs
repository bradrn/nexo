{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Nexo.Env where

import Control.Monad.State.Strict (StateT(..), MonadState(..), MonadTrans(lift), modify)

class Monad m => MonadEnv t e m | m -> t e where
    lookupName :: String -> m t
    extend :: (String, t) -> m ()
    getEnv :: m e
    withEnv :: e -> m a -> m a

class Scoped e where
    nullEnv :: e
    addScope :: e -> e -> e

withName :: MonadEnv t e m => (String, t) -> m a -> m a
withName b m = do
    e <- getEnv
    withEnv e $ extend b >> m

instance MonadEnv t e m => MonadEnv t e (StateT s m) where
    lookupName = lift . lookupName
    extend = lift . extend
    getEnv = lift getEnv
    withEnv e (StateT m) = StateT $ withEnv e . m

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
