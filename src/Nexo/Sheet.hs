{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}

module Nexo.Sheet
       ( Sheet(..)
       , Cell(..)
       , ValueState(..)
       , ValueEnv
       , Eval
       , Value'
       , ValueState'
       , display
       , evalSheet
       , insert
       ) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.State.Strict
    ( execState, gets, modify', put, state, runState, State, StateT (..), MonadState )
import Control.Monad.Trans (lift)
#if MIN_VERSION_recursion_schemes(5,2,0)
import Data.Fix (Fix(..))
#else
import Data.Functor.Foldable (Fix(..))
#endif
import qualified Data.Map.Strict as Map

import Nexo.Core.Typecheck
import Nexo.Expr.Type
import Nexo.Interpret
import Nexo.Env
import Nexo.Env.Std

data ValueState e
    = ValuePresent PType (Value e)
    | ValueError String
    | Invalidated
    deriving (Show)

display :: ValueState e -> String
display (ValuePresent _ v) = render v
display (ValueError e) = '#' : e
display Invalidated = "#INVALIDATED"

fromEither :: PType -> Either String (Value e) -> ValueState e
fromEither t (Right val) = ValuePresent t val
fromEither _ (Left err) = ValueError err

toEither :: ValueState e -> Either String (PType, Value e)
toEither (ValuePresent t val) = Right (t, val)
toEither (ValueError err) = Left err
toEither Invalidated = Left "#INVALIDATED"
    
newtype ValueEnv m = ValueEnv (SheetEnv (Value (ValueEnv m)) m)
    deriving (Show, Scoped)
newtype InValueEnvT m a = InValueEnvT { unwrapValueEnv :: InSheetEnvT (Value (ValueEnv m)) m a }
    deriving (Functor, Applicative, Monad, MonadFail)
instance (Monad m, MonadFail m) => MonadEnv (Value (ValueEnv m)) (ValueEnv m) (InValueEnvT m) where
    lookupName = InValueEnvT . lookupName
    extend = InValueEnvT . extend
    getEnv = InValueEnvT $ ValueEnv <$> getEnv
    withEnv (ValueEnv e) (InValueEnvT m) = InValueEnvT $ withEnv e m

    
type ValueState' = ValueState (ValueEnv Eval)
type Value' = Value (ValueEnv Eval)

data Cell = Cell
    { cellName :: String
    , cellType :: Maybe PType
    , cellExpr :: Expr
    , cellValue :: ValueState (ValueEnv Eval)
    } deriving (Show)

newtype Sheet = Sheet { getSheet :: Map.Map Int Cell }
    deriving (Show)

insert :: Int -> Cell -> Sheet -> Sheet
insert k v = Sheet . Map.insert k v . getSheet

newtype Eval a = Eval { runEval :: StateT (Map.Map Int Cell) (Either String) a }
    deriving (Functor, Applicative, Monad, MonadState (Map.Map Int Cell))
instance MonadFail Eval where
    fail = Eval . lift . Left

raise :: State (Map.Map Int Cell) (Either String a) -> Eval a
raise = Eval . StateT . (raiseEither .) . runState
  where
    raiseEither (a, s) = (,s) <$> a

lower :: Eval a -> State (Map.Map Int Cell) (Either String a)
lower = state . (\f s -> lowerEither (f s) s) . runStateT . runEval
  where
    lowerEither (Left e) s = (Left e, s)
    lowerEither (Right (a,s')) _ = (Right a, s')

evalSheet :: Sheet -> Sheet
evalSheet (Sheet s) =
    Sheet $ flip execState Map.empty $ go $ invalidate s
  where
    invalidate :: Map.Map Int Cell -> Map.Map Int Cell
    invalidate = Map.map $ \c -> c { cellValue = Invalidated }

    go :: Map.Map Int Cell -> State (Map.Map Int Cell) ()
    go sheet = put sheet >> mapM_ cacheExpr (Map.keys sheet)

    cacheExpr :: Int -> State (Map.Map Int Cell) (ValueState (ValueEnv Eval))
    cacheExpr ident = gets (Map.lookup ident) >>= \case
        -- Error if ident is unassigned
        Nothing -> pure $ ValueError "#IREF"
        -- Typecheck, evaluate, cache and return new value if invalidated
        Just c@Cell{cellType = type_, cellExpr = expr, cellValue = Invalidated} -> do
            let expr' = maybe expr (Fix . XTApp expr) type_
                tenv = SheetEnv { lookupGlobal = fmap fst . cacheByName, locals = stdFnTs }
            r <- lower $ runInSheetEnvT (typecheck expr') tenv
            (v, t) <- case r of
                Left e -> pure (ValueError e, Nothing)
                Right ((coreExpr, resultType), _) -> do
                    let venv :: SheetEnv (Value (ValueEnv Eval)) Eval
                        venv = SheetEnv { lookupGlobal = fmap snd . cacheByName, locals = stdFnVals }
                    result <- lower $ fst <$> runInSheetEnvT (unwrapValueEnv $ evalExpr coreExpr) venv
                    pure $ (,Just resultType) $ fromEither resultType result
            modify' $ Map.insert ident (c { cellType = t, cellValue = v })
            pure v
        -- Else return cached value
        Just Cell{cellValue = v} -> pure v

    cacheByName :: String -> Eval (PType, Value (ValueEnv Eval))
    cacheByName name = do
        ident <- gets $
            flip Map.foldrWithKey Nothing $ \k v -> \case
                Nothing -> if name == cellName v then Just k else Nothing
                found   -> found
        case ident of
            Just i -> raise $ toEither <$> cacheExpr i
            Nothing -> fail "#REF"
