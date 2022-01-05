{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Nexo.Sheet
       ( Sheet(..)
       , Widget(..)
       , Cell(..)
       , ValueState(..)
       , GlobalEnv
       , Eval(..)
       , Value'
       , ValueState'
       , display
       , evalSheet
       , insert
       ) where

import Control.Monad.Except (runExceptT, MonadError(..), ExceptT(..))
import Control.Monad.Free (Free)
import Control.Monad.State.Strict
    ( gets, modify', State, StateT (..), MonadState(..) )
import Control.Monad.Trans (lift)
import Data.Bifunctor (second)
import Data.Fix (Fix(Fix))
import Data.Functor.Identity (Identity(Identity))
import Data.Set (unions)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Reorder
import Nexo.Core.Substitute
import Nexo.Core.Typecheck
import Nexo.Expr.Type
import Nexo.Interpret
import Nexo.Env (MonadScoped(..), MonadEnv(..), MonadSubst(..), Scoped(..))
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
    
type ValueState' = ValueState GlobalEnv
type Value' = Value GlobalEnv

data Widget
    = ValueCell String
    | InputList [String]
    | Table [(String, [String])]
    deriving (Show)

data Cell = Cell
    { cellName :: String
    , cellType :: Maybe PType
    , cellWidget :: Widget
    , cellExpr :: Expr
    , cellValue :: ValueState GlobalEnv
    } deriving (Show)

newtype Sheet = Sheet { getSheet :: Map.Map Int Cell }
    deriving (Show)

insert :: Int -> Cell -> Sheet -> Sheet
insert k v = Sheet . Map.insert k v . getSheet

data GlobalEnv = GlobalEnv
    { lookupGlobal :: String -> ExceptT String Eval (PType, Value')
    -- , sheet :: Sheet
    , localTypes :: [(String, PType)]
    , localValues :: [(String, ExceptT String Eval Value')]}
instance Show GlobalEnv where
    show _ = "\\VSE[]"

instance Scoped GlobalEnv where
    nullEnv = GlobalEnv
        { lookupGlobal = const $ throwError "#NAME"
        -- , sheet = Sheet Map.empty
        , localTypes = []
        , localValues = []
        }
    e1 `addScope` e2 = GlobalEnv
        { lookupGlobal = lookupGlobal e1
        -- , sheet = sheet e1
        , localTypes = localTypes e2 ++ localTypes e1
        , localValues = localValues e2 ++ localValues e1
        }

instance Substitutable GlobalEnv where
    apply s GlobalEnv{..} =
        flip (GlobalEnv lookupGlobal) localValues <$>  -- assume other cells have no frees
            traverse (\(n,t) -> (n,) <$> apply s t) localTypes
    frees GlobalEnv{localTypes=locals} =
        let (tfs, ufs) = unzip $ frees . snd <$> locals
        in (unions tfs, unions ufs)

newtype Eval a = Eval
    { runEval :: Map.Map Int Cell -> GlobalEnv -> ((a, Map.Map Int Cell), GlobalEnv)
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState (Map.Map Int Cell)
        ) via StateT (Map.Map Int Cell) (State GlobalEnv)

instance MonadScoped GlobalEnv Eval where
    getEnv = Eval $ \s e -> ((e, s), e)
    withEnv e' (Eval m) = Eval $ \s e ->
        let ((a, s'), _) = m s e'
        in ((a, s'), e)

instance MonadEnv PType (ExceptT String Eval) where
    lookupName n = do
        env <- getEnv
        case lookup n (localTypes env) of
            Nothing -> fst <$> lookupGlobal env n
            Just t -> pure t
    extend b = lift $ Eval $ \s e@GlobalEnv{localTypes}
        -> (((), s), e{localTypes=b:localTypes})

instance MonadEnv (ExceptT String Eval Value') (ExceptT String Eval) where
    lookupName n = do
        env <- getEnv
        case lookup n (localValues env) of
            Nothing -> fmap pure $ snd <$> lookupGlobal env n
            Just v -> pure v
    extend b = lift $ Eval $ \s e@GlobalEnv{localValues}
        -> (((), s), e{localValues=b:localValues})

instance MonadSubst (ExceptT String Eval) where
  applyToEnv subst = ExceptT $ Eval $ \s e ->
      case apply subst e of
          Just e' -> ((Right (), s), e')
          Nothing -> ((Left "#TYPE", s), e)

evalSheet :: Sheet -> Sheet
evalSheet (Sheet s) = Sheet $
    snd $ fst $ runEval (go $ invalidate s) Map.empty initEnv
  where
    invalidate :: Map.Map Int Cell -> Map.Map Int Cell
    invalidate = Map.map $ \c -> c { cellValue = Invalidated }

    initEnv = GlobalEnv
        { lookupGlobal = cacheByName
        , localTypes = stdFnTs
        , localValues = second pure <$> stdFnVals
        }

    go :: Map.Map Int Cell -> Eval ()
    go sheet = put sheet >> mapM_ cacheExpr (Map.keys sheet)

    cacheExpr :: Int -> Eval ValueState'
    cacheExpr ident = gets (Map.lookup ident) >>= \case
        -- Error if ident is unassigned
        Nothing -> pure $ ValueError "#IREF"
        -- Typecheck, evaluate, cache and return new value if invalidated
        Just c@Cell{cellType = type_, cellExpr = expr, cellValue = Invalidated} -> do
            let expr' = reorder $ maybe expr (Fix . XTApp expr) type_
            r <- runExceptT $ typecheck expr'
            (v, t) <- case r of
                Left e -> pure (ValueError e, Nothing)
                Right (coreExpr, resultType) -> do
                    result <- runExceptT $ evalExpr coreExpr
                    pure $ (,Just resultType) $ fromEither resultType result
            modify' $ Map.insert ident (c { cellType = t, cellValue = v })
            pure v
        -- Else return cached value
        Just Cell{cellValue = v} -> pure v

    cacheByName 
        :: String -> ExceptT String Eval (PType, Value')
    cacheByName name = do
        ident <- gets $
            flip Map.foldrWithKey Nothing $ \k v -> \case
                Nothing -> if name == cellName v then Just k else Nothing
                found   -> found
        case ident of
            Just i -> failOnLeft =<< toEither <$> lift (cacheExpr i)
            Nothing -> throwError "#REF"
      where
        failOnLeft = either throwError pure 
