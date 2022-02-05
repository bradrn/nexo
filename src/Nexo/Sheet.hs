{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

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

import Control.Category ((>>>))
import Control.Monad.Except (runExceptT, MonadError(..), ExceptT(..), withExceptT)
import Control.Monad.State.Strict
    ( gets, modify', State, StateT (..), MonadState(..) )
import Control.Monad.Trans (lift)
import Data.Bifunctor (second)
import Data.Fix (Fix(Fix))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(Identity))
import Data.Set (unions)

import qualified Data.Map.Strict as Map

import Nexo.Core.Substitute
import Nexo.Core.Typecheck
import Nexo.Expr.Type
import Nexo.Interpret
import Nexo.Env (MonadScoped(..), MonadEnv(..), MonadSubst(..), Scoped(..))
import Nexo.Expr.Desugar
import Nexo.Env.Std
import Nexo.Error
import Nexo.Core.Type (TypeError(KindMismatch, UnknownName))

data ValueState e
    = ValuePresent PType (Value e)
    | ValueError (Maybe PType) Error
    | Invalidated
    deriving (Show, Eq)

display :: ValueState e -> String
display (ValuePresent _ v) = render v
display (ValueError _ e) = renderError e
display Invalidated = "#INVALIDATED"

fromEither :: PType -> Either Error (Value e) -> ValueState e
fromEither t (Right val) = ValuePresent t val
fromEither t (Left err) = ValueError (Just t) err

type ValueState' = ValueState GlobalEnv
type Value' = Value GlobalEnv

data Widget
    = ValueCell String
    | InputList [String]
    | Table [(String, Either String [String])]
    deriving (Show, Eq)

data Cell = Cell
    { cellName :: String
    , cellType :: Maybe PType
    , cellWidget :: Widget
    , cellExpr :: AST
    , cellValue :: ValueState GlobalEnv
    } deriving (Show, Eq)

newtype Sheet = Sheet { getSheet :: Map.Map Int Cell }
    deriving (Show, Eq)

insert :: Int -> Cell -> Sheet -> Sheet
insert k v = Sheet . Map.insert k v . getSheet

data GlobalEnv = GlobalEnv
    { globalCells :: Map.Map Int Cell
    , localTypes :: [(String, PType)]
    , localValues :: [(String, ExceptT RuntimeError Eval Value')]
    }
instance Show GlobalEnv where
    show _ = "\\VSE[]"

instance Scoped GlobalEnv where
    nullEnv = GlobalEnv
        { globalCells = Map.empty
        , localTypes = []
        , localValues = []
        }
    e1 `addScope` e2 = GlobalEnv
        { globalCells = globalCells e1
        , localTypes = localTypes e2 ++ localTypes e1
        , localValues = localValues e2 ++ localValues e1
        }

instance Substitutable GlobalEnv where
    apply s GlobalEnv{..} =
        flip (GlobalEnv globalCells) localValues <$>  -- assume other cells have no frees
            traverse (\(n,t) -> (n,) <$> apply s t) localTypes
    frees GlobalEnv{localTypes=locals} =
        let (tfs, ufs) = unzip $ frees . snd <$> locals
        in (unions tfs, unions ufs)

newtype Eval a = Eval
    { runEval :: GlobalEnv -> (a, GlobalEnv)
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState GlobalEnv
        ) via State GlobalEnv

modifySheet :: (Map.Map Int Cell -> Map.Map Int Cell) -> Eval ()
modifySheet f = modify' $ \e@GlobalEnv{globalCells} -> e { globalCells = f globalCells }

instance MonadScoped GlobalEnv Eval where
    getEnv = get
    withEnv e' (Eval m) = Eval $ \e ->
        let (a, _) = m e'
        in (a, e)

instance MonadEnv PType (ExceptT TypeError Eval) where
    lookupName n = do
        env <- getEnv
        case lookup n (localTypes env) of
            Nothing -> lift (cacheByName n) >>= \case
                Nothing -> throwError $ UnknownName n
                Just (t, _) -> pure t
            Just t -> pure t
    extend b = lift $ modify' $ \e@GlobalEnv{localTypes} ->
        e{localTypes=b:localTypes}

instance MonadEnv (ExceptT RuntimeError Eval Value') (ExceptT RuntimeError Eval) where
    lookupName n = do
        env <- getEnv
        case lookup n (localValues env) of
            Nothing -> lift (cacheByName n) >>= \case
                Nothing -> error "lookupName: bug in typechecker"
                Just (_, Left e) -> throwError e
                Just (_, Right v) -> pure $ pure v
            Just v -> pure v
    extend b = lift $ modify' $ \e@GlobalEnv{localValues} ->
        e{localValues=b:localValues}

instance MonadSubst (ExceptT TypeError Eval) where
  applyToEnv subst = ExceptT $ Eval $ \e ->
      case apply subst e of
          Just e' -> (Right (), e')
          Nothing -> (Left KindMismatch, e)

cacheExpr :: Int -> Eval (Maybe ValueState')
cacheExpr ident = gets (Map.lookup ident . globalCells) >>= \case
    -- Error if ident is unassigned
    Nothing -> pure Nothing
    -- Typecheck, evaluate, cache and return new value if invalidated
    Just c@Cell{cellType = type_, cellExpr = expr, cellValue = Invalidated} -> do
        let expr' = desugar $ maybe expr (Fix . ASTTApp expr) type_
        r <- runExceptT $ withExceptT TypeError $ typecheck expr'
        (v, t) <- case r of
            Left e -> pure (ValueError Nothing e, Nothing)
            Right (coreExpr, resultType) -> do
                result <- runExceptT $ withExceptT RuntimeError $ evalExpr coreExpr
                pure $ (,Just resultType) $ fromEither resultType result
        modifySheet $ Map.insert ident (c { cellType = t, cellValue = v })
        pure $ Just v
    -- Else return cached value
    Just Cell{cellValue = v} -> pure $ Just v

cacheByName :: String -> Eval (Maybe (PType, Either RuntimeError Value'))
cacheByName name = do
    ident <- gets $ globalCells >>>
        flip Map.foldrWithKey Nothing \k v -> \case
            Nothing -> if name == cellName v then Just k else Nothing
            found   -> found
    case ident of
        Just i -> cacheExpr i <&> \case
            Just (ValuePresent t v) -> Just (t, Right v)
            Just (ValueError (Just t) (RuntimeError e)) -> Just (t, Left e)
            _ -> Nothing
        Nothing -> pure Nothing

evalSheet :: Sheet -> Sheet
evalSheet (Sheet s) = Sheet $
    globalCells $ snd $ runEval (go $ invalidate s) initEnv
  where
    invalidate :: Map.Map Int Cell -> Map.Map Int Cell
    invalidate = Map.map $ \c -> c { cellValue = Invalidated }

    initEnv = GlobalEnv
        { globalCells = Map.empty
        , localTypes = stdFnTs
        , localValues = second pure <$> stdFnVals
        }

    go :: Map.Map Int Cell -> Eval ()
    go sheet = modifySheet (const sheet) >> mapM_ cacheExpr (Map.keys sheet)
