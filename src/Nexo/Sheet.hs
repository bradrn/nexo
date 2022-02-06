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
import Data.Fix (Fix(Fix))
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(Identity))
import Data.Set (unions)

import qualified Data.Map.Strict as Map

import Nexo.Core.Substitute
import Nexo.Core.Typecheck
import Nexo.Expr.Type
import Nexo.Interpret
import Nexo.Env (MonadScoped(..), MonadEnv(..), MonadSubst(..))
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

data Sheet = Sheet
    { sheetImports :: [String]
    , sheetImportsCached :: Maybe [Map.Map String (PType, Value GlobalEnv)]
    , getSheet :: Map.Map Int Cell
    }
    deriving (Show, Eq)

insert :: Int -> Cell -> Sheet -> Sheet
insert k v s@Sheet{getSheet=sheet} = s { getSheet = Map.insert k v sheet }

data GlobalEnv = GlobalEnv
    { imports :: [Map.Map String (PType, Value GlobalEnv)]
    , globalCells :: Map.Map Int Cell
    , localTypes :: [(String, PType)]
    , localValues :: [(String, ExceptT RuntimeError Eval Value')]
    }
instance Show GlobalEnv where
    show _ = "\\VSE[]"

instance Substitutable GlobalEnv where
    apply s GlobalEnv{..} =
        flip (GlobalEnv imports globalCells) localValues <$>  -- assume other cells have no frees
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
            Just t -> pure t
            Nothing -> lift (cacheByName n) >>= \case
                Just (t, _) -> pure t
                Nothing -> lift (lookupImports n) >>= \case
                    Just (t, _) -> pure t
                    Nothing -> case Map.lookup n stdFns of
                        Just (t, _) -> pure t
                        Nothing -> throwError $ UnknownName n
    extend b = lift $ modify' $ \e@GlobalEnv{localTypes} ->
        e{localTypes=b:localTypes}

instance MonadEnv (ExceptT RuntimeError Eval Value') (ExceptT RuntimeError Eval) where
    lookupName n = do
        env <- getEnv
        case lookup n (localValues env) of
            Just v -> pure v
            Nothing -> lift (cacheByName n) >>= \case
                Just (_, Left e) -> throwError e
                Just (_, Right v) -> pure $ pure v
                Nothing -> lift (lookupImports n) >>= \case
                    Just (_, v) -> pure $ pure v
                    Nothing -> case Map.lookup n stdFns of
                        Just (_, v) -> pure $ pure v
                        Nothing -> error "lookupName: bug in typechecker"
    extend b = lift $ modify' $ \e@GlobalEnv{localValues} ->
        e{localValues=b:localValues}

instance MonadSubst (ExceptT TypeError Eval) where
  applyToEnv subst = ExceptT $ Eval $ \e ->
      case apply subst e of
          Just e' -> (Right (), e')
          Nothing -> (Left KindMismatch, e)

lookupImports :: String -> Eval (Maybe (PType, Value GlobalEnv))
lookupImports n = asum . fmap (Map.lookup n) <$> gets imports

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

evalSheet
    :: Applicative m
    => (String -> m (Map.Map String (PType, Value GlobalEnv)))
    -> Sheet
    -> m Sheet
evalSheet getImport (Sheet is cached s) = do
    maybe (traverse getImport is) pure cached <&> \is' ->
        Sheet is (Just is')
            (globalCells $ snd $ runEval (go $ invalidate s) (initEnv is'))
  where
    invalidate :: Map.Map Int Cell -> Map.Map Int Cell
    invalidate = Map.map $ \c -> c { cellValue = Invalidated }

    initEnv is' = GlobalEnv
        { imports = is'
        , globalCells = Map.empty
        , localTypes = []
        , localValues = []
        }

    go :: Map.Map Int Cell -> Eval ()
    go sheet = modifySheet (const sheet) >> mapM_ cacheExpr (Map.keys sheet)
