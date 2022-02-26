{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Nexo.Core.Kind where

import Control.Monad.Except (MonadError(throwError))
import Control.Monad.State.Strict (StateT, gets, modify, evalStateT)
import Data.Functor ((<&>))

import qualified Data.Map.Strict as Map

import qualified Nexo.Core.Type as Core
import Nexo.Core.Unit (inferUnit)
import qualified Nexo.Expr.Type as Expr

infer :: MonadError Core.TypeError m => Expr.Type -> m (Core.Type, Core.Kind)
infer = flip evalStateT Map.empty . go
  where
    go :: MonadError Core.TypeError m => Expr.Type -> StateT (Map.Map String Core.Kind) m (Core.Type, Core.Kind)
    go (Expr.TNum u) = inferUnit u <&> \u' ->
        (Core.TNum $ Core.TUnit u', Core.Type)
    go Expr.TBool = pure (Core.TBool, Core.Type)
    go Expr.TText = pure (Core.TText, Core.Type)
    go (Expr.TVar s) =
        gets (Map.lookup s) >>= \case
            Just k -> pure (Core.TVar (Core.Rigid s), k)
            Nothing -> (Core.TVar (Core.Rigid s), Core.Type) <$ modify (Map.insert s Core.Type)
    go (Expr.TFun args t) = do
        (args', argks) <- unzip <$> traverse go args
        (t', k) <- go t
        if (k == Core.Type) && all (==Core.Type) argks
            then pure (Core.TFun args' t', Core.Type)
            else throwError Core.KindMismatch
    go (Expr.TList t) = go t >>= \case
        (t', Core.Type) -> pure (Core.TList t', Core.Type)
        _ -> throwError Core.KindMismatch
    go (Expr.TRecord r) = do
        r' <- traverse go r
        if all (==Core.Type) (Map.elems $ snd <$> r')
            then pure (Core.TRecord $ fst <$> r', Core.Type)
            else throwError Core.KindMismatch
    go (Expr.TTable r) = do
        r' <- traverse go r
        if all (==Core.Type) (Map.elems $ snd <$> r')
            then pure (Core.TRecord $ fst <$> r', Core.Type)
            else throwError Core.KindMismatch
