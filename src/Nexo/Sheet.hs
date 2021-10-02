{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

module Nexo.Sheet
       ( Sheet(..)
       , Cell(..)
       , ValueState(..)
       , display
       , evalSheet
       , insert
       ) where

import Control.Monad.State.Strict
    ( execState, gets, modify', put, state, runState, State, StateT (..), MonadState )
import Control.Monad.Trans (lift)
import Data.Fix (Fix(Fix))
import qualified Data.Map.Strict as Map

import Nexo.Core.Typecheck
import Nexo.Expr.Type
import Nexo.Interpret

data ValueState
    = ValuePresent PType Value
    | ValueError String
    | Invalidated
    deriving (Show)

display :: ValueState -> String
display (ValuePresent _ v) = render v
display (ValueError e) = '#' : e
display Invalidated = "#INVALIDATED"

fromEither :: PType -> Either String Value -> ValueState
fromEither t (Right val) = ValuePresent t val
fromEither _ (Left err) = ValueError err

toEither :: ValueState -> Either String (PType, Value)
toEither (ValuePresent t val) = Right (t, val)
toEither (ValueError err) = Left err
toEither Invalidated = Left "#INVALIDATED"
    
data Cell = Cell
    { cellName :: String
    , cellType :: Maybe PType
    , cellExpr :: Expr
    , cellValue :: ValueState
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

    cacheExpr :: Int -> State (Map.Map Int Cell) ValueState
    cacheExpr ident = gets (Map.lookup ident) >>= \case
        -- Error if ident is unassigned
        Nothing -> pure $ ValueError "#IREF"
        -- Typecheck, evaluate, cache and return new value if invalidated
        Just c@Cell{cellType = type_, cellExpr = expr, cellValue = Invalidated} -> do
            let expr' = maybe expr (Fix . XTApp expr) type_
            r <- lower $ typecheck (fmap fst . cacheByName) expr'
            (v, t) <- case r of
                Left e -> pure (ValueError e, Nothing)
                Right (coreExpr, resultType) -> do
                    result <- lower $ evalExpr (fmap snd . cacheByName) coreExpr
                    pure $ (,Just resultType) $ fromEither resultType result
            modify' $ Map.insert ident (c { cellType = t, cellValue = v })
            pure v
        -- Else return cached value
        Just Cell{cellValue = v} -> pure v

    cacheByName :: String -> Eval (PType, Value)
    cacheByName name = do
        ident <- gets $
            flip Map.foldrWithKey Nothing $ \k v -> \case
                Nothing -> if name == cellName v then Just k else Nothing
                found   -> found
        case ident of
            Just i -> raise $ toEither <$> cacheExpr i
            Nothing -> fail "#REF"
