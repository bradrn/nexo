{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Nexo.Core.Typecheck (typecheck) where

import Control.Applicative (liftA2)
import Control.Monad (zipWithM)
import Control.Monad.State.Strict (execState, modify', State)
import Data.Bifunctor (first, second)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor.Foldable (cata)

import qualified Data.Map.Strict as Map

import Nexo.Core.Type
import Nexo.Expr.Type
import Nexo.Expr.Unit

typecheck :: MonadFail f => (String -> f Type) -> Expr -> f (CoreExpr, Type)
typecheck lookupName = cata $ \case
    XLit v -> pure $ (CLit v,) $
        case v of
            VNum _ -> TNum Uno
            VBool _ -> TBool
            VText _ -> TText
            VRecord _ -> error "typecheck: bug in parser"
            VList   _ -> error "typecheck: bug in parser"
    XList vs -> do
        vs' <- sequenceA vs
        let xs = fst <$> vs'
            ts = snd <$> vs'
        case nubOrd ts of
            [t] -> pure (CApp (Right "List") $ ((0,Nothing),) <$> xs, TList t)
            _ -> fail "#TYPE"
    XRecord kvs -> do
        kvs' <- sequenceA kvs
        let xs = fst <$> kvs'
            ts = snd <$> kvs'
        pure (CRec xs, TRecord ts)
    XField r' f -> do
        (r, tr) <- r'
        case tr of
            TRecord fs | Just tf <- Map.lookup f fs
              -> pure
                 ( CApp (Right "GetField")
                        [ ((0,Nothing),r)
                        , ((0,Nothing),CLit (VText f))
                        ]
                 , tf)
            _ -> fail "#TYPE"
    XVar v -> (CVar v,) <$> lookupName v
    XFun f vs -> do
        vs' <- sequenceA vs
        let xs = fst <$> vs'
            ts = snd <$> vs'
        (ps, t) <- unify ts =<< fntype f
        pure (CApp (Right f) (zip ps xs), t)
    XOp o x y -> do
        (xx, tx) <- x
        (xy, ty) <- y
        ([px, py], t) <- unify [tx, ty] =<< optype o
        pure (CApp (Left o) [(px, xx), (py, xy)], t)
    XUnit r' u -> do
        (r, tr) <- r'
        (_, t) <- unify [tr] (FunType [TNum Uno] (TNum u))
        pure (r, t)
  where
    fntype "If" = pure $ FunType [TBool, TVar "a", TVar "a"] (TVar "a")
    fntype "Mean" = pure $ FunType [TList (TNum $ UVar "u")] (TNum $ UVar "u")
    fntype "Avg"  = pure $ FunType [TList (TNum $ UVar "u")] (TNum $ UVar "u")
    fntype "PopStdDev" = pure $ FunType [TList (TNum $ UVar "u")] (TNum $ UVar "u")
    fntype "Median" = pure $ FunType [TList (TNum $ UVar "u")] (TNum $ UVar "u")
    fntype "Mode" = pure $ FunType [TList (TNum $ UVar "u")] (TNum $ UVar "u")
    fntype "Sin" = pure $ FunType [TNum (UName "rad")] (TNum Uno)
    fntype "Cos" = pure $ FunType [TNum (UName "rad")] (TNum Uno)
    fntype "Tan" = pure $ FunType [TNum (UName "rad")] (TNum Uno)
    fntype "InvSin" = pure $ FunType [TNum Uno] (TNum (UName "rad"))
    fntype "InvCos" = pure $ FunType [TNum Uno] (TNum (UName "rad"))
    fntype "InvTan" = pure $ FunType [TNum Uno] (TNum (UName "rad"))
    fntype "Root" = pure $ FunType [TNum Uno, TNum Uno] (TNum Uno)
    fntype "Power" = pure $ FunType [TNum Uno, TNum Uno] (TNum Uno)
    fntype _ = fail "#NAME"

    optype OEq    = pure $ FunType [TVar "a", TVar "a"] (TVar "a")
    optype ONeq   = pure $ FunType [TVar "a", TVar "a"] (TVar "a")
    optype OPlus  = pure $ FunType [TNum $ UVar "u", TNum $ UVar "u"] (TNum $ UVar "u")
    optype OMinus = pure $ FunType [TNum $ UVar "u", TNum $ UVar "u"] (TNum $ UVar "u")
    optype OTimes = pure $ FunType [TNum $ UVar "u", TNum $ UVar "v"] (TNum $ UMul (UVar "u") (UVar "v"))
    optype ODiv   = pure $ FunType [TNum $ UVar "u", TNum $ UVar "v"] (TNum $ UMul (UVar "u") (UVar "v"))
    optype OGt    = pure $ FunType [TNum $ UVar "u", TNum $ UVar "u"] TBool
    optype OLt    = pure $ FunType [TNum $ UVar "u", TNum $ UVar "u"] TBool
    optype OAnd   = pure $ FunType [TBool, TBool] TBool
    optype OOr    = pure $ FunType [TBool, TBool] TBool

    unify :: MonadFail f => [Type] -> FunType -> f ([(Int, Maybe Double)], Type)
    unify ts (FunType args out) = do
        targs <- zip' ts args
        let (tsubsts, usubsts) = getSubsts targs
        case liftA2 (,) (traverse meets tsubsts) (traverse concords usubsts) of
            Nothing -> fail "#TYPE"
            Just (tsubsts', usubsts') -> do
                let args' = usubst usubsts' . subst tsubsts' <$> args
                    out'  = usubst usubsts' . subst tsubsts'  $  out
                levels <- zipWithM match ts args'
                let maxlevel = if null levels then 0 else maximum (fst <$> levels)
                pure (levels, liftBy maxlevel out')

    zip' :: MonadFail f => [a] -> [b] -> f [(a, b)]
    zip' (a:as) (b:bs) = ((a,b) :) <$> zip' as bs
    zip' [] [] = pure []
    zip' _ _ = fail "#TYPE"

    getSubsts :: [(Type, Type)] -> (Map.Map String [Type], Map.Map String [UnitDef])
    getSubsts = second (fmap reverse) . flip execState (Map.empty, Map.empty) . go
      where
        go :: [(Type, Type)] -> State (Map.Map String [Type], Map.Map String [UnitDef]) ()
        go [] = pure ()
        go ((t,TVar v) : ts) =
            modify' (first $ Map.insertWith (++) v [t]) >> go ts
        go ((TNum u, TNum (UVar v)) : ts) =
            modify' (second $ Map.insertWith (++) v [u]) >> go ts
        go ((TList t1, TList t2) : ts) = go ((t1,t2) : ts)
        go (_ : ts) = go ts

    subst :: Map.Map String Type -> Type -> Type
    subst ss (TVar v) = case Map.lookup v ss of
        Nothing -> TVar v
        Just t -> subst ss t
    subst _ t = t

    usubst :: Map.Map String UnitDef -> Type -> Type
    usubst ss (TNum (UVar v)) = case Map.lookup v ss of
        Nothing -> TNum (UVar v)
        Just u -> usubst ss $ TNum u
    usubst _ t = t

    match :: MonadFail f => Type -> Type -> f (Int, Maybe Double)
    match t t' | t == t' = pure (0, Nothing)
    match (TList t) t' = first (1+) <$> match t t'
    match (TNum u) (TNum v) | Just f <- concord u v = pure (0, Just f)
    match _ _ = fail "#TYPE"

    liftBy :: Int -> Type -> Type
    liftBy 0 t = t
    liftBy n t = liftBy (n-1) (TList t)
