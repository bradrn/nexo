{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Nexo.Core.Typecheck where

import Control.Monad ((<=<))
import Control.Monad.State.Strict (evalStateT)
import Data.Bifunctor (second)
import Data.Foldable (for_)
import Data.Functor.Foldable (cata)
import Data.Traversable (for)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import Nexo.Core.Solve
import Nexo.Core.Substitute
import Nexo.Core.Type
import Nexo.Expr.Type
import Nexo.Expr.Unit
import Nexo.Env

data Conversion
    = MultiplyBy Double Conversion
    | UnliftBy Int Conversion
    | IdConversion
  deriving (Show)

getConversion :: Type -> Type -> Conversion
getConversion supplied@(TList _) declared =
    let suppliedLists = getNestedListNum supplied
        declaredLists = getNestedListNum declared

        diff = suppliedLists - declaredLists

        -- in this case supplied is more lifted than declared; so
        -- strip lists of supplied so both are lifted same amount
        supplied' = stripLists diff supplied
    in case diff of
        0 | TList s' <- supplied
          , TList d' <- declared
          -> getConversion s' d'
        _ -> UnliftBy diff $ getConversion supplied' declared
  where
    getNestedListNum (TList t) = 1 + getNestedListNum t
    getNestedListNum _ = 0

    stripLists 0 t = t
    stripLists n (TList t) | n>0 = stripLists (n-1) t
    stripLists _ _ = error "getConversion: bug in unifier"
getConversion (TNum uSupplied) (TNum uDeclared) =
    case concord uSupplied uDeclared of
        Just 1 -> IdConversion
        Just f -> MultiplyBy f IdConversion
        Nothing -> error "getConversion: bug in unifier"
getConversion t1 t2
    | t1 == t2 = IdConversion
    | otherwise = error "getConversion: bug in unifier"

applyConversion
    :: ( MonadEnv PType e m
       , MonadFail m
       , MonadFresh m
       , MonadSubst m
       )
    => Conversion -> (CoreExpr, Type) -> m (Int, CoreExpr)
applyConversion (UnliftBy 0 c) (x, t) = applyConversion c (x, t)
applyConversion (UnliftBy n c) (x, t) = do
    (n', x') <- applyConversion c (x, t)
    pure (n+n', x')
applyConversion (MultiplyBy f c) (x, t) = do
    -- need to re-run typechecker to make sure we insert unlifts correctly
    -- note that we avoid infinite recursion by doing all calculations with concordant units

    let removeUnits (TList a) = TList $ removeUnits a
        removeUnits (TNum _) = TNum Uno
        removeUnits _ = error "applyConversion: bug in inferStep"
        t' = removeUnits t

    x' <- inferStep $ XOp OTimes (pure (CLit (LNum f), TNum Uno)) (pure (x, t'))
    applyConversion c x'
applyConversion IdConversion (x, _) = pure (0, x)

getConvertedExpr
    :: ( MonadEnv PType e m
       , MonadFail m
       , MonadFresh m
       , MonadSubst m
       )
    => Subst -> (CoreExpr, Type) -> Type -> m ((Int, CoreExpr), Conversion)
getConvertedExpr s (suppliedExpr, suppliedT) declaredT = do
    suppliedT' <- whenJustElse "#TYPE" $ apply s suppliedT
    declaredT' <- whenJustElse "#TYPE" $ apply s declaredT

    let conv = getConversion suppliedT' declaredT'
    (,conv) <$> applyConversion conv (suppliedExpr, suppliedT')

liftBy :: Int -> Type -> Type
liftBy 0 t = t
liftBy n t = TList $ liftBy (n-1) t

getConvertedArgs
    :: ( MonadEnv PType e m
       , MonadFail m
       , MonadFresh m
       , MonadSubst m
       )
    => Subst -> ([(CoreExpr, Type)], Type) -> Type -> m ([(Int, CoreExpr)], Type)
getConvertedArgs s (supplied, ret) declaredT = do
    let declaredTs = getTFunArgs declaredT
    (convertedArgs, convs) <- unzip <$>
        traverse2 "#TYPE" (getConvertedExpr s) supplied declaredTs
    ret' <- liftBy (getMaxLift convs) <$>
        whenJustElse "#TYPE" (apply s ret)
    pure (convertedArgs, ret')
  where
    getTFunArgs (TFun args _) = args
    getTFunArgs _ = error "getConvertedArgs: bug in unifier"

    getMaxLift = foldr (\case {UnliftBy n _ -> max n ; _ -> id}) 0

-- This uses a variant of Hindley-Milner. Rather than composing all
-- the substitutions then applying at the end, instead it applies each
-- substitution as it is found. This allows it to compare function
-- arguments to their expected types so that it can apply conversions
-- correctly.
inferStep
    :: ( MonadEnv PType e m
       , MonadFail m
       , MonadFresh m
       , MonadSubst m
       )
    => ExprF (m (CoreExpr, Type))
    -> m (CoreExpr, Type)
inferStep = \case
    XLit n@(LNum _) -> pure (CLit n, TNum Uno)
    XLit n@(LBool _) -> pure (CLit n, TBool)
    XLit n@(LText _) -> pure (CLit n, TText)
    XList xs' -> do
        xs <- sequenceA xs'
        tv <- TVar <$> fresh
        let cs = Unify tv . snd <$> xs
        s <- whenJustElse "#TYPE" $ solve cs

        elemsConverted <- traverse (fmap fst . flip (getConvertedExpr s) tv) xs

        t <- whenJustElse "#TYPE" $ apply s tv
        applyToEnv s

        pure (CApp (Right "List") elemsConverted, TList t)
    XRecord r' -> do
        r <- sequenceA r'
        pure (CRec $ fst <$> r, TRecord $ snd <$> r)
    XTable r' -> do
        tvs <- traverse ((fmap.fmap) TVar $ const fresh) r'
        let tsDeclared = TList <$> tvs
        scope $ do
            _ <- Map.traverseWithKey (curry extend) $ Forall [] [] <$> tsDeclared

            r <- sequenceA r'
            let rWithTvs :: Map.Map String ((CoreExpr, Type), Type)
                rWithTvs = Map.merge
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XTable case")
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XTable case")
                    (Map.zipWithMatched $ const (,))
                    r tsDeclared

            let (<&>) = flip fmap  -- for convenience
                cs = Map.elems $ rWithTvs <&> \((_, tSupplied), tDeclared) -> Unify tSupplied (tDeclared)
            s <- whenJustElse "#TYPE" $ solve cs

            rConverted <- for rWithTvs $ \((x, tSupplied), tDeclared) ->
                    snd . fst <$> getConvertedExpr s (x, tSupplied) tDeclared

            ts <- traverse (whenJustElse "#TYPE" . apply s) tvs
            applyToEnv s

            pure (CTab rConverted, TTable ts)
    XVar v -> do
        t <- instantiate =<< lookupName v
        pure (CVar v, t)
    XLet v t' vx' x' -> do
        tDeclared <- maybe (TVar <$> fresh) instantiate t'
        (vx, tSupplied) <- vx'
        s <- whenJustElse "#TYPE" $ unify (Unify tSupplied tDeclared)

        ((0, vxConverted), _) <- getConvertedExpr s (vx, tSupplied) tDeclared

        t <- whenJustElse "#TYPE" $ apply s tDeclared
        applyToEnv s

        scope $ do
            extend (v, Forall [] [] t)
            (xRet, tRet) <- x'
            pure (CLet v vxConverted xRet, tRet)
    XLam args x -> do
        tvs <- for args $ \arg -> (arg,) <$> fresh
        ((retc, rett), argts) <- scope $ do
            for_ tvs $ extend . second (Forall [] [] . TVar)
            ret <- x
            argts <- traverse (instantiate <=< lookupName) args
            pure (ret, argts)
        pure (CLam args retc, TFun argts rett)
    XField x f -> do
        (r, rt) <- x
        tv <- fresh
        -- unify with minimal record which could work
        let c = Subtype rt (TRecord $ Map.singleton f $ TVar tv)
        s <- whenJustElse "#TYPE" $ solve [c]
        -- then back-substitute
        t <- whenJustElse "#TYPE" $ apply s (TVar tv)
        applyToEnv s
        pure (CApp (Right "GetField") [(0,r), (0,CLit (LText f))], t)
    XFun f args' -> do
        tfun <- instantiate =<< lookupName f
        args <- sequenceA args'
        tv <- fresh
        s <- whenJustElse "#TYPE" $ solve [Subtype tfun (TFun (snd <$> args) (TVar tv))]

        (argsConverted, ret) <- getConvertedArgs s (args, TVar tv) tfun
        
        pure (CApp (Right f) argsConverted, ret)
    XOp o arg1' arg2' -> do
        tfun <- instantiate =<< optype o
        arg1@(_, t1) <- arg1'
        arg2@(_, t2) <- arg2'
        tv <- fresh
        s <- whenJustElse "#TYPE" $ solve [Subtype tfun (TFun [t1,t2] (TVar tv))]

        (argsConverted, ret) <- getConvertedArgs s ([arg1,arg2], TVar tv) tfun
        applyToEnv s

        pure (CApp (Left o) argsConverted, ret)
    XUnit x' u -> do
        (x, t) <- x'
        _ <- whenJustElse "#TYPE" $ solve [Subtype t (TNum Uno)]

        case getConversion t (TNum Uno) of
            UnliftBy n _ -> pure (x, liftBy n $ TNum u)
            _            -> pure (x, TNum u)
    XTApp x' pty -> do
        t' <- instantiate pty
        (x, t) <- x'
        s <- whenJustElse "#TYPE" $ unify (Unify t t')

        ((0, xConverted), _) <- getConvertedExpr s (x, t) t'
        applyToEnv s
        pure (xConverted, t')

typecheck :: (MonadEnv PType e f, MonadFail f, MonadSubst f) => Expr -> f (CoreExpr, PType)
typecheck = flip evalStateT (0::Int) . fmap (second generalise) . cata inferStep

optype :: MonadFail m => Op -> m PType
optype OEq    = pure $ Forall [] ["a"]      $ TFun [TVar "a", TVar "a"] (TVar "a")
optype ONeq   = pure $ Forall [] ["a"]      $ TFun [TVar "a", TVar "a"] (TVar "a")
optype OPlus  = pure $ Forall [] ["u"]      $ TFun [TNum $ UVar "u", TNum $ UVar "u"] (TNum $ UVar "u")
optype OMinus = pure $ Forall [] ["u"]      $ TFun [TNum $ UVar "u", TNum $ UVar "u"] (TNum $ UVar "u")
optype OTimes = pure $ Forall [] ["u", "v"] $ TFun [TNum $ UVar "u", TNum $ UVar "v"] (TNum $ UMul (UVar "u") (UVar "v"))
optype ODiv   = pure $ Forall [] ["u", "v"] $ TFun [TNum $ UVar "u", TNum $ UVar "v"] (TNum $ UMul (UVar "u") (UVar "v"))
optype OGt    = pure $ Forall [] ["u"]      $ TFun [TNum $ UVar "u", TNum $ UVar "u"] TBool
optype OLt    = pure $ Forall [] ["u"]      $ TFun [TNum $ UVar "u", TNum $ UVar "u"] TBool
optype OAnd   = pure $ Forall [] []         $ TFun [TBool, TBool] TBool
optype OOr    = pure $ Forall [] []         $ TFun [TBool, TBool] TBool
