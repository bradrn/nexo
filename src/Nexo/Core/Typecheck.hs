{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE LambdaCase                   #-}
{-# LANGUAGE TupleSections                #-}
{-# LANGUAGE TypeApplications             #-}

module Nexo.Core.Typecheck where

import Control.Monad.State.Strict (evalStateT, MonadTrans (lift))
import Data.Bifunctor (second)
import Data.Functor.Foldable (cata)

import qualified Data.Map.Strict as Map

import Nexo.Core.Solve
import Nexo.Core.Substitute
import Nexo.Core.Type
import Nexo.Expr.Type
import Nexo.Expr.Unit

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
    :: ( MonadFail m
       , MonadFresh m
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

    x' <- inferStep (error "applyConversion: bug in inferStep") $
        XOp OTimes (pure (CLit (VNum f), TNum Uno)) (pure (x, t'))
    applyConversion c x'
applyConversion IdConversion (x, _) = pure (0, x)

-- This uses a variant of Hindley-Milner. Rather than composing all
-- the substitutions then applying at the end, instead it applies each
-- substitution as it is found. This allows it to compare function
-- arguments to their expected types so that it can apply conversions
-- correctly.
inferStep
    :: ( MonadFail m
       , MonadFresh m
       )
    => (String -> m PType)
    -> ExprF (m (CoreExpr, Type))
    -> m (CoreExpr, Type)
inferStep lookupName = \case
    XLit n@(VNum _) -> pure (CLit n, TNum Uno)
    XLit n@(VBool _) -> pure (CLit n, TBool)
    XLit n@(VText _) -> pure (CLit n, TText)
    XLit _ -> error "inferStep: bug in parser"
    XList xs' -> do
        xs <- sequenceA xs'
        tv <- TVar <$> fresh
        let cs = Unify tv . snd <$> xs
        s <- whenJustElse "#TYPE" $ solve cs
        t <- whenJustElse "#TYPE" $ apply s tv
        pure (CApp (Right "List") $ (0,) . fst <$> xs, TList t)
    XRecord r' -> do
        r <- sequenceA r'
        pure (CRec $ fst <$> r, TRecord $ snd <$> r)
    XVar v -> do
        t <- instantiate =<< lookupName v
        pure (CVar v, t)
    XField x f -> do
        (r, rt) <- x
        tv <- fresh
        -- unify with minimal record which could work
        let c = Subtype rt (TRecord $ Map.singleton f $ TVar tv)
        s <- whenJustElse "#TYPE" $ solve [c]
        -- then back-substitute
        t <- whenJustElse "#TYPE" $ apply s (TVar tv)
        pure (CApp (Right "GetField") [(0,r), (0,CLit (VText f))], t)
    XFun f args' -> do
        tfun <- instantiate =<< fntype f
        (args, argts) <- unzip <$> sequenceA args'
        tv <- fresh
        s <- whenJustElse "#TYPE" $ solve [Subtype tfun (TFun argts (TVar tv))]

        argtsSupplied <- traverse (whenJustElse "#TYPE" . apply s) argts
        argtsDeclared <- whenJustElse "#TYPE" $ getTFunArgs <$> apply s tfun

        let convs = zipWith getConversion argtsSupplied argtsDeclared :: [Conversion]
            argsWithTs = zip args argtsSupplied :: [(CoreExpr, Type)]
        argsConverted <- traverse2 "#TYPE" applyConversion convs argsWithTs
        
        ret <- whenJustElse "#TYPE" $ apply s (TVar tv)
        let ret' = liftBy (getMaxLift convs) ret

        pure (CApp (Right f) argsConverted, ret')
    XOp o arg1' arg2' -> do
        tfun <- instantiate =<< optype o
        (arg1, t1) <- arg1'
        (arg2, t2) <- arg2'
        tv <- fresh
        s <- whenJustElse "#TYPE" $ solve [Subtype tfun (TFun [t1,t2] (TVar tv))]

        arg1Supplied <- whenJustElse "#TYPE" $ apply s t1
        arg2Supplied <- whenJustElse "#TYPE" $ apply s t2
        [arg1Declared, arg2Declared] <- whenJustElse "#TYPE" $ getTFunArgs <$> apply s tfun

        let conv1 = getConversion arg1Supplied arg1Declared
            conv2 = getConversion arg2Supplied arg2Declared
        arg1Converted <- applyConversion conv1 (arg1, t1)
        arg2Converted <- applyConversion conv2 (arg2, t2)

        ret <- whenJustElse "#TYPE" $ apply s (TVar tv)
        let ret' = liftBy (getMaxLift [conv1, conv2]) ret

        pure (CApp (Left o) [arg1Converted,arg2Converted], ret')
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

        tSupplied <- whenJustElse "#TYPE" $ apply s t
        tDeclared <- whenJustElse "#TYPE" $ apply s t'
        let conv = getConversion tSupplied tDeclared
        (0, xConverted) <- applyConversion conv (x, t)
        pure (xConverted, t')
  where
    getTFunArgs (TFun args _) = args
    getTFunArgs _ = error "inferStep: bug in unifier"

    getMaxLift = foldr (\case {UnliftBy n _ -> max n ; _ -> id}) 0

    liftBy 0 t = t
    liftBy n t = TList $ liftBy (n-1) t

typecheck :: MonadFail f => (String -> f PType) -> Expr -> f (CoreExpr, PType)
typecheck lookupName = flip evalStateT (0::Int) . fmap (second generalise) . cata (inferStep lookupName')
  where
    lookupName' = lift . lookupName

fntype :: MonadFail m => String -> m PType
fntype "If" = pure        $ Forall ["a"] [] $ TFun [TBool, TVar "a", TVar "a"] (TVar "a")
fntype "Mean" = pure      $ Forall [] ["u"] $ TFun [TList (TNum $ UVar "u")] (TNum $ UVar "u")
fntype "Avg"  = pure      $ Forall [] ["u"] $ TFun [TList (TNum $ UVar "u")] (TNum $ UVar "u")
fntype "PopStdDev" = pure $ Forall [] ["u"] $ TFun [TList (TNum $ UVar "u")] (TNum $ UVar "u")
fntype "Median" = pure    $ Forall [] ["u"] $ TFun [TList (TNum $ UVar "u")] (TNum $ UVar "u")
fntype "Mode" = pure      $ Forall [] ["u"] $ TFun [TList (TNum $ UVar "u")] (TNum $ UVar "u")
fntype "Sin" = pure       $ Forall [] []    $ TFun [TNum (UName "rad")] (TNum Uno)
fntype "Cos" = pure       $ Forall [] []    $ TFun [TNum (UName "rad")] (TNum Uno)
fntype "Tan" = pure       $ Forall [] []    $ TFun [TNum (UName "rad")] (TNum Uno)
fntype "InvSin" = pure    $ Forall [] []    $ TFun [TNum Uno] (TNum (UName "rad"))
fntype "InvCos" = pure    $ Forall [] []    $ TFun [TNum Uno] (TNum (UName "rad"))
fntype "InvTan" = pure    $ Forall [] []    $ TFun [TNum Uno] (TNum (UName "rad"))
fntype "Root" = pure      $ Forall [] []    $ TFun [TNum Uno, TNum Uno] (TNum Uno)
fntype "Power" = pure     $ Forall [] []    $ TFun [TNum Uno, TNum Uno] (TNum Uno)
fntype _ = fail "#NAME"

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
