{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Nexo.Core.Typecheck where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State.Strict (evalStateT)
import Data.Bifunctor (second)
import Data.Fix (Fix(Fix))
import Data.Foldable (for_)
import Data.Functor.Foldable (para)
import Data.Traversable (for, forM)

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
    :: ( MonadEnv PType m
       , MonadError String m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Conversion -> (Expr, (CoreExpr, Type)) -> m (Int, CoreExpr)
applyConversion (UnliftBy 0 conv) (x, (c, t)) = applyConversion conv (x, (c, t))
applyConversion (UnliftBy n conv) (x, (c, t)) = do
    (n', c') <- applyConversion conv (x, (c, t))
    pure (n+n', c')
applyConversion (MultiplyBy f conv) (x, (c, t)) = do
    -- need to re-run typechecker to make sure we insert unlifts correctly
    -- note that we avoid infinite recursion by doing all calculations with concordant units

    let removeUnits (TList a) = TList $ removeUnits a
        removeUnits (TNum _) = TNum Uno
        removeUnits _ = error "applyConversion: bug in inferStep"
        t' = removeUnits t

    let xNum = Fix (XAtom (Lit (LNum f)))
    timesType <- instantiate $ Forall [] ["u", "v"] $ TFun [TNum $ UVarR "u", TNum $ UVarR "v"] (TNum $ UMul (UVarR "u") (UVarR "v"))
    c' <- inferStep $ XFunApp (Fix (XAtom (Var "*")), pure (CVar "*", timesType))
        [ (xNum, pure (CLit (LNum f), TNum Uno))
        , (x, pure (c, t'))
        ]
    applyConversion conv (Fix $ XNamedFunApp "*" [xNum, x], c')
applyConversion IdConversion (_, (c, _)) = pure (0, c)

getConvertedExpr
    :: ( MonadEnv PType m
       , MonadError String m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Subst -> (Expr, (CoreExpr, Type)) -> Type -> m ((Int, CoreExpr), Conversion)
getConvertedExpr s (x, (suppliedExpr, suppliedT)) declaredT = do
    suppliedT' <- whenJustElse "#TYPE" $ apply s suppliedT
    declaredT' <- whenJustElse "#TYPE" $ apply s declaredT

    let conv = getConversion suppliedT' declaredT'
    (,conv) <$> applyConversion conv (x, (suppliedExpr, suppliedT'))

liftBy :: Int -> Type -> Type
liftBy 0 t = t
liftBy n t = TList $ liftBy (n-1) t

getConvertedArgs
    :: ( MonadEnv PType m
       , MonadError String m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Subst -> ([(Expr, (CoreExpr, Type))], Type) -> Type -> m ([(Int, CoreExpr)], Type)
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

traverseExprs :: Applicative m => [(x, m y)] -> m [(x, y)]
traverseExprs = traverse $ \(x, y) -> (x,) <$> y

-- This uses a variant of Hindley-Milner. Rather than composing all
-- the substitutions then applying at the end, instead it applies each
-- substitution as it is found. This allows it to compare function
-- arguments to their expected types so that it can apply conversions
-- correctly.
inferStep
    :: forall m e.
       ( MonadEnv PType m
       , MonadError String m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => ExprF (Expr, m (CoreExpr, Type))
    -> m (CoreExpr, Type)
inferStep = \case
    XAtom Null -> (CNull,) . TVar <$> fresh
    XAtom (Lit n@(LNum _)) -> pure (CLit n, TNum Uno)
    XAtom (Lit n@(LBool _)) -> pure (CLit n, TBool)
    XAtom (Lit n@(LText _)) -> pure (CLit n, TText)
    XAtom (Var v) -> do
        t <- instantiate =<< lookupName v
        pure (CVar v, t)
    XRecord Nonrecursive r' _ -> do
        r <- traverse snd r'
        pure (CRec Nonrecursive $ Map.toList $ fst <$> r, TRecord $ snd <$> r)
    XRecord Recursive r' order -> do
        tsDeclared <- traverse ((fmap.fmap) TVar $ const fresh) r'

        scope $ do
            _ <- Map.traverseWithKey (curry extend) $ Forall [] [] <$> tsDeclared

            let r'WithTvs :: Map.Map String ((Expr, m (CoreExpr, Type)), Type)
                r'WithTvs = Map.merge
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XRecord Recursive case")
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XRecord Recursive case")
                    (Map.zipWithMatched $ const (,))
                    r' tsDeclared

                r'WithTvsOrdered :: [(String, ((Expr, m (CoreExpr, Type)), Type))]
                r'WithTvsOrdered = (\k -> (k, r'WithTvs Map.! k)) <$> order

            r <- for r'WithTvsOrdered $ \(name, ((x, r'Elem), tDeclared)) -> do
                rElem@(_, tSupplied) <- r'Elem
                tInferred <- (\(Forall [] [] t) -> t) <$> lookupName name
                s <- solve
                    [ Unify tSupplied tDeclared  -- to get subst. for type variable
                    , Unify tInferred tDeclared  -- to ensure it's consistent with previous inferences
                    ]
                xConverted <- snd . fst <$> getConvertedExpr s (x, rElem) tDeclared
                applyToEnv s
                t <- whenJustElse "#TYPE" $ apply s tDeclared
                pure (name, (xConverted, t))

            pure (CRec Recursive (second fst <$> r), TRecord (snd <$> Map.fromList r))
    XFunApp (Fix (XAtom (Var "List")), _) xs' -> do
        xs <- traverseExprs xs'
        tv <- TVar <$> fresh
        let cs = Unify tv . snd . snd <$> xs
        s <- solve cs

        elemsConverted <- traverse (fmap fst . flip (getConvertedExpr s) tv) xs

        t <- whenJustElse "#TYPE" $ apply s tv
        applyToEnv s

        pure (CApp (CVar "List") elemsConverted, TList t)
    XFunApp (Fix (XAtom (Var "Table")), _) r' -> traverse snd r' >>= \case
        -- for 'Table', need to know AT POINT OF TYPECHECKING what the fields are
        -- (unless we get fancy type-level extensible row types!)
        [(x, TRecord r)] -> do
            ts <- forM r $ \tSupplied -> do
                tvar <- TVar <$> fresh
                let tDeclared = TList tvar
                s <- unify $ Unify tSupplied tDeclared
                applyToEnv s
                whenJustElse "#TYPE" $ apply s tvar
            pure (CTab x, TTable ts)
        _ -> throwError "#TYPE"
    XFunApp (Fix (XAtom (Var "Lambda")), _) xs -> do
        let unVar (Fix (XAtom (Var v))) = pure v
            unVar _ = throwError "#SYNTAX"
            (_, x) = last xs
        args <- traverse (unVar . fst) $ init xs
        tvs <- for args $ \arg -> (arg,) <$> fresh
        ((retc, rett), argts) <- scope $ do
            for_ tvs $ extend . second (Forall [] [] . TVar)
            ret <- x
            argts <- traverse (instantiate <=< lookupName) args
            pure (ret, argts)
        pure (CLam args retc, TFun argts rett)
    XFunApp (Fix (XAtom (Var "GetField")), _) [(_, x), (Fix (XAtom (Var f)), _)] -> do
        (r, rt) <- x
        tv <- fresh
        -- unify with minimal record which could work
        let c = Subtype rt (TRecord $ Map.singleton f $ TVar tv)
        s <- solve [c]
        -- then back-substitute
        t <- whenJustElse "#TYPE" $ apply s (TVar tv)
        applyToEnv s
        pure (CApp (CVar "GetField") [(0,r), (0,CLit (LText f))], t)
    XFunApp (_, f) args' -> do
        (c, tfun) <- f
        args <- traverseExprs args'
        tv <- fresh
        s <- solve [Subtype tfun (TFun (snd . snd <$> args) (TVar tv))]

        (argsConverted, ret) <- getConvertedArgs s (args, TVar tv) tfun
        applyToEnv s
        
        pure (CApp c argsConverted, ret)
    XUnitApp (_, x') u -> do
        (x, t) <- x'
        _ <- solve [Subtype t (TNum Uno)]

        case getConversion t (TNum Uno) of
            UnliftBy n _ -> pure (x, liftBy n $ TNum u)
            _            -> pure (x, TNum u)
    XTypeApp (orig, x') pty -> do
        let t' = instantiateRigid pty
        (x, t) <- x'
        s <- unify (Unify t t')

        ((_0, xConverted), _) <- getConvertedExpr s (orig, (x, t)) t'
        applyToEnv s
        pure (xConverted, t')

typecheck
    :: ( MonadEnv PType f
       , MonadError String f
       , MonadScoped e f
       , MonadSubst f
       ) => Expr -> f (CoreExpr, PType)
typecheck = flip evalStateT (0::Int) . fmap (second generalise) . para inferStep
