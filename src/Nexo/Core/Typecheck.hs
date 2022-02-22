{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Nexo.Core.Typecheck where

import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.State.Strict (evalStateT)
import Data.Bifunctor (second)
import Data.Fix (Fix(Fix))
import Data.Foldable (for_)
import Data.Functor.Foldable (para)
import Data.Traversable (for)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import Nexo.Core.Solve
import Nexo.Core.Substitute
import Nexo.Core.Type
import Nexo.Core.Unit
import Nexo.Expr.Type
import Nexo.Env

whenJustElse :: MonadError e m => e -> Maybe a -> m a
whenJustElse e = maybe (throwError e) pure

whenRightElse :: MonadError e m => (c -> e) -> Either c a -> m a
whenRightElse e = either (throwError . e) pure

toMismatch :: Constraint -> Mismatch
toMismatch (Unify   tSupplied tDeclared) = Mismatch { tSupplied, tDeclared }
toMismatch (Subtype tSupplied tDeclared) = Mismatch { tSupplied, tDeclared } 

apply' :: Substitutable a => Subst -> a -> a
apply' = apply
    
data Conversion
    = MultiplyBy Double Conversion
    | UnliftBy Int Conversion
    | IdConversion
  deriving (Show)

getConversion :: CoreType -> CoreType -> Conversion
getConversion supplied@(CList _) declared =
    let suppliedLists = getNestedListNum supplied
        declaredLists = getNestedListNum declared

        diff = suppliedLists - declaredLists

        -- in this case supplied is more lifted than declared; so
        -- strip lists of supplied so both are lifted same amount
        supplied' = stripLists diff supplied
    in case diff of
        0 | CList s' <- supplied
          , CList d' <- declared
          -> getConversion s' d'
        _ -> UnliftBy diff $ getConversion supplied' declared
  where
    getNestedListNum (CList t) = 1 + getNestedListNum t
    getNestedListNum _ = 0

    stripLists 0 t = t
    stripLists n (CList t) | n>0 = stripLists (n-1) t
    stripLists _ _ = error "getConversion: bug in unifier"
getConversion (CNum (CUnit uSupplied)) (CNum (CUnit uDeclared)) =
    case concord uSupplied uDeclared of
        Just 1 -> IdConversion
        Just f -> MultiplyBy f IdConversion
        Nothing -> error "getConversion: bug in unifier"
getConversion t1 t2
    | t1 == t2 = IdConversion
    | otherwise = error "getConversion: bug in unifier"

applyConversion
    :: ( MonadEnv PType m
       , MonadError TypeError m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Conversion -> (Expr, (CoreExpr, CoreType)) -> m (Int, CoreExpr)
applyConversion (UnliftBy 0 conv) (x, (c, t)) = applyConversion conv (x, (c, t))
applyConversion (UnliftBy n conv) (x, (c, t)) = do
    (n', c') <- applyConversion conv (x, (c, t))
    pure (n+n', c')
applyConversion (MultiplyBy f conv) (x, (c, t)) = do
    -- need to re-run typechecker to make sure we insert unlifts correctly
    -- note that we avoid infinite recursion by doing all calculations with concordant units

    let removeUnits (CList a) = CList $ removeUnits a
        removeUnits (CNum _) = CNum cUno
        removeUnits _ = error "applyConversion: bug in inferStep"
        t' = removeUnits t

    let xNum = Fix (XAtom (Lit (LNum f)))
    timesType <- instantiate $ Forall ["u", "v"] $ CFun [CNum $ CUnit $ uVarR "u", CNum $ CUnit $ uVarR "v"] (CNum $ CUnit (1, Map.fromList [(Right (Rigid "u"), 1), (Right (Rigid "v"), 1)]))
    c' <- inferStep $ XFunApp (Fix (XAtom (Var "*")), pure (CVar "*", timesType))
        [ (xNum, pure (CLit (LNum f), CNum cUno))
        , (x, pure (c, t'))
        ]
    applyConversion conv (Fix $ XNamedFunApp "*" [xNum, x], c')
applyConversion IdConversion (_, (c, _)) = pure (0, c)

getConvertedExpr
    :: ( MonadEnv PType m
       , MonadError TypeError m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Subst -> (Expr, (CoreExpr, CoreType)) -> CoreType -> m ((Int, CoreExpr), Conversion)
getConvertedExpr s (x, (suppliedExpr, suppliedT)) declaredT =
    let suppliedT' = apply' s suppliedT
        declaredT' = apply' s declaredT
        conv = getConversion suppliedT' declaredT'
    in (,conv) <$> applyConversion conv (x, (suppliedExpr, suppliedT'))

liftBy :: Int -> CoreType -> CoreType
liftBy 0 t = t
liftBy n t = CList $ liftBy (n-1) t

getConvertedArgs
    :: ( MonadEnv PType m
       , MonadError TypeError m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Subst -> ([(Expr, (CoreExpr, CoreType))], CoreType) -> CoreType -> m ([(Int, CoreExpr)], CoreType)
getConvertedArgs s (supplied, ret) declaredT = do
    let declaredTs = getCFunArgs declaredT
    (convertedArgs, convs) <- unzip <$>
        traverse2 (getConvertedExpr s) supplied declaredTs
    let ret' = liftBy (getMaxLift convs) $ apply' s ret
    pure (convertedArgs, ret')
  where
    getCFunArgs (CFun args _) = args
    getCFunArgs _ = error "getConvertedArgs: bug in unifier"

    getMaxLift = foldr (\case {UnliftBy n _ -> max n ; _ -> id}) 0

    traverse2 :: Applicative f => (a -> b -> f c) -> [a] -> [b] -> f [c]
    traverse2 f (a:as) (b:bs) = (:) <$> f a b <*> traverse2 f as bs
    traverse2 _ [] [] = pure []
    traverse2 _ _ _ = error "getConvertedArgs: bug in unifier - lengths of argument lists do not match"


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
       , MonadError TypeError m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => ExprF (Expr, m (CoreExpr, CoreType))
    -> m (CoreExpr, CoreType)
inferStep = \case
    XAtom Null -> (CNull,) . CTVar <$> fresh
    XAtom (Lit n@(LNum _)) -> pure (CLit n, CNum cUno)
    XAtom (Lit n@(LBool _)) -> pure (CLit n, CBool)
    XAtom (Lit n@(LText _)) -> pure (CLit n, CText)
    XAtom (Var v) -> do
        t <- instantiate =<< lookupName v
        pure (CVar v, t)
    XRecord Nonrecursive r' _ -> do
        r <- traverse snd r'
        pure (CRec Nonrecursive $ Map.toList $ fst <$> r, CRecord $ snd <$> r)
    XRecord Recursive r' order -> do
        tsDeclared <- traverse ((fmap.fmap) CTVar $ const fresh) r'

        scope $ do
            _ <- Map.traverseWithKey (curry extend) $ Forall [] <$> tsDeclared

            let r'WithTvs :: Map.Map String ((Expr, m (CoreExpr, CoreType)), CoreType)
                r'WithTvs = Map.merge
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XRecord Recursive case")
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XRecord Recursive case")
                    (Map.zipWithMatched $ const (,))
                    r' tsDeclared

                r'WithTvsOrdered :: [(String, ((Expr, m (CoreExpr, CoreType)), CoreType))]
                r'WithTvsOrdered = (\k -> (k, r'WithTvs Map.! k)) <$> order

            r <- for r'WithTvsOrdered $ \(name, ((x, r'Elem), tDeclared)) -> do
                rElem@(_, tSupplied) <- r'Elem
                let e = TypeMismatch (RecordField name) $
                        Mismatch { tSupplied, tDeclared }
                s <- whenJustElse e $ unify (Unify tSupplied tDeclared)
                xConverted <- snd . fst <$> getConvertedExpr s (x, rElem) tDeclared
                applyToEnv s
                let t = apply' s tDeclared
                pure (name, (xConverted, t))

            pure (CRec Recursive (second fst <$> r), CRecord (snd <$> Map.fromList r))
    XFunApp (Fix (XAtom (Var "List")), _) xs' -> do
        xs <- traverseExprs xs'
        tv <- CTVar <$> fresh
        let cs = Unify tv . snd . snd <$> xs
            e = TypeMismatch ListElement . toMismatch
        s <- whenRightElse e $ solve cs

        elemsConverted <- traverse (fmap fst . flip (getConvertedExpr s) tv) xs

        let t = apply' s tv
        applyToEnv s

        pure (CApp (CVar "List") elemsConverted, CList t)
    XFunApp (Fix (XAtom (Var "Table")), _) r' -> traverse snd r' >>= \case
        -- for 'Table', need to know AT POINT OF TYPECHECKING what the fields are
        -- (unless we get fancy type-level extensible row types!)
        [(x, CRecord r)] -> do
            ts <- flip Map.traverseWithKey r $ \k tSupplied -> do
                tvar <- CTVar <$> fresh
                let tDeclared = CList tvar
                    e = TypeMismatch (TableColumnNotList k) $
                        Mismatch { tSupplied, tDeclared }
                s <- whenJustElse e $ unify $ Unify tSupplied tDeclared
                applyToEnv s
                pure $ apply' s tvar
            pure (CTab x, CTable ts)
        [(_, t)] -> throwError $ TableColumnsUnknown t
        _ -> throwError $ WrongNumberOfArguments "Table"
    XFunApp (Fix (XAtom (Var "Lambda")), _) xs -> do
        let unVar (Fix (XAtom (Var v))) = pure v
            unVar _ = throwError LambdaArgumentNotVariable
            (_, x) = last xs
        args <- traverse (unVar . fst) $ init xs
        tvs <- for args $ \arg -> (arg,) <$> fresh
        ((retc, rett), argts) <- scope $ do
            for_ tvs $ extend . second (Forall [] . CTVar)
            ret <- x
            argts <- traverse (instantiate <=< lookupName) args
            pure (ret, argts)
        pure (CLam args retc, CFun argts rett)
    XFunApp (Fix (XAtom (Var "GetField")), _) args -> case args of
        [(_, x), (Fix (XAtom (Var f)), _)] -> do
            (r, rt) <- x
            tv <- fresh
            -- unify with minimal record which could work
            let c = Subtype rt (CRecord $ Map.singleton f $ CTVar tv)
            s <- whenJustElse (RecordFieldAbsent f) $ unify c
            -- then back-substitute
            let t = apply' s (CTVar tv)
            applyToEnv s
            pure (CApp (CVar "GetField") [(0,r), (0,CLit (LText f))], t)
        _ -> throwError $ WrongNumberOfArguments "GetField"
    XFunApp (_, f) args' -> do
        (c, tfun) <- f
        args <- traverseExprs args'
        tv <- fresh
        let con = Subtype tfun (CFun (snd . snd <$> args) (CTVar tv))
            e = TypeMismatch (ArgumentOfFunction c) $ toMismatch con
        s <- whenJustElse e $ unify con

        (argsConverted, ret) <- getConvertedArgs s (args, CTVar tv) tfun
        applyToEnv s
        
        pure (CApp c argsConverted, ret)
    XUnitApp (_, x') u -> do
        (x, t) <- x'
        let e = TypeMismatch UnitAp $
                Mismatch { tSupplied = t, tDeclared = CNum cUno }
        _ <- whenJustElse e $ unify (Subtype t (CNum cUno))

        u' <- whenRightElse id $ simplify u

        case getConversion t (CNum cUno) of
            UnliftBy n _ -> pure (x, liftBy n $ CNum $ CUnit u')
            _            -> pure (x, CNum $ CUnit u')
    XTypeApp (orig, x') ty -> do
        t' <- whenRightElse id $ instantiateRigid ty
        (x, t) <- x'
        let e = TypeMismatch TypeSpecification $
                Mismatch { tSupplied = t, tDeclared = t' }
        s <- whenJustElse e $ unify (Unify t t')

        ((_0, xConverted), _) <- getConvertedExpr s (orig, (x, t)) t'
        applyToEnv s
        pure (xConverted, t')

typecheck
    :: ( MonadEnv PType f
       , MonadError TypeError f
       , MonadScoped e f
       , MonadSubst f
       ) => Expr -> f (CoreExpr, PType)
typecheck = flip evalStateT (0::Int) . fmap (second generalise) . para inferStep
