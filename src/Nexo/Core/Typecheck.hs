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
import qualified Nexo.Expr.Type as Expr
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
getConversion (TNum (TUnit uSupplied)) (TNum (TUnit uDeclared)) =
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
    => Conversion -> (Expr.Expr, (Expr, Type)) -> m (Int, Expr)
applyConversion (UnliftBy 0 conv) (x, (c, t)) = applyConversion conv (x, (c, t))
applyConversion (UnliftBy n conv) (x, (c, t)) = do
    (n', c') <- applyConversion conv (x, (c, t))
    pure (n+n', c')
applyConversion (MultiplyBy f conv) (x, (c, t)) = do
    -- need to re-run typechecker to make sure we insert unlifts correctly
    -- note that we avoid infinite recursion by doing all calculations with concordant units

    let removeUnits (TList a) = TList $ removeUnits a
        removeUnits (TNum _) = TNum tUno
        removeUnits _ = error "applyConversion: bug in inferStep"
        t' = removeUnits t

    let xNum = Fix (Expr.Atom (Expr.Lit (Expr.Num f)))
    timesType <- instantiate $ Forall ["u", "v"] $ TFun [TNum $ TUnit $ uVarR "u", TNum $ TUnit $ uVarR "v"] (TNum $ TUnit (1, Map.fromList [(Right (Rigid "u"), 1), (Right (Rigid "v"), 1)]))
    c' <- inferStep $ Expr.FunApp (Fix (Expr.Atom (Expr.Var "*")), pure (Var "*", timesType))
        [ (xNum, pure (Lit (Expr.Num f), TNum tUno))
        , (x, pure (c, t'))
        ]
    applyConversion conv (Fix $ Expr.NamedFunApp "*" [xNum, x], c')
applyConversion IdConversion (_, (c, _)) = pure (0, c)

getConvertedExpr
    :: ( MonadEnv PType m
       , MonadError TypeError m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Subst -> (Expr.Expr, (Expr, Type)) -> Type -> m ((Int, Expr), Conversion)
getConvertedExpr s (x, (suppliedExpr, suppliedT)) declaredT =
    let suppliedT' = apply' s suppliedT
        declaredT' = apply' s declaredT
        conv = getConversion suppliedT' declaredT'
    in (,conv) <$> applyConversion conv (x, (suppliedExpr, suppliedT'))

liftBy :: Int -> Type -> Type
liftBy 0 t = t
liftBy n t = TList $ liftBy (n-1) t

getConvertedArgs
    :: ( MonadEnv PType m
       , MonadError TypeError m
       , MonadFresh m
       , MonadScoped e m
       , MonadSubst m
       )
    => Subst -> ([(Expr.Expr, (Expr, Type))], Type) -> Type -> m ([(Int, Expr)], Type)
getConvertedArgs s (supplied, ret) declaredT = do
    let declaredTs = getTFunArgs declaredT
    (convertedArgs, convs) <- unzip <$>
        traverse2 (getConvertedExpr s) supplied declaredTs
    let ret' = liftBy (getMaxLift convs) $ apply' s ret
    pure (convertedArgs, ret')
  where
    getTFunArgs (TFun args _) = args
    getTFunArgs _ = error "getConvertedArgs: bug in unifier"

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
    => Expr.ExprF (Expr.Expr, m (Expr, Type))
    -> m (Expr, Type)
inferStep = \case
    Expr.Atom Expr.Null -> (Null,) . TVar <$> fresh
    Expr.Atom (Expr.Lit n@(Expr.Num _)) -> pure (Lit n, TNum tUno)
    Expr.Atom (Expr.Lit n@(Expr.Bool _)) -> pure (Lit n, TBool)
    Expr.Atom (Expr.Lit n@(Expr.Text _)) -> pure (Lit n, TText)
    Expr.Atom (Expr.Var v) -> do
        t <- instantiate =<< lookupName v
        pure (Var v, t)
    Expr.Record Expr.Nonrecursive r' _ -> do
        r <- traverse snd r'
        pure (Rec Expr.Nonrecursive $ Map.toList $ fst <$> r, TRecord $ snd <$> r)
    Expr.Record Expr.Recursive r' order -> do
        tsDeclared <- traverse ((fmap.fmap) TVar $ const fresh) r'

        scope $ do
            _ <- Map.traverseWithKey (curry extend) $ Forall [] <$> tsDeclared

            let r'WithTvs :: Map.Map String ((Expr.Expr, m (Expr, Type)), Type)
                r'WithTvs = Map.merge
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XRecord Recursive case")
                    (Map.mapMissing $ \_ _ -> error "inferStep: bug in XRecord Recursive case")
                    (Map.zipWithMatched $ const (,))
                    r' tsDeclared

                r'WithTvsOrdered :: [(String, ((Expr.Expr, m (Expr, Type)), Type))]
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

            pure (Rec Expr.Recursive (second fst <$> r), TRecord (snd <$> Map.fromList r))
    Expr.FunApp (Fix (Expr.Atom (Expr.Var "List")), _) xs' -> do
        xs <- traverseExprs xs'
        tv <- TVar <$> fresh
        let cs = Unify tv . snd . snd <$> xs
            e = TypeMismatch ListElement . toMismatch
        s <- whenRightElse e $ solve cs

        elemsConverted <- traverse (fmap fst . flip (getConvertedExpr s) tv) xs

        let t = apply' s tv
        applyToEnv s

        pure (App (Var "List") elemsConverted, TList t)
    Expr.FunApp (Fix (Expr.Atom (Expr.Var "Table")), _) r' -> traverse snd r' >>= \case
        -- for 'Table', need to know AT POINT OF TYPECHECKING what the fields are
        -- (unless we get fancy type-level extensible row types!)
        [(x, TRecord r)] -> do
            ts <- flip Map.traverseWithKey r $ \k tSupplied -> do
                tvar <- TVar <$> fresh
                let tDeclared = TList tvar
                    e = TypeMismatch (TableColumnNotList k) $
                        Mismatch { tSupplied, tDeclared }
                s <- whenJustElse e $ unify $ Unify tSupplied tDeclared
                applyToEnv s
                pure $ apply' s tvar
            pure (Tab x, TTable ts)
        [(_, t)] -> throwError $ TableColumnsUnknown t
        _ -> throwError $ WrongNumberOfArguments "Table"
    Expr.FunApp (Fix (Expr.Atom (Expr.Var "Lambda")), _) xs -> do
        let unVar (Fix (Expr.Atom (Expr.Var v))) = pure v
            unVar _ = throwError LambdaArgumentNotVariable
            (_, x) = last xs
        args <- traverse (unVar . fst) $ init xs
        tvs <- for args $ \arg -> (arg,) <$> fresh
        ((retc, rett), argts) <- scope $ do
            for_ tvs $ extend . second (Forall [] . TVar)
            ret <- x
            argts <- traverse (instantiate <=< lookupName) args
            pure (ret, argts)
        pure (Lam args retc, TFun argts rett)
    Expr.FunApp (Fix (Expr.Atom (Expr.Var "GetField")), _) args -> case args of
        [(_, x), (Fix (Expr.Atom (Expr.Var f)), _)] -> do
            (r, rt) <- x
            tv <- fresh
            -- unify with minimal record which could work
            let c = Subtype rt (TRecord $ Map.singleton f $ TVar tv)
            s <- whenJustElse (RecordFieldAbsent f) $ unify c
            -- then back-substitute
            let t = apply' s (TVar tv)
            applyToEnv s
            pure (App (Var "GetField") [(0,r), (0,Lit (Expr.Text f))], t)
        _ -> throwError $ WrongNumberOfArguments "GetField"
    Expr.FunApp (_, f) args' -> do
        (c, tfun) <- f
        args <- traverseExprs args'
        tv <- fresh
        let con = Subtype tfun (TFun (snd . snd <$> args) (TVar tv))
            e = TypeMismatch (ArgumentOfFunction c) $ toMismatch con
        s <- whenJustElse e $ unify con

        (argsConverted, ret) <- getConvertedArgs s (args, TVar tv) tfun
        applyToEnv s
        
        pure (App c argsConverted, ret)
    Expr.UnitApp (_, x') u -> do
        (x, t) <- x'
        let e = TypeMismatch UnitAp $
                Mismatch { tSupplied = t, tDeclared = TNum tUno }
        _ <- whenJustElse e $ unify (Subtype t (TNum tUno))

        u' <- whenRightElse id $ simplify u

        case getConversion t (TNum tUno) of
            UnliftBy n _ -> pure (x, liftBy n $ TNum $ TUnit u')
            _            -> pure (x, TNum $ TUnit u')
    Expr.TypeApp (orig, x') ty -> do
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
       ) => Expr.Expr -> f (Expr, PType)
typecheck = flip evalStateT (0::Int) . fmap (second generalise) . para inferStep
