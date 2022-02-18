{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Nexo.Core.Solve where

import Control.Monad (join)
import Data.Traversable (for)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import Nexo.Core.Substitute
import Nexo.Expr.Type
import Nexo.Expr.Unit

data Constraint = Unify Type Type | Subtype Type Type
  deriving (Show)

instance Substitutable Constraint where
    apply s (Unify t1 t2)   = Unify   (apply s t1) (apply s t2)
    apply s (Subtype t1 t2) = Subtype (apply s t1) (apply s t2)

    frees (Unify t1 t2)   = frees t1 <> frees t2
    frees (Subtype t1 t2) = frees t1 <> frees t2
    
whenRight :: Either a b -> Maybe b
whenRight = either (const Nothing) Just

{- TYPING RULES

Definitions:
  T ⫇ U (‘T is a subtype of U’)  iff   T can be used everywhere U can
    (alternately, iff T/U can be unified by applying a substitution to
     T and U followed by applying a conversion to T)
  T ∼ U (‘T is equivalent to U’)   iff   T ⫇ U ∧ U ⫇ T
    (note that this is not the same as T=U !)

Rules:

   T ⫇ U          Uᵢ ⫇ Tᵢ   Tᵣ ⫇ Uᵣ         {k:v} ∼ {j:u}      ({k:List(v)}) ⫇ record
-----------   -------------------------   -----------------    --------------------
List(T) ⫇ U   ({Tᵢ}) → Tᵣ ⫇ ({Uᵢ}) → Uᵣ   ({k:v}) ⫇ ({j:u})    Table({k:v}) ⫇ record

      T ⫇ U                  u concords v
-----------------   -----   ---------------
List(T) ⫇ List(U)   T ∼ T   Num<u> ∼ Num<v>

Note that in the implementation, some derived rules for ∼ are
implemented directly, because it’s easier than using the definition.
-}
    
unify :: Constraint -> Maybe Subst
unify (Subtype (TList t1) (TList t2)) = unify (Subtype t1 t2)
unify (Subtype (TList t1) t2) = unify (Subtype t1 t2)
unify (Subtype (TFun ts1 r1) (TFun ts2 r2)) =
    let cs = Unify r1 r2 : zipWith Subtype ts2 ts1
    in whenRight $ solve cs
unify (Subtype (TRecord r1) (TRecord r2)) = do
    let merged = Map.merge
             Map.dropMissing                          -- OK if first record has fields not in second
            (Map.mapMissing $ \_ _ -> Nothing)  -- but throw error if expected fields are missing
            -- try to unify if they do
            (Map.zipWithMatched $ \_ x y -> pure $ Unify x y)
            r1 r2
    cs <- sequenceA $ Map.elems merged
    whenRight $ solve cs
unify (Subtype (TTable t1) r2) = do
    let r1 = TRecord $ TList <$> t1
    unify (Subtype r1 r2)
unify (Subtype t1 t2) = unify (Unify t1 t2)

unify (Unify (TNum u) (TNum v)) = unify (Unify u v)
unify (Unify TBool TBool) = pure nullSubst
unify (Unify TText TText) = pure nullSubst
unify (Unify (TVar (Undetermined v)) r) = bind v (Left r)
unify (Unify r (TVar (Undetermined v))) = bind v (Left r)
unify (Unify (TFun ts1 r1) (TFun ts2 r2))
    | length ts1 == length ts2 =
        let cs = Unify r1 r2 : zipWith Unify ts1 ts2
        in whenRight $ solve cs
    | otherwise = Nothing
unify (Unify (TList t1) (TList t2)) = unify $ Unify t1 t2
unify (Unify (TRecord r1) (TRecord r2)) = do
    let merged = Map.merge
            (Map.mapMissing $ \_ _ -> Nothing)
            (Map.mapMissing $ \_ _ -> Nothing)
            (Map.zipWithMatched $ \_ x y -> pure $ Subtype x y)
            r1 r2
    cs <- sequenceA $ Map.elems merged
    whenRight $ solve cs
unify (Unify (TTable t1) (TTable t2)) = do
    let merged = Map.merge
            (Map.mapMissing $ \_ _ -> Nothing)
            (Map.mapMissing $ \_ _ -> Nothing)
            (Map.zipWithMatched $ \_ x y -> pure $ Subtype x y)
            t1 t2
    cs <- sequenceA $ Map.elems merged
    whenRight $ solve cs
unify (Unify (TUnit u1) (TUnit u2)) = unifyU u1 u2
unify _ = Nothing

unifyU :: UnitDef -> UnitDef -> Maybe Subst
unifyU ud vd = join $ go <$> simplify ud <*> simplify vd
  where
    go :: Unit -> Unit -> Maybe Subst
    go (f, u) (g, v)
        | (units, uvars) <- splitEither u
        , [(Right (Undetermined t), x)] <- Map.toList uvars
        = do -- in this case there is only one type varialble
             -- proceed by moving rest to other side and taking the root
            let v' = Map.merge
                    (Map.mapMissing $ const negate)  -- negate LHS exponents
                    Map.preserveMissing              -- preserve RHS exponents
                    (Map.zipWithMatched $ const $ \lhs rhs -> rhs-lhs)
                    units v
            v'' <- for v' $ \n ->
                case properFraction @Double @Int (fromIntegral n / fromIntegral x) of
                    (n', 0) -> pure n'
                    _ -> Nothing
            bind t (Right $ unitToDef ((g/f)**(1/fromIntegral x), v''))

        | (vnits, vvars) <- splitEither v
        , [(Right (Undetermined t), x)] <- Map.toList vvars
        = do -- same as last case, but in reverse
            let u' = Map.merge
                    Map.preserveMissing              -- preserve LHS exponents
                    (Map.mapMissing $ const negate)  -- negate RHS exponents
                    (Map.zipWithMatched $ const $ \lhs rhs -> rhs-lhs)
                    u vnits
            u'' <- for u' $ \n ->
                case properFraction @Double @Int (fromIntegral n / fromIntegral x) of
                    (n', 0) -> pure n'
                    _ -> Nothing
            bind t (Right $ unitToDef ((f/g)**(1/fromIntegral x), u''))

        | u == v = pure nullSubst
        | otherwise = Nothing

    splitEither :: Map.Map (Either a b) c -> (Map.Map (Either a b) c, Map.Map (Either a b) c)
    splitEither = Map.mapEitherWithKey $ \case
        Left _ -> Left
        Right _ -> Right

solve :: [Constraint] -> Either Constraint Subst
solve [] = pure nullSubst
solve (c:cs) =
    case unify c of
        Nothing -> Left c
        Just s1 -> do
            let cs' = apply s1 <$> cs
            s2 <- solve cs'
            pure $ s2 `compose` s1
