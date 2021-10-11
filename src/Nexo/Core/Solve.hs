{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Nexo.Core.Solve where

import Prelude hiding (fail)
import Control.Monad (join)
import Control.Monad.Fail (MonadFail(..))
import Data.Traversable (for)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map

import Nexo.Core.Substitute
import Nexo.Expr.Type
import Nexo.Expr.Unit

data Constraint = Unify Type Type | Subtype Type Type
  deriving (Show)

instance Substitutable Constraint where
    apply s (Unify t1 t2)   = Unify   <$> apply s t1 <*> apply s t2
    apply s (Subtype t1 t2) = Subtype <$> apply s t1 <*> apply s t2

    frees (Unify t1 t2)   = frees t1 <> frees t2
    frees (Subtype t1 t2) = frees t1 <> frees t2
    
whenJustElse :: MonadFail m => String -> Maybe a -> m a
whenJustElse s = maybe (fail s) pure

traverse2 :: MonadFail f => String -> (a -> b -> f c) -> [a] -> [b] -> f [c]
traverse2 e f (a:as) (b:bs) = (:) <$> f a b <*> traverse2 e f as bs
traverse2 _ _ [] [] = pure []
traverse2 e _ _ _ = fail e
    
{- TYPING RULES

Definitions:
  T ⫇ U (‘T is a subtype of U’)  iff   T can be used everywhere U can
    (alternately, iff T/U can be unified by applying a substitution to
     T and U followed by applying a conversion to T)
  T ∼ U (‘T is equivalent to U’)   iff   T ⫇ U ∧ U ⫇ T
    (note that this is not the same as T=U !)

Rules:

   T ⫇ U          Uᵢ ⫇ Tᵢ   Tᵣ ⫇ Uᵣ         {k:v} ⊆ {j:u}
-----------   -------------------------   -----------------
List(T) ⫇ U   ({Tᵢ}) → Tᵣ ⫇ ({Uᵢ}) → Uᵣ   ({k:v}) ⫇ ({j:u})

      T ⫇ U                  u concords v
-----------------   -----   ---------------
List(T) ⫇ List(U)   T ∼ T   Num<u> ∼ Num<v>

Note that in the implementation, some derived rules for ∼ are
implemented directly, because it’s easier than using the definition.
-}
    
unify :: MonadFail m => Constraint -> m Subst
unify (Subtype (TList t1) (TList t2)) = unify (Subtype t1 t2)
unify (Subtype (TList t1) t2) = unify (Subtype t1 t2)
unify (Subtype (TFun ts1 r1) (TFun ts2 r2)) =
    let cs = Unify r1 r2 : zipWith Subtype ts2 ts1
    in solve cs
unify (Subtype (TRecord r1) (TRecord r2)) = do
    let merged = Map.merge
             Map.dropMissing                          -- OK if first record has fields not in second
            (Map.mapMissing $ \_ _ -> fail "#UNIFY")  -- but fail if expected fields are missing
            -- try to unify if they do
            (Map.zipWithMatched $ \_ x y -> pure $ Subtype x y)
            r1 r2
    cs <- sequenceA $ Map.elems merged
    solve cs
unify (Subtype t1 t2) = unify (Unify t1 t2)

unify (Unify (TNum u) (TNum v)) = unifyU u v
unify (Unify TBool TBool) = pure nullSubst
unify (Unify TText TText) = pure nullSubst
unify (Unify (TVar v) r) = bind v (Left r)
unify (Unify r (TVar v)) = bind v (Left r)
unify (Unify (TFun ts1 r1) (TFun ts2 r2)) =
    let cs = Unify r1 r2 : zipWith Unify ts1 ts2
    in solve cs
unify (Unify (TList t1) (TList t2)) = unify $ Unify t1 t2
unify (Unify (TRecord r1) (TRecord r2)) = do
    let merged = Map.merge
            (Map.mapMissing $ \_ _ -> fail "#UNIFY")
            (Map.mapMissing $ \_ _ -> fail "#UNIFY")
            (Map.zipWithMatched $ \_ x y -> pure $ Subtype x y)
            r1 r2
    cs <- sequenceA $ Map.elems merged
    solve cs
unify _ = fail "#UNIFY"

unifyU :: MonadFail m => UnitDef -> UnitDef -> m Subst
unifyU ud vd = join $ whenJustElse "#UNIFY" $ go <$> simplify ud <*> simplify vd
  where
    go :: MonadFail m => Unit -> Unit -> m Subst
    go (f, u) (g, v)
        | (units, uvars) <- splitEither u
        , [(Right t, x)] <- Map.toList uvars
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
                    _ -> fail "#FRACT"
            bind t (Right $ unitToDef ((g/f)**(1/fromIntegral x), v''))

        | (vnits, vvars) <- splitEither v
        , [(Right t, x)] <- Map.toList vvars
        = do -- same as last case, but in reverse
            let u' = Map.merge
                    Map.preserveMissing              -- preserve LHS exponents
                    (Map.mapMissing $ const negate)  -- negate RHS exponents
                    (Map.zipWithMatched $ const $ \lhs rhs -> rhs-lhs)
                    u vnits
            u'' <- for u' $ \n ->
                case properFraction @Double @Int (fromIntegral n / fromIntegral x) of
                    (n', 0) -> pure n'
                    _ -> fail "#FRACT"
            bind t (Right $ unitToDef ((f/g)**(1/fromIntegral x), u''))

        | u == v = pure nullSubst
        | otherwise = fail "#UNIFY"

    splitEither :: Map.Map (Either a b) c -> (Map.Map (Either a b) c, Map.Map (Either a b) c)
    splitEither = Map.mapEitherWithKey $ \case
        Left _ -> Left
        Right _ -> Right

solve :: MonadFail m => [Constraint] -> m Subst
solve [] = pure nullSubst
solve (c:cs) = do
    s1 <- unify c
    cs' <- whenJustElse "#UNIFY" $ traverse (apply s1) cs
    s2 <- solve cs'
    whenJustElse "#UNIFY" $ s2 `compose` s1
