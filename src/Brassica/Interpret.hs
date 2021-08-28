{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Brassica.Interpret
       ( Type
       , Expr
       , Sheet(..)
       , Cell(..)
       , ValueState(..)
       , display
       , evalSheet
       , insert
       , parseMaybe
       , pType
       , pExpr
       ) where

import Control.Monad.Combinators.Expr
import Control.Monad.State.Strict
    ( execState, gets, modify', put, state, runState, State, StateT (..), MonadState )
import Data.Containers.ListUtils (nubOrd)
import Data.Fix (Fix(..))
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Void ( Void )
import Data.Deriving (deriveShow1)
import Text.Megaparsec ( choice, oneOf, many, Parsec, parseMaybe, between, sepBy, try, manyTill, (<|>), empty )
import Text.Megaparsec.Char ( alphaNumChar, space1, letterChar, char )

import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Trans (lift)
import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad (zipWithM)
import Data.List (transpose, sort)

----------------------- TYPES ----------------------- 

-- | Data type listing all the types in Supercell
data Type
    = TNum
    | TBool
    | TText
    | TVar TVar
    -- | TFun FunType
    | TList Type
    | TRecord (Map.Map String Type)
    deriving (Show, Eq, Ord)

type TVar = String
data FunType = FunType [Type] Type
    deriving (Show, Eq, Ord)

meet :: Type -> Type -> Maybe Type
meet t u | t == u = Just t
meet (TList t) u = meet t u
meet t (TList u) = meet t u
meet _ _ = Nothing

meets :: [Type] -> Maybe Type
meets = foldr (\val acc -> acc >>= meet val) =<< listToMaybe

data Value
    = VNum Double
    | VBool Bool
    | VText String
    | VList [Value]
    | VRecord (Map.Map String Value)
    deriving (Show, Eq)

-- | Operators
data Op
    = OPlus     -- ^ Plus
    | OMinus    -- ^ Minus
    | OTimes    -- ^ Times
    | ODiv      -- ^ Division
    | OEq       -- ^ Equals
    | ONeq      -- ^ Not Equals
    | OGt       -- ^ Greater Than
    | OLt       -- ^ Less Than
    | OAnd      -- ^ Logical And
    | OOr       -- ^ Logical Or
    deriving (Show)

data ExprF r
    = XLit Value
    | XList [r]
    | XRecord (Map.Map String r)
    | XVar String
    | XField r String
    | XFun String [r]
    | XOp Op r r
    deriving (Show, Functor)
deriveShow1 ''ExprF

type Expr = Fix ExprF

data ValueState
    = ValuePresent Type Value
    | ValueError String
    | Invalidated
    deriving (Show)

display :: ValueState -> String
display (ValuePresent _ v) = show v
display (ValueError e) = '#' : e
display Invalidated = "#INVALIDATED"

fromEither :: Type -> Either String Value -> ValueState
fromEither t (Right val) = ValuePresent t val
fromEither _ (Left err) = ValueError err

toEither :: ValueState -> Either String (Type, Value)
toEither (ValuePresent t val) = Right (t, val)
toEither (ValueError err) = Left err
toEither Invalidated = Left "#INVALIDATED"

data Cell = Cell
    { cellName :: String
    , cellType :: Maybe Type
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

----------------------- PARSER ----------------------- 

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

paren, sqparen :: Parser a -> Parser a
paren   = between (symbol "(") (symbol ")")
sqparen = between (symbol "[") (symbol "]")

pIdentifier :: Parser String
pIdentifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> oneOf "_")

pRecordSpec :: Parser a -> Parser (Map.Map String a)
pRecordSpec p = fmap Map.fromList $ ((,) <$> pIdentifier <* symbol ":" <*> p) `sepBy` symbol ","

pType :: Parser Type
pType = TNum <$ symbol "Num"
    <|> TBool <$ symbol "Bool"
    <|> TText <$ symbol "Text"
    <|> TRecord <$> paren (pRecordSpec pType)
    <|> TList <$> (symbol "List" *> pType)

pValue :: Parser Value
pValue = VNum <$> lexeme L.float
    <|> VBool <$> pBool
    <|> VText <$> lexeme pString
  where
    pBool = True <$ symbol "True" <|> False <$ symbol "False"

    pString :: Parser String
    pString = char '"' *> (L.charLiteral `manyTill` char '"')

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ binary "*" $ wrap $ XOp OTimes
      , binary "/" $ wrap $ XOp ODiv
      ]
    , [ binary "+" $ wrap $ XOp OPlus
      , binary "-" $ wrap $ XOp OMinus
      ]
    , [ binary "=" $ wrap $ XOp OEq
      , binary "<>" $ wrap $ XOp OEq
      , binary ">" $ wrap $ XOp OGt
      , binary "<" $ wrap $ XOp OLt
      ]
    , [ binary "&&" $ wrap $ XOp OAnd
      , binary "||" $ wrap $ XOp OOr
      ]
    ]
  where
    binary name f = InfixL (f <$ symbol name)

    wrap :: (Expr -> Expr -> ExprF Expr) -> Expr -> Expr -> Expr
    wrap e x y = Fix $ e x y

pTerm :: Parser Expr
pTerm = wrapDot $ choice
    [ try $ Fix . XRecord <$> paren (pRecordSpec pTerm)
    , try $ (Fix .) . XFun <$> pIdentifier <*> paren (pExpr `sepBy` symbol ",")
    , paren pExpr
    , Fix . XVar <$> pIdentifier
    , Fix . XList <$> sqparen (pExpr `sepBy` symbol ",")
    , Fix . XLit <$> pValue
    ]
  where
    wrapDot :: Parser Expr -> Parser Expr
    wrapDot p = do
        r <- p
        (Fix . XField r <$> (symbol "." *> pIdentifier)) <|> pure r

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

----------------------- TYPECHECKER ----------------------- 

data CoreExpr
    = CLit Value
    | CVar String
    | CRec (Map.Map String CoreExpr)
    | CApp (Either Op String) [(Int, CoreExpr)]
    deriving (Show)

makeBaseFunctor ''CoreExpr

typecheck :: MonadFail f => (String -> f Type) -> Expr -> f (CoreExpr, Type)
typecheck lookupName = cata \case
    XLit v -> pure $ (CLit v,) $
        case v of
            VNum _ -> TNum
            VBool _ -> TBool
            VText _ -> TText
            VRecord _ -> error "typecheck: bug in parser"
            VList   _ -> error "typecheck: bug in parser"
    XList vs -> do
        vs' <- sequenceA vs
        let xs = fst <$> vs'
            ts = snd <$> vs'
        case nubOrd ts of
            [t] -> pure (CApp (Right "List") $ (0,) <$> xs, TList t)
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
                        [ (0,r)
                        , (0,CLit (VText f))
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
  where
    fntype "If" = pure $ FunType [TBool, TVar "a", TVar "a"] (TVar "a")
    fntype "Mean" = pure $ FunType [TList TNum] TNum
    fntype "Avg"  = pure $ FunType [TList TNum] TNum
    fntype "PopStdDev" = pure $ FunType [TList TNum] TNum
    fntype "Median" = pure $ FunType [TList TNum] TNum
    fntype "Mode" = pure $ FunType [TList TNum] TNum
    fntype "Sin" = pure $ FunType [TNum] TNum
    fntype "Cos" = pure $ FunType [TNum] TNum
    fntype "Tan" = pure $ FunType [TNum] TNum
    fntype "InvSin" = pure $ FunType [TNum] TNum
    fntype "InvCos" = pure $ FunType [TNum] TNum
    fntype "InvTan" = pure $ FunType [TNum] TNum
    fntype "Root" = pure $ FunType [TNum, TNum] TNum
    fntype "Power" = pure $ FunType [TNum, TNum] TNum
    fntype _ = fail "#NAME"

    optype OEq    = pure $ FunType [TVar "a", TVar "a"] (TVar "a")
    optype ONeq   = pure $ FunType [TVar "a", TVar "a"] (TVar "a")
    optype OPlus  = pure $ FunType [TNum, TNum] TNum
    optype OMinus = pure $ FunType [TNum, TNum] TNum
    optype OTimes = pure $ FunType [TNum, TNum] TNum
    optype ODiv   = pure $ FunType [TNum, TNum] TNum
    optype OGt    = pure $ FunType [TNum, TNum] TBool
    optype OLt    = pure $ FunType [TNum, TNum] TBool
    optype OAnd   = pure $ FunType [TBool, TBool] TBool
    optype OOr    = pure $ FunType [TBool, TBool] TBool

    unify :: MonadFail f => [Type] -> FunType -> f ([Int], Type)
    unify ts (FunType args out) = do
        targs <- zip' ts args
        let substs = getSubsts targs
        case traverse meets substs of
            Nothing -> fail "#TYPE"
            Just substs' -> do
                let args' = subst substs' args
                levels <- zipWithM match ts args'
                let maxlevel = if null levels then 0 else maximum levels
                pure (levels, liftBy maxlevel out)

    zip' :: MonadFail f => [a] -> [b] -> f [(a, b)]
    zip' (a:as) (b:bs) = ((a,b) :) <$> zip' as bs
    zip' [] [] = pure []
    zip' _ _ = fail "#TYPE"

    getSubsts :: [(Type, Type)] -> Map.Map String [Type]
    getSubsts = flip execState Map.empty . go
      where
        go :: [(Type, Type)] -> State (Map.Map String [Type]) ()
        go [] = pure ()
        go ((t,TVar v) : ts) =
            modify' (Map.insertWith (++) v [t]) >> go ts
        go ((TList t1, TList t2) : ts) = go ((t1,t2) : ts)
        go (_ : ts) = go ts

    subst :: Map.Map String Type -> [Type] -> [Type]
    subst _ [] = []
    subst ss (TVar v : ts) = case Map.lookup v ss of
        Nothing -> TVar v : subst ss ts
        Just t -> subst ss $ t : ts
    subst ss (t : ts) = t : subst ss ts

    match :: MonadFail f => Type -> Type -> f Int
    match t t' | t == t' = pure 0
    match (TList t) t' = (1+) <$> match t t'
    match _ _ = fail "#TYPE"

    liftBy :: Int -> Type -> Type
    liftBy 0 t = t
    liftBy n t = liftBy (n-1) (TList t)

----------------------- INTERPRETER ----------------------- 

extractNum :: Value -> Double
extractNum (VNum n) = n
extractNum _ = error "unexpected value"

mean :: [Double] -> Double
mean list = sum list / fromIntegral (length list)  -- same as VNum (sum (map extractNum list) / fromIntegral (length list))

popStdDev :: [Double] -> Double
popStdDev list =
    let mu = mean list
        diffSquared = map (square . subtract mu) list
        meanSquared = mean diffSquared
    in sqrt meanSquared
  where
    -- could just do (^2), but that gives a defaulting warning
    square x = x * x

median :: [Double] -> Double
median list = 
    let sortList = sort list
        x = sortList !! (length sortList `quot` 2)
        y = sortList !! ((length sortList `quot` 2) - 1)
    in
        if odd (length sortList)
        then x
        else (x + y) / 2

mode :: Ord a => [a] -> a
mode = getMostFrequent . foldr (\val -> Map.insertWith (+) val 1) Map.empty
  where
    getMostFrequent :: Ord a => Map.Map a Int -> a
    getMostFrequent freqs =
        let highestFreq = maximum $ Map.elems freqs
        in head $ Map.keys $ Map.filter (==highestFreq) freqs

evalApp :: Either Op String -> [Value] -> Value
evalApp (Right "If") [cond, tcase, fcase] = case cond of -- If logical Function
    VBool True -> tcase
    VBool False -> fcase
    _ -> error "evalFun: bug in typechecker"
evalApp (Right "Mean") [VList list] = VNum $ mean (map extractNum list)
evalApp (Right "Avg") [VList list] = VNum $ mean (map extractNum list)
evalApp (Right "PopStdDev") [VList list] = VNum $ popStdDev (map extractNum list)
evalApp (Right "Median") [VList list] = VNum $ median (map extractNum list)
evalApp (Right "Mode") [VList list] = VNum $ mode (map extractNum list)
evalApp (Right "Sin") [VNum n] = VNum $ sin n
evalApp (Right "Cos") [VNum n] = VNum $ cos n
evalApp (Right "Tan") [VNum n] = VNum $ tan n
evalApp (Right "InvSin") [VNum n] = VNum $ asin n
evalApp (Right "InvCos") [VNum n] = VNum $ acos n
evalApp (Right "InvTan") [VNum n] = VNum $ atan n
evalApp (Right "Root") [VNum n1, VNum n2] = VNum $ n1**(1/n2)
evalApp (Right "Power") [VNum n1, VNum n2] = VNum $ n1**n2
evalApp (Right "List") vs = VList vs                    -- List function used by Haskell for making lists
evalApp (Right "GetField") [VRecord r, VText f]
    | Just v <- Map.lookup f r = v
evalApp (Left OPlus ) [VNum i1, VNum i2] = VNum $ i1 + i2
evalApp (Left OMinus) [VNum i1, VNum i2] = VNum $ i1 - i2
evalApp (Left OTimes) [VNum i1, VNum i2] = VNum $ i1 * i2
evalApp (Left ODiv  ) [VNum i1, VNum i2] = VNum $ i1 / i2
evalApp (Left OEq   ) [v1     , v2     ] = VBool $ v1 == v2
evalApp (Left ONeq  ) [v1     , v2     ] = VBool $ v1 /= v2
evalApp (Left OGt   ) [VNum i1, VNum i2] = VBool $ i1 > i2
evalApp (Left OLt   ) [VNum i1, VNum i2] = VBool $ i1 < i2
evalApp (Left OAnd  ) [VBool p, VBool q] = VBool $ p && q
evalApp (Left OOr   ) [VBool p, VBool q] = VBool $ p || q
evalApp _ _ = error "evalApp: bug in typechecker"

broadcast :: ([Value] -> Value) -> [(Int, Value)] -> Value
broadcast fn args
    | all ((0==) . fst) args = fn $ snd <$> args
    | otherwise = VList $ broadcast fn <$> unliftSplit args
  where
    unliftSplit :: [(Int, Value)] -> [[(Int, Value)]]
    unliftSplit args' =
        let levels = fst <$> args'
            maxlevel = if null levels then 0 else maximum levels
            fills = catMaybes $ zipWith
                (\level x -> if level == maxlevel then Just x else Nothing)
                levels args'
            placeholders = zipWith
                (\level x -> if level == maxlevel then Nothing else Just x)
                levels args'
        in replaceIn placeholders <$> transposeVLists fills

    transposeVLists :: [(Int, Value)] -> [[(Int, Value)]]
    transposeVLists = transpose . fmap extractVList
      where
        extractVList (i, VList l) = (i-1,) <$> l
        extractVList _ = error "broadcast: bug in typechecker"

    replaceIn :: [Maybe a] -> [a] -> [a]
    replaceIn [] _ = []
    replaceIn (Nothing:is) (r:rs) = r : replaceIn is rs
    replaceIn (Nothing:_) _ = error "broadcast: bug in unlifter"
    replaceIn (Just i:is) rs = i : replaceIn is rs

evalExpr :: MonadFail f => (String -> f Value) -> CoreExpr -> f Value
evalExpr lookupName = cata \case
    CLitF v -> pure v
    CVarF name -> lookupName name
    CRecF xs -> VRecord <$> sequenceA xs
    CAppF fn es -> broadcast (evalApp fn) <$> traverse liftTuple es
  where
    liftTuple :: Functor f => (a, f b) -> f (a, b)
    liftTuple (a, b) = (a,) <$> b

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
            r <- lower $ typecheck (fmap fst . cacheByName) expr
            (v, t) <- case r of
                Left e -> pure (ValueError e, Nothing)
                Right (coreExpr, resultType) -> case type_ of
                    Just type'
                        | resultType /= type' ->
                            -- give type error if types don't match
                            pure (ValueError "#TYPE", type_)
                    _ -> do
                        -- otherwise infer type and return
                        result <- lower $ evalExpr (fmap snd . cacheByName) coreExpr
                        pure $ (,Just resultType) $ fromEither resultType result
            modify' $ Map.insert ident (c { cellType = t, cellValue = v })
            pure v
        -- Else return cached value
        Just Cell{cellValue = v} -> pure v

    cacheByName :: String -> Eval (Type, Value)
    cacheByName name = do
        ident <- gets $
            flip Map.foldrWithKey Nothing $ \k v -> \case
                Nothing -> if name == cellName v then Just k else Nothing
                found   -> found
        case ident of
            Just i -> raise $ toEither <$> cacheExpr i
            Nothing -> fail "#REF"
