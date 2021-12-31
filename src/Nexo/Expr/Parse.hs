{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Nexo.Expr.Parse
       ( parseMaybe
       , pPType
       , pExpr
       , pLit
       ) where

import Control.Monad.Combinators.Expr
import Data.Fix (Fix(..))
import Data.Void ( Void )
import Text.Megaparsec ( choice, oneOf, many, Parsec, parseMaybe, between, sepBy, try, manyTill, (<|>), empty, optional, eof, sepBy1 )
import Text.Megaparsec.Char ( alphaNumChar, space1, letterChar, char )

import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L

import Nexo.Core.Substitute (generalise)
import Nexo.Expr.Type
import Nexo.Expr.Unit

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
pRecordSpec = fmap fst . pOrderedRecordSpec

pOrderedRecordSpec :: Parser a -> Parser (Map.Map String a, [String])
pOrderedRecordSpec p = fmap fromListWithOrder $
    ((,) <$> pIdentifier <* symbol ":" <*> p) `sepBy` symbol ","
  where
    fromListWithOrder l = (Map.fromList l, fst <$> l)

pRecursivity :: Parser Recursivity
pRecursivity = (Recursive <$ symbol "rec") <|> pure Nonrecursive

pUnit :: Parser UnitDef
pUnit = do
    f <- factored
    UDiv f <$> (symbol "/" *> pUnit)
        <|> UExp f <$> (symbol "^" *> L.signed sc L.decimal)
        <|> UMul f <$> pUnit
        <|> pure f
  where
    factored =
        try (mkPrefix <$> pPrefix <*> pIdentifier)
        <|> UName <$> pIdentifier
        <|> UFactor <$> L.decimal
        <|> paren pUnit

    mkPrefix p u = UMul (UPrefix p) (UName u)

    pPrefix :: Parser String
    pPrefix = choice $ symbol <$>
        ["Y","Z","E","P","T","G","M","k","h","da","d"
        ,"c","m","μ","u","n","p","f","a","z","y"   
        ]

pUnitType :: Parser UnitDef
pUnitType =
    between (symbol "<") (symbol ">")
        (pUnit <|> UVar <$> (char '\'' *> pIdentifier))
    <|> pure Uno

pType :: Parser Type
pType = do
    t1 <- TNum <$> (symbol "Num" *> pUnitType)
        <|> TBool <$ symbol "Bool"
        <|> TText <$ symbol "Text"
        <|> TRecord <$> paren (pRecordSpec pType)
        <|> TTable <$> paren (pRecordSpec pType)
        <|> TList <$> (symbol "List" *> pType)
        <|> TVar <$> (char '\'' *> pIdentifier)
        <|> pMultiArgFun
    optional (symbol "->" *> pType) >>= pure . \case
        Just t2 -> TFun [t1] t2
        Nothing -> t1
  where
    pMultiArgFun = TFun
        <$> paren (pType `sepBy1` symbol ",")
        <* symbol "->"
        <*> pType

pPType :: Parser PType
pPType = generalise <$> pType

pLit :: Parser Literal
pLit = LNum <$> lexeme (L.signed sc $ try L.float <|> L.decimal)
    <|> LBool <$> pBool
    <|> LText <$> lexeme pString
  where
    pBool = True <$ symbol "True" <|> False <$ symbol "False"

    pString :: Parser String
    pString = char '"' *> (L.charLiteral `manyTill` char '"')

pLet :: Parser Expr
pLet = symbol "Let" *> paren do
    v <- pIdentifier
    vt <- optional $ symbol ":" *> pPType
    _ <- symbol "="
    vx <- pExprInner
    _ <- symbol ","
    x <- pExprInner
    pure $ Fix $ XLet v vt vx x
    

pLam :: Parser Expr
pLam = (Fix .) . XLam <$> args <* symbol "->" <*> pTerm
  where
    args = paren (pIdentifier `sepBy` symbol ",")
        <|> (pure <$> pIdentifier)
    
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
pTerm = wrap $ choice
    [ try $ (Fix .) . uncurry3 XRecord <$> pRecursivity <*> paren (pOrderedRecordSpec pTerm)
    , try $ Fix . XTable <$> (symbol "Table" *> paren pExprInner)
    , pLet
    , try $ (Fix .) . XFun <$> pIdentifier <*> paren (pExprInner `sepBy` symbol ",")
    , try pLam
    , paren pExprInner
    , Fix . XLit <$> pLit
    , Fix XNull <$ symbol "Null"
    , Fix . XVar <$> pIdentifier
    , Fix . XList <$> sqparen (pExprInner `sepBy` symbol ",")
    ]
  where
    uncurry3 :: (a -> b -> c -> x) -> (a -> (b,c) -> x)
    uncurry3 f = \a (b,c) -> f a b c

    wrap :: Parser Expr -> Parser Expr
    wrap p = do
        r <- p
        (Fix . XField r <$> (symbol "." *> pIdentifier))
            <|> (Fix . XTApp r <$> (symbol ":" *> pPType))
            <|> (Fix . XUnit r <$> pUnit)
            <|> pure r

pExprInner :: Parser Expr
pExprInner = makeExprParser pTerm operatorTable

pExpr :: Parser Expr
pExpr = (Fix XNull <$ eof) <|> pExprInner
