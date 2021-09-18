module Nexo.Expr.Parse
       ( parseMaybe
       , pType
       , pExpr
       , pValue
       ) where

import Control.Monad.Combinators.Expr
import Data.Fix (Fix(..))
import Data.Void ( Void )
import Text.Megaparsec ( choice, oneOf, many, Parsec, parseMaybe, between, sepBy, try, manyTill, (<|>), empty )
import Text.Megaparsec.Char ( alphaNumChar, space1, letterChar, char )

import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L

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
pRecordSpec p = fmap Map.fromList $ ((,) <$> pIdentifier <* symbol ":" <*> p) `sepBy` symbol ","

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
pUnitType = between (symbol "<") (symbol ">") pUnit <|> pure Uno

pType :: Parser Type
pType = TNum <$> (symbol "Num" *> pUnitType)
    <|> TBool <$ symbol "Bool"
    <|> TText <$ symbol "Text"
    <|> TRecord <$> paren (pRecordSpec pType)
    <|> TList <$> (symbol "List" *> pType)

pValue :: Parser Value
pValue = VNum <$> lexeme (L.signed sc $ try L.float <|> L.decimal)
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
pTerm = wrap $ choice
    [ try $ Fix . XRecord <$> paren (pRecordSpec pTerm)
    , try $ (Fix .) . XFun <$> pIdentifier <*> paren (pExpr `sepBy` symbol ",")
    , paren pExpr
    , Fix . XLit <$> pValue
    , Fix . XVar <$> pIdentifier
    , Fix . XList <$> sqparen (pExpr `sepBy` symbol ",")
    ]
  where
    wrap :: Parser Expr -> Parser Expr
    wrap p = do
        r <- p
        (Fix . XField r <$> (symbol "." *> pIdentifier))
            <|> (Fix . XUnit r <$> pUnit)
            <|> pure r

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable
