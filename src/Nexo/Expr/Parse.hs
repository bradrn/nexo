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
import Text.Megaparsec ( choice, oneOf, many, Parsec, parseMaybe, between, sepBy, try, manyTill, (<|>), empty, optional, eof, sepBy1, getSourcePos )
import Text.Megaparsec.Char ( alphaNumChar, space1, letterChar, char )

import qualified Data.Map.Strict as Map
import qualified Text.Megaparsec.Char.Lexer as L

import Nexo.Core.Substitute (generalise)
import Nexo.Expr.Type
import Nexo.Expr.Type.Annotated

type Parser = Parsec Void String

annotateLoc' :: (SourceSpan -> a -> b) -> Parser a -> Parser b
annotateLoc' ann p = do
    start <- getSourcePos
    r <- p
    end <- getSourcePos
    pure $ ann SourceSpan{spanStart=start, spanEnd=end} r

annotateLoc :: Parser (ExprF ExprLoc) -> Parser ExprLoc
annotateLoc = annotateLoc' $ (Fix .) . ExprLocF

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
        (pUnit <|> UVar . Rigid <$> (char '\'' *> pIdentifier))
    <|> pure Uno

pType :: Parser Type
pType = do
    t1 <- TNum <$> (symbol "Num" *> pUnitType)
        <|> TBool <$ symbol "Bool"
        <|> TText <$ symbol "Text"
        <|> try pMultiArgFun
        <|> TRecord <$> paren (pRecordSpec pType)
        <|> TTable <$> paren (pRecordSpec pType)
        <|> TList <$> (symbol "List" *> pType)
        <|> TVar . Rigid <$> (char '\'' *> pIdentifier)
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

pLet :: Parser ExprLoc
pLet = annotateLoc $ symbol "Let" *> paren do
    v <- pIdentifier
    vt <- optional $ symbol ":" *> pPType
    _ <- symbol "="
    vx <- pExprInner
    _ <- symbol ","
    x <- pExprInner
    pure $ XLet v vt vx x
    

pLam :: Parser ExprLoc
pLam = annotateLoc $ XLam <$> args <* symbol "->" <*> pTerm
  where
    args = paren (pIdentifier `sepBy` symbol ",")
        <|> (pure <$> pIdentifier)
    
operatorTable :: [[Operator Parser ExprLoc]]
operatorTable =
    [ [ binary "*" $ XOp OTimes
      , binary "/" $ XOp ODiv
      ]
    , [ binary "+" $ XOp OPlus
      , binary "-" $ XOp OMinus
      ]
    , [ binary "=" $ XOp OEq
      , binary "<>" $ XOp ONeq
      , binary ">" $ XOp OGt
      , binary "<" $ XOp OLt
      ]
    , [ binary "&&" $ XOp OAnd
      , binary "||" $ XOp OOr
      ]
    ]
  where
    binary :: String -> (ExprLoc -> ExprLoc -> ExprF ExprLoc) -> Operator Parser ExprLoc
    binary name f = InfixL (annotateLoc' wrap $ f <$ symbol name)

    wrap :: SourceSpan -> (ExprLoc -> ExprLoc -> ExprF ExprLoc) -> ExprLoc -> ExprLoc -> ExprLoc
    wrap s f x1 x2 = Fix $ ExprLocF s $ f x1 x2

pTerm :: Parser ExprLoc
pTerm = wrap $ choice
    [ try $ annotateLoc $ uncurry3 XRecord <$> pRecursivity <*> paren (pOrderedRecordSpec pTerm)
    , try $ annotateLoc $ XTable <$> (symbol "Table" *> paren pExprInner)
    , pLet
    , try $ annotateLoc $ XFun <$> pIdentifier <*> paren (pExprInner `sepBy` symbol ",")
    , try pLam
    , paren pExprInner
    , annotateLoc $ XLit <$> pLit
    , annotateLoc $ XNull <$ symbol "Null"
    , annotateLoc $ XVar <$> pIdentifier
    , annotateLoc $ XList <$> sqparen (pExprInner `sepBy` symbol ",")
    ]
  where
    uncurry3 :: (a -> b -> c -> x) -> (a -> (b,c) -> x)
    uncurry3 f = \a (b,c) -> f a b c

    wrap :: Parser ExprLoc -> Parser ExprLoc
    wrap p = do
        r <- p
        annotateLoc (XField r <$> (symbol "." *> pIdentifier))
            <|> annotateLoc (XTApp r <$> (symbol ":" *> pPType))
            <|> annotateLoc (XUnit r <$> pUnit)
            <|> pure r

pExprInner :: Parser ExprLoc
pExprInner = makeExprParser pTerm operatorTable

pExpr :: Parser ExprLoc
pExpr = annotateLoc (XNull <$ eof) <|> pExprInner
