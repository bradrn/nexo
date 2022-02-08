{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Nexo.Expr.Parse
       ( parseMaybe
       , pPType
       , pExpr
       , pExprInner
       , pLit
       ) where

import Data.Fix (Fix(..))
import Data.Void ( Void )
import Text.Megaparsec ( choice, oneOf, many, Parsec, parseMaybe, between, sepBy, try, manyTill, (<|>), empty, optional, eof, sepBy1, getSourcePos, SourcePos )
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

annotateLoc :: Parser (ASTF ASTLoc) -> Parser ASTLoc
annotateLoc = annotateLoc' $ (Fix .) . AnnLocF

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

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
    f <- withExp
    UDiv f <$> (symbol "/" *> pUnit)
        <|> UMul f <$> pUnit
        <|> pure f
  where
    withExp = do
        f <- factored
        UExp f <$> (symbol "^" *> lexeme (L.signed sc L.decimal))
            <|> pure f
      
    factored = ULeaf <$> pIdentifier
        <|> UFactor <$> lexeme (try L.float <|> L.decimal)
        <|> UVar . Rigid <$> (char '\'' *> pIdentifier)
        <|> paren pUnit

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
        <|> try pNoArgFun
        <|> try pMultiArgFun
        <|> TRecord <$> paren (pRecordSpec pType)
        <|> TTable <$> (symbol "Table" *> paren (pRecordSpec pType))
        <|> TList <$> (symbol "List" *> paren pType)
        <|> TVar . Rigid <$> (char '\'' *> pIdentifier)
    optional (symbol "->" *> pType) >>= pure . \case
        Just t2 -> TFun [t1] t2
        Nothing -> t1
  where
    pMultiArgFun = TFun
        <$> paren (pType `sepBy1` symbol ",")
        <* symbol "->"
        <*> pType
    pNoArgFun = TFun [] <$ symbol "(" <* symbol ")" <* symbol "->" <*> pType

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

pLet :: Parser (ASTF ASTLoc)
pLet = symbol "Let" *> paren do
    v <- pIdentifier
    vt <- optional $ symbol ":" *> pPType
    _ <- symbol ","
    vx <- pExprInner
    _ <- symbol ","
    x <- pExprInner
    pure $ ASTLet v vt vx x
    

pLam :: Parser (ASTF ASTLoc)
pLam = ASTLam <$> try (args <* symbol "->") <*> pExprInner
  where
    args = paren (pIdentifier `sepBy` symbol ",")
        <|> (pure <$> pIdentifier)

pTermInner :: Parser (ASTF ASTLoc)
pTermInner = choice
    [ pLet
    , ASTNull <$ symbol "Null"
    , pLam
    , uncurry3 ASTRecord <$> pRecursivity <*> try (paren (pOrderedRecordSpec pExprInner))
    , try $ ASTFun <$> pIdentifier <*> paren (pExprInner `sepBy` symbol ",")
    , ASTLit <$> pLit
    , ASTVar <$> pIdentifier
    , spanExpr . unFix <$> paren pExprInner
    ]
  where
    uncurry3 :: (a -> b -> c -> x) -> (a -> (b,c) -> x)
    uncurry3 f = \a (b,c) -> f a b c

pTerm :: Parser ASTLoc
pTerm = do
    r@(Fix (AnnLocF SourceSpan{spanStart} _)) <- annotateLoc pTermInner
    choice
        [ annotateLoc' (withBeginning spanStart) $ ASTField r <$> (symbol "." *> pIdentifier)
        , annotateLoc' (withBeginning spanStart) $ ASTTApp r <$> (symbol ":" *> pPType)
        , annotateLoc' (withBeginning spanStart) $ ASTUnit r <$> try pUnit
        , pure r
        ]
  where
    withBeginning :: SourcePos -> SourceSpan -> ASTF ASTLoc -> ASTLoc
    withBeginning spanStart SourceSpan{spanEnd} =
        Fix . AnnLocF SourceSpan{spanStart, spanEnd}

pExprInner :: Parser ASTLoc
pExprInner = prec4
  where
    prec4 = mkOp prec3 $ symbol "*" <|> symbol "/"
    prec3 = mkOp prec2 $ symbol "+" <|> symbol "-"
    prec2 = mkOp prec1 $
        symbol "="
        <|> symbol "<>"
        <|> symbol ">"
        <|> symbol "<"
    prec1 = mkOp pTerm $ symbol "&&" <|> symbol "||"

    mkOp :: Parser ASTLoc -> Parser String -> Parser ASTLoc
    mkOp p po = go
      where
        go = annotateLoc' mkExprLoc $ (,) <$> p <*> optional ((,) <$> po <*> go)

        mkExprLoc _     (xloc, Nothing) = xloc
        mkExprLoc sspan (xloc1, Just (op, xloc2)) = Fix $ AnnLocF sspan $ ASTOp op xloc1 xloc2

pExpr :: Parser ASTLoc
pExpr = annotateLoc (ASTNull <$ eof) <|> pExprInner
