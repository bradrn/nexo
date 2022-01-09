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

pLet :: Parser (ExprF ExprLoc)
pLet = symbol "Let" *> paren do
    v <- pIdentifier
    vt <- optional $ symbol ":" *> pPType
    _ <- symbol ","
    vx <- pExprInner
    _ <- symbol ","
    x <- pExprInner
    pure $ XLet v vt vx x
    

pLam :: Parser (ExprF ExprLoc)
pLam = XLam <$> try (args <* symbol "->") <*> pTerm
  where
    args = paren (pIdentifier `sepBy` symbol ",")
        <|> (pure <$> pIdentifier)

pTermInner :: Parser (ExprF ExprLoc)
pTermInner = choice
    [ pLet
    , XTable <$> (symbol "Table" *> paren pExprInner)
    , XNull <$ symbol "Null"
    , XList <$> sqparen (pExprInner `sepBy` symbol ",")
    , pLam
    , uncurry3 XRecord <$> pRecursivity <*> try (paren (pOrderedRecordSpec pTerm))
    , try $ XFun <$> pIdentifier <*> paren (pExprInner `sepBy` symbol ",")
    , XLit <$> pLit
    , XVar <$> pIdentifier
    , spanExpr . unFix <$> paren pExprInner
    ]
  where
    uncurry3 :: (a -> b -> c -> x) -> (a -> (b,c) -> x)
    uncurry3 f = \a (b,c) -> f a b c

pTerm :: Parser ExprLoc
pTerm = do
    r@(Fix (ExprLocF SourceSpan{spanStart} _)) <- annotateLoc pTermInner
    choice
        [ annotateLoc' (withBeginning spanStart) $ XField r <$> (symbol "." *> pIdentifier)
        , annotateLoc' (withBeginning spanStart) $ XTApp r <$> (symbol ":" *> pPType)
        , annotateLoc' (withBeginning spanStart) $ XUnit r <$> try pUnit
        , pure r
        ]
  where
    withBeginning :: SourcePos -> SourceSpan -> ExprF ExprLoc -> ExprLoc
    withBeginning spanStart SourceSpan{spanEnd} =
        Fix . ExprLocF SourceSpan{spanStart, spanEnd}

pExprInner :: Parser ExprLoc
pExprInner = prec4
  where
    prec4 = mkOp prec3 $ (OTimes <$ symbol "*") <|> (ODiv <$ symbol "/")
    prec3 = mkOp prec2 $ (OPlus <$ symbol "+") <|> (OMinus <$ symbol "-")
    prec2 = mkOp prec1 $
        OEq <$ symbol "="
        <|> ONeq <$ symbol "<>"
        <|> OGt <$ symbol ">"
        <|> OLt <$ symbol "<"
    prec1 = mkOp pTerm $ (OAnd <$ symbol "&&") <|> (OOr <$ symbol "||")

    mkOp :: Parser ExprLoc -> Parser Op -> Parser ExprLoc
    mkOp p po = go
      where
        go = annotateLoc' mkExprLoc $ (,) <$> p <*> optional ((,) <$> po <*> go)

        mkExprLoc _     (xloc, Nothing) = xloc
        mkExprLoc sspan (xloc1, Just (op, xloc2)) = Fix $ ExprLocF sspan $ XOp op xloc1 xloc2

pExpr :: Parser ExprLoc
pExpr = annotateLoc (XNull <$ eof) <|> pExprInner
