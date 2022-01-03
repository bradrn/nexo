{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Nexo.Expr.Type.Annotated where

import Data.Fix (Fix)
import Data.Functor.Foldable (hoist)
import Text.Megaparsec.Pos (SourcePos(..), unPos)

import Nexo.Expr.Type

data SourceSpan = SourceSpan
    { spanStart :: SourcePos
    , spanEnd   :: SourcePos
    } deriving (Show)

extractSpan :: SourceSpan -> String -> String
extractSpan
    SourceSpan
    { spanStart = SourcePos{sourceLine = unPos -> l1, sourceColumn = unPos -> c1}
    , spanEnd   = SourcePos{sourceLine = unPos -> l2, sourceColumn = unPos -> c2}
    } str
    | l1 == l2 =
        let line = lines str !! (l1-1)
        in take (c2-c1) $ drop (c1-1) line
    | otherwise =
        let (line:rest) = take (l2-l1) $ drop (l1-1) $ lines str
        in unlines $ drop (c1-1) line : init rest ++ [take (c2-1) $ last rest]

data ExprLocF r = ExprLocF
    { span :: SourceSpan
    , spanExpr :: ExprF r
    } deriving (Show, Functor)

-- TODO: use to give nicer typechecker errors also
type ExprLoc = Fix ExprLocF

delocalise :: ExprLoc -> Expr
delocalise = hoist spanExpr
