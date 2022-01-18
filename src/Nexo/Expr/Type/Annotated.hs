{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

module Nexo.Expr.Type.Annotated where

import Data.Deriving (deriveShow1)
import Data.Fix (Fix)
import Data.Functor.Foldable (hoist)
import Data.List (intercalate)
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
        let (line:rest) = take (l2-l1+1) $ drop (l1-1) $ lines str
        in intercalate "\n" $ drop (c1-1) line : init rest ++ [take (c2-1) $ last rest]

data AnnLocF t r = AnnLocF
    { span :: SourceSpan
    , spanExpr :: t r
    } deriving (Show, Functor)
deriveShow1 ''AnnLocF

-- TODO: use to give nicer typechecker errors also
type ASTLoc = Fix (AnnLocF ASTF)
type ExprLoc = Fix (AnnLocF ExprF)

delocalise :: Functor t => Fix (AnnLocF t) -> Fix t
delocalise = hoist spanExpr
