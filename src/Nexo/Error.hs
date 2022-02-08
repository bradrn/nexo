{-# LANGUAGE NamedFieldPuns #-}

module Nexo.Error where

import Nexo.Core.Type (TypeError(..), Mismatch(..), Context(..), CoreExpr(CVar))
import Nexo.Interpret (RuntimeError(..))
import Nexo.Render (renderMonomorphicType)

data Error
    = ParseError
    | TypeError TypeError
    | RuntimeError RuntimeError
    deriving (Show, Eq)

renderError :: Error -> String
renderError ParseError = "Error: parse error"
renderError (TypeError e) = "Error: type error\n" ++ renderTypeError e
renderError (RuntimeError e) = "Error: runtime error\n" ++ renderRuntimeError e

renderTypeError :: TypeError -> String
renderTypeError (UnknownName s) = "Unknown name: " ++ s
renderTypeError (WrongNumberOfArguments s) = "Wrong number of arguments supplied to " ++ s
renderTypeError (RecordFieldAbsent s) = "Attempted to access nonexistent field: " ++ s
renderTypeError (TableColumnsUnknown ty) =
    "Attempted to create table from record without known column names.\nThe supplied record has the following type: "
    ++ renderMonomorphicType ty
renderTypeError LambdaArgumentNotVariable = "Attempted to use non-variable as argument to lambda"
renderTypeError KindMismatch = "Attempted to use same variable for type and unit"
renderTypeError (TypeMismatch ctx Mismatch{tSupplied, tDeclared}) =
    place ++ " has an unexpected type.\nThis expression has the following type: "
    ++ renderMonomorphicType tSupplied ++ "\nBut " ++ reason ++ ": " ++ renderMonomorphicType tDeclared
  where
    (place, reason) = case ctx of
      TypeSpecification -> ("The argument of a type specification", "the type specification says its type should be")
      ArgumentOfFunction e ->
          let e' = case e of
                 CVar f -> "The expression " ++ f
                 _ -> "An unnamed expression"
          in (e' ++ ", used in a function application,", "the function used should satsify the following type")
      ListElement -> ("An element of a list", "the other elements of the list have type")
      RecordField s -> ("The field " ++ s, "the field was declared with type")
      TableColumnNotList s -> ("The table column " ++ s, s ++ " should be a list of type")
      UnitAp -> ("The argument of a unit application", "the argument of a unit application may only be of type")

renderRuntimeError :: RuntimeError -> String
renderRuntimeError DimensionMismatch = "Dimension mismatch"
