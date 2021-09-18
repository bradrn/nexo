module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Expr.Unit
import Nexo.Sheet

testEvalExpr :: String -> Maybe (Type, Value)
testEvalExpr xstr = do
    x <- parseMaybe pExpr xstr
    let c = Cell "test" Nothing x Invalidated
        s = Sheet $ Map.singleton 0 c
        Sheet s' = evalSheet s
    val <- cellValue <$> Map.lookup 0 s'
    case val of
        ValuePresent t v -> Just (t, v)
        _ -> Nothing

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ literals
    , values
    , functions
    , units
    ]

literals :: TestTree
literals = testGroup "Literals"
    [ testCase "Numbers" $ do
        testEvalExpr "1"    @?= Just (TNum Uno, VNum 1)
        testEvalExpr "1.23" @?= Just (TNum Uno, VNum 1.23)
        testEvalExpr "-1.23" @?= Just (TNum Uno, VNum (-1.23))
    , testCase "Bools" $ do
        testEvalExpr "True"  @?= Just (TBool, VBool True)
        testEvalExpr "False" @?= Just (TBool, VBool False)
    , testCase "Text" $ do
        testEvalExpr "\"\""            @?= Just (TText, VText "")
        testEvalExpr "\"Hello world\"" @?= Just (TText, VText "Hello world")
    ]

values :: TestTree
values = testGroup "Values"
    [ testCase "Lists" $ do
        testEvalExpr "[1,2,3]"    @?= Just (TList (TNum Uno), VList (VNum <$> [1,2,3]))
        testEvalExpr "[1,2,True]" @?= Nothing
    , testCase "Records" $ do
        testEvalExpr "(x: 1, y: True)" @?= Just
            ( TRecord (Map.fromList [("x", TNum Uno), ("y", TBool)])
            , VRecord (Map.fromList [("x", VNum 1  ), ("y", VBool True)])
            )
        testEvalExpr "(x: 1, y: True).x" @?= Just (TNum Uno, VNum 1)
    ]

functions :: TestTree
functions = testGroup "Functions"
    [ testCase "Functions" $ do
        testEvalExpr "If(True , 1, 2)" @?= Just (TNum Uno, VNum 1)
        testEvalExpr "If(False, 1, 2)" @?= Just (TNum Uno, VNum 2)
        testEvalExpr "Power(2, 8)" @?= Just (TNum Uno, VNum 256)
        testEvalExpr "Mean([1,2,3,4,5])" @?= Just (TNum Uno, VNum 3)
    , testCase "Operators" $ do
        testEvalExpr "1 + 1" @?= Just (TNum Uno, VNum 2)
        testEvalExpr "2 * 3" @?= Just (TNum Uno, VNum 6)
        testEvalExpr "5 < 1" @?= Just (TBool, VBool False)
    ]

units :: TestTree
units = testGroup "Units"
    [ testCase "Unit application" $ do
        testEvalExpr "1 m"       @?= Just (TNum (UName "m"), VNum 1)
        testEvalExpr "1 km"      @?= Just (TNum (UMul (UPrefix "k") (UName "m")), VNum 1)
        testEvalExpr "1 m^-1"    @?= Just (TNum (UExp (UName "m") (-1)), VNum 1)
        testEvalExpr "[1,2,3] s" @?= Just (TList (TNum (UName "s")), VList (VNum <$> [1,2,3]))
    , testCase "Unit computation" $ do
        testEvalExpr "1 m + 2 m" @?= Just (TNum (UName "m"), VNum 3)
        testEvalExpr "1 m + 2 s" @?= Nothing
        testEvalExpr "1 m * 2 s" @?= Just (TNum (UMul (UName "m") (UName "s")), VNum 2)
    ]
