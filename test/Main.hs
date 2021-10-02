module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Expr.Unit
import Nexo.Sheet

testEvalExpr :: String -> Maybe (PType, Value)
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
        testEvalExpr "1"    @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "1.23" @?= Just (Forall [] [] $ TNum Uno, VNum 1.23)
        testEvalExpr "-1.23" @?= Just (Forall [] [] $ TNum Uno, VNum (-1.23))
    , testCase "Bools" $ do
        testEvalExpr "True"  @?= Just (Forall [] [] TBool, VBool True)
        testEvalExpr "False" @?= Just (Forall [] [] TBool, VBool False)
    , testCase "Text" $ do
        testEvalExpr "\"\""            @?= Just (Forall [] [] TText, VText "")
        testEvalExpr "\"Hello world\"" @?= Just (Forall [] [] TText, VText "Hello world")
    ]

values :: TestTree
values = testGroup "Values"
    [ testCase "Lists" $ do
        testEvalExpr "[1,2,3]"    @?= Just (Forall [] [] $ TList (TNum Uno), VList (VNum <$> [1,2,3]))
        testEvalExpr "[1,2,True]" @?= Nothing
    , testCase "Records" $ do
        testEvalExpr "(x: 1, y: True)" @?= Just
            ( Forall [] [] $ TRecord (Map.fromList [("x", TNum Uno), ("y", TBool)])
            , VRecord (Map.fromList [("x", VNum 1  ), ("y", VBool True)])
            )
        testEvalExpr "(x: 1, y: True).x" @?= Just (Forall [] [] $ TNum Uno, VNum 1)
    ]

functions :: TestTree
functions = testGroup "Functions"
    [ testCase "Functions" $ do
        testEvalExpr "If(True , 1, 2)" @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "If(False, 1, 2)" @?= Just (Forall [] [] $ TNum Uno, VNum 2)
        testEvalExpr "Power(2, 8)" @?= Just (Forall [] [] $ TNum Uno, VNum 256)
        testEvalExpr "Mean([1,2,3,4,5])" @?= Just (Forall [] [] $ TNum Uno, VNum 3)
    , testCase "Operators" $ do
        testEvalExpr "1 + 1" @?= Just (Forall [] [] $ TNum Uno, VNum 2)
        testEvalExpr "2 * 3" @?= Just (Forall [] [] $ TNum (UMul Uno Uno), VNum 6)
        testEvalExpr "5 < 1" @?= Just (Forall [] [] TBool, VBool False)
    , testCase "Broadcasting" $ do
        testEvalExpr "10 + [1,2,3]" @?= Just (Forall [] [] $ TList $ TNum Uno, VList $ VNum <$> [11,12,13])
        testEvalExpr "[10,20,30] + [1,2,3]" @?= Just (Forall [] [] $ TList $ TNum Uno, VList $ VNum <$> [11,22,33])
        testEvalExpr "If([True,False], [[1,2],[3,4]], [10,20])" @?= Just
            ( Forall [] [] $ TList $ TList $ TNum Uno
            , VList [VList $ VNum <$> [1,20], VList $ VNum <$> [3,20]])
    ]

units :: TestTree
units = testGroup "Units"
    [ testCase "Unit application" $ do
        testEvalExpr "1 m"       @?= Just (Forall [] [] $ TNum (UName "m"), VNum 1)
        testEvalExpr "1 km"      @?= Just (Forall [] [] $ TNum (UMul (UPrefix "k") (UName "m")), VNum 1)
        testEvalExpr "1 m^-1"    @?= Just (Forall [] [] $ TNum (UExp (UName "m") (-1)), VNum 1)
        testEvalExpr "[1,2,3] s" @?= Just (Forall [] [] $ TList (TNum (UName "s")), VList (VNum <$> [1,2,3]))
    , testCase "Unit computation" $ do
        testEvalExpr "1 m + 2 m" @?= Just (Forall [] [] $ TNum (UName "m"), VNum 3)
        testEvalExpr "1 km + 2 m" @?= Just (Forall [] [] $ TNum (UMul (UName "m") (UFactor 1000)), VNum 1.002)
        testEvalExpr "1 s + 1 h" @?= Just (Forall [] [] $ TNum (UName "s"), VNum 3601)
        testEvalExpr "1 m + 2 s" @?= Nothing
        testEvalExpr "1 m * 2 s" @?= Just (Forall [] [] $ TNum (UMul (UName "m") (UName "s")), VNum 2)
    ]
