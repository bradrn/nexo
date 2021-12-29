module Main where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map.Strict as Map

import Nexo.Expr.Type
import Nexo.Expr.Unit
import Nexo.Interpret
import Nexo.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ literals
    , values
    , functions
    , units
    , multis
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
    , testCase "Null" $ do
        testEvalExpr "" @?= Just (Forall ["a"] [] $ TVar "a", VNull)
        testEvalExpr "Null" @?= Just (Forall ["a"] [] $ TVar "a", VNull)
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
        testEvalExpr "(x: [1,2,3], y: True).x" @?= Just (Forall [] [] $ TList (TNum Uno), VList $ VNum <$> [1, 2, 3])
    , testCase "Tables" $ do
        testEvalExpr "Table(rec (x: [1,2,3], y: (x+1)))" @?= Just
            ( Forall [] [] $ TTable $ Map.fromList [("x", TNum Uno), ("y", TNum Uno)]
            , VTable $ Map.fromList [("x", VNum <$> [1,2,3]), ("y", VNum <$> [2,3,4])]
            )
        testEvalExpr "Table(rec (x: (y-1), y: [2,3,4]))" @?= Just
            ( Forall [] [] $ TTable $ Map.fromList [("x", TNum Uno), ("y", TNum Uno)]
            , VTable $ Map.fromList [("x", VNum <$> [1,2,3]), ("y", VNum <$> [2,3,4])]
            )
        testEvalExpr "Table(rec (x: 1, y: (x+1)))" @?= Nothing
        testEvalExpr "Table(rec (x: (y-1), y: [2,3,4])).x" @?= Just ( Forall [] [] $ TList (TNum Uno), VList $ VNum <$> [1,2,3])
        testEvalExpr "Table(rec (x: [[1]], y: (x+1)))" @?= Just
            ( Forall [] [] $ TTable $ Map.fromList
                  [ ("x", TList (TNum Uno))
                  , ("y", TList (TNum Uno))
                  ]
            , VTable $ Map.fromList [("x",[VList [VNum 1]]), ("y",[VList [VNum 2]])]
            )
        testEvalExpr "Table(rec (y: (z+1), z: [[1]]))" @?= Just
            ( Forall [] [] $ TTable $ Map.fromList
                  [ ("y", TList (TNum Uno))
                  , ("z", TList (TNum Uno))
                  ]
            , VTable $ Map.fromList [("y",[VList [VNum 2]]), ("z",[VList [VNum 1]])]
            )
        testEvalExpr "Table((x: [1, Null]))" @?= Just
            ( Forall [] [] $ TTable $ Map.fromList [("x", TNum Uno)]
            , VTable $ Map.fromList [("x", [VNum 1, VNull])]
            )
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
    , testCase "Null handling" $ do
        testEvalExpr "10 + Null"          @?= Just (Forall [] [] $ TNum Uno, VNull)
        testEvalExpr "Power(2, Null)"     @?= Just (Forall [] [] $ TNum Uno, VNull)
        testEvalExpr "If(True, 1, Null)"  @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "If(False, 1, Null)" @?= Just (Forall [] [] $ TNum Uno, VNull)
    , testCase "Ascription" $ do
        testEvalExpr "1 : Num" @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "1 : Bool" @?= Nothing
        testEvalExpr "[True,False] : List Bool" @?= Just (Forall [] [] $ TList TBool, VList $ VBool <$> [True,False])
        testEvalExpr "[True,False] : Bool" @?= Nothing
    , testCase "Let" $ do
        testEvalExpr "Let(x = 1, x)" @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "Let(x : Num = 1, x)" @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "Let(x : Bool = 1, x)" @?= Nothing
        testEvalExpr "Let(x : Num<m> = 1 km, x)" @?= Just (Forall [] [] $ TNum (UName "m"), VNum 1000)
        testEvalExpr "Let(x : Num<m> = 1 km, x + 1 m)" @?= Just (Forall [] [] $ TNum (UName "m"), VNum 1001)
    , testCase "Lambdas" $ do
        fmap fst (testEvalExpr "a -> a") @?= Just (Forall ["a"] [] $ TFun [TVar "a"] $ TVar "a")
        fmap fst (testEvalExpr "(a,b) -> a") @?= Just (Forall ["a","b"] [] $ TFun [TVar "a",TVar "b"] $ TVar "a")
        fmap fst (testEvalExpr "a -> x") @?= Nothing
        fmap fst (testEvalExpr "num -> (num+1)") @?= Just (Forall [] [] $ TFun [TNum Uno] $ TNum Uno)
        fmap fst (testEvalExpr "(num,num2) -> (num+num2)") @?= Just (Forall [] ["c"] $ TFun [TNum (UVar "c"),TNum (UVar "c")] $ TNum (UVar "c"))
    ]

units :: TestTree
units = testGroup "Units"
    [ testCase "Unit application" $ do
        testEvalExpr "1 m"       @?= Just (Forall [] [] $ TNum (UName "m"), VNum 1)
        testEvalExpr "1 km"      @?= Just (Forall [] [] $ TNum (UMul (UPrefix "k") (UName "m")), VNum 1)
        testEvalExpr "1 m^-1"    @?= Just (Forall [] [] $ TNum (UExp (UName "m") (-1)), VNum 1)
        testEvalExpr "[1,2,3] s" @?= Just (Forall [] [] $ TList (TNum (UName "s")), VList (VNum <$> [1,2,3]))
    , testCase "Unit computation" $ do
        testEvalExpr "(1 km) : Num<m>" @?= Just (Forall [] [] $ TNum (UName "m"), VNum 1000)
        testEvalExpr "1 km : Num<s>" @?= Nothing
        testEvalExpr "(1 m^-1) : Num<km^-1>" @?= Just (Forall [] [] (TNum (UExp (UMul (UPrefix "k") (UName "m")) (-1))),VNum 1000)
        testEvalExpr "1 m + 2 m" @?= Just (Forall [] [] $ TNum (UName "m"), VNum 3)
        testEvalExpr "1 km + 2 m" @?= Just (Forall [] [] $ TNum (UMul (UName "m") (UFactor 1000)), VNum 1.002)
        testEvalExpr "1 s + 1 h" @?= Just (Forall [] [] $ TNum (UName "s"), VNum 3601)
        testEvalExpr "1 m + 2 s" @?= Nothing
        testEvalExpr "1 m * 2 s" @?= Just (Forall [] [] $ TNum (UMul (UName "m") (UName "s")), VNum 2)
        testEvalExpr "[1,2,3] m + [4,5,6] km" @?= Just (Forall [] [] (TList (TNum (UName "m"))),VList [VNum 4001,VNum 5002,VNum 6003])
        testEvalExpr "[1 m, 2 km]" @?= Just (Forall [] [] (TList (TNum (UName "m"))),VList [VNum 1,VNum 2000])
        testEvalExpr "Table(rec (x: [1] m, y: (1 km + x)))" @?= Just
            ( Forall [] [] $ TTable $ Map.fromList
                  [ ("x", TNum (UName "m"))
                  , ("y", TNum (UMul (UName "m") (UFactor 1000)))
                  ]
            , VTable $ Map.fromList [("x",[VNum 1]), ("y",[VNum 1.001])]
            )
    ]

multis :: TestTree
multis = testGroup "Multiple cells"
    [ testCase "Cell references" $ do
        testEvalExprs [("test", "ref+1"), ("ref", "2")] @?=
            Just (Forall [] [] $ TNum Uno, VNum 3)
        testEvalExprs [("test", "ref+1"), ("ref", "2 m")] @?= Nothing
    , testCase "Custom functions" $
        testEvalExprs [("test", "AddOne(1)"), ("AddOne", "n -> (n+1)")] @?=
            Just (Forall [] [] $ TNum Uno, VNum 2)
    ]
