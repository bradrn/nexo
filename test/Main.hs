module Main where

import Data.Bifunctor (second, bimap)
import Data.Fix (Fix(Fix))
import Data.Functor ((<&>))
import Hedgehog
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import qualified Data.Map.Strict as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Nexo.Core.Substitute (generalise)
import Nexo.Expr.Type
import Nexo.Expr.Unit
import Nexo.Interpret
import Nexo.Render (renderExpr)
import Nexo.Sheet
import Nexo.Sheet.Parse
import Nexo.Sheet.Render (renderSheet)
import Nexo.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "Interpreter"
      [ literals
      , values
      , functions
      , units
      , multis
      ]
    , renderer
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
        testEvalExpr "" @?= Just (Forall ["a"] [] $ TVarR "a", VNull)
        testEvalExpr "Null" @?= Just (Forall ["a"] [] $ TVarR "a", VNull)
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
        testEvalExpr "[True,False] : List(Bool)" @?= Just (Forall [] [] $ TList TBool, VList $ VBool <$> [True,False])
        testEvalExpr "[True,False] : Bool" @?= Nothing
        testEvalExpr "(x: 1, y: True) : (x: Num, y: Bool)" @?= Just
            ( Forall [] [] $ TRecord (Map.fromList [("x", TNum Uno), ("y", TBool)])
            , VRecord (Map.fromList [("x", VNum 1  ), ("y", VBool True)])
            )
        fst <$> testEvalExpr "(x -> (x+1)) : Num -> Num" @?= Just (Forall [] [] $ TFun [TNum Uno] (TNum Uno))
        fst <$> testEvalExpr "(x -> (x+1)) : 't -> 't" @?= Nothing
        fst <$> testEvalExpr "((x,y) -> (x+y)) : Num<'u> -> Num<'u>" @?= Nothing
        fst <$> testEvalExpr "((x,y) -> (x+y)) : (Num<'u>, Num<'u>) -> Num<'u>" @?=
            Just (Forall [] ["u"] $ TFun [TNum (UVarR "u"), TNum (UVarR "u")] (TNum (UVarR "u")))
    , testCase "Let" $ do
        testEvalExpr "Let(x, 1, x)" @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "Let(x : Num, 1, x)" @?= Just (Forall [] [] $ TNum Uno, VNum 1)
        testEvalExpr "Let(x : Bool, 1, x)" @?= Nothing
        testEvalExpr "Let(x : Num<m>, 1 km, x)" @?= Just (Forall [] [] $ TNum (ULeaf "m"), VNum 1000)
        testEvalExpr "Let(x : Num<m>, 1 km, x + 1 m)" @?= Just (Forall [] [] $ TNum (ULeaf "m"), VNum 1001)
    , testCase "Lambdas" $ do
        fmap fst (testEvalExpr "a -> a") @?= Just (Forall ["a"] [] $ TFun [TVarR "a"] $ TVarR "a")
        fmap fst (testEvalExpr "(a,b) -> a") @?= Just (Forall ["a","b"] [] $ TFun [TVarR "a",TVarR "b"] $ TVarR "a")
        fmap fst (testEvalExpr "a -> x") @?= Nothing
        fmap fst (testEvalExpr "num -> (num+1)") @?= Just (Forall [] [] $ TFun [TNum Uno] $ TNum Uno)
        fmap fst (testEvalExpr "(num,num2) -> (num+num2)") @?= Just (Forall [] ["c"] $ TFun [TNum (UVarR "c"),TNum (UVarR "c")] $ TNum (UVarR "c"))
    ]

units :: TestTree
units = testGroup "Units"
    [ testCase "Unit application" $ do
        testEvalExpr "1 m"       @?= Just (Forall [] [] $ TNum (ULeaf "m"), VNum 1)
        testEvalExpr "1 km"      @?= Just (Forall [] [] $ TNum (ULeaf "km"), VNum 1)
        testEvalExpr "1 m^-1"    @?= Just (Forall [] [] $ TNum (UExp (ULeaf "m") (-1)), VNum 1)
        testEvalExpr "[1,2,3] s" @?= Just (Forall [] [] $ TList (TNum (ULeaf "s")), VList (VNum <$> [1,2,3]))
    , testCase "Unit computation" $ do
        testEvalExpr "(1 km) : Num<m>" @?= Just (Forall [] [] $ TNum (ULeaf "m"), VNum 1000)
        testEvalExpr "1 km : Num<s>" @?= Nothing
        testEvalExpr "(1 m^-1) : Num<km^-1>" @?= Just (Forall [] [] (TNum (UExp (ULeaf "km") (-1))),VNum 1000)
        testEvalExpr "1 m + 2 m" @?= Just (Forall [] [] $ TNum (ULeaf "m"), VNum 3)
        testEvalExpr "1 km + 2 m" @?= Just (Forall [] [] $ TNum (UMul (ULeaf "m") (UFactor 1000)), VNum 1.002)
        testEvalExpr "1 s + 1 h" @?= Just (Forall [] [] $ TNum (ULeaf "s"), VNum 3601)
        testEvalExpr "1 m + 2 s" @?= Nothing
        testEvalExpr "1 m * 2 s" @?= Just (Forall [] [] $ TNum (UMul (ULeaf "m") (ULeaf "s")), VNum 2)
        testEvalExpr "[1,2,3] m + [4,5,6] km" @?= Just (Forall [] [] (TList (TNum (ULeaf "m"))),VList [VNum 4001,VNum 5002,VNum 6003])
        testEvalExpr "[1 m, 2 km]" @?= Just (Forall [] [] (TList (TNum (ULeaf "m"))),VList [VNum 1,VNum 2000])
        fst <$> testEvalExpr "x -> (x+1)" @?= Just (Forall [] [] (TFun [TNum Uno] (TNum Uno)))
        fst <$> testEvalExpr "x -> (x+1m)" @?= Just (Forall [] [] (TFun [TNum (ULeaf "m")] (TNum (ULeaf "m"))))
        testEvalExpr "Table(rec (x: [1] m, y: (1 km + x)))" @?= Just
            ( Forall [] [] $ TTable $ Map.fromList
                  [ ("x", TNum (ULeaf "m"))
                  , ("y", TNum (UMul (ULeaf "m") (UFactor 1000)))
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

renderer :: TestTree
renderer = testGroup "Renderer"
    [ testProperty "tripping" $ property $ do
        imports <- forAll $ Gen.list (Range.linear 0 5) $
            (:) <$> Gen.alpha <*> Gen.string (Range.linear 0 10) Gen.alphaNum
        -- without resizing, the test takes a really long time
        sheet <- forAll $ Gen.resize 20 $ Map.fromList . zip [0..] <$> Gen.list (Range.linear 0 20) genCell
        tripping (Sheet imports Nothing sheet) renderSheet parseSheet
    ]

genCell :: MonadGen m => m Cell
genCell = do
    name <- genIdentifier
    type_ <- Gen.frequency
        -- choose Nothing much more frequently than normal
        -- since random types will probably be ill-typed
        [ (10, pure Nothing)
        , (1, Just <$> genType)
        ]
    (expr, widget) <- genWidgetWithExpr
    pure $ Cell
        { cellName = name
        , cellType = type_
        , cellWidget = widget
        , cellExpr = expr
        , cellValue = Invalidated
        }

genWidgetWithExpr :: MonadGen m => m (AST, Widget)
genWidgetWithExpr = Gen.frequency
    [ (3, genExpr <&> \expr -> (expr, ValueCell $ renderExpr expr))
    , (2, genTable <&> \tbl ->
              (mkTable Recursive tbl
              , Table $ second (bimap renderExpr (fmap renderExpr)) <$> tbl
              )
      )
    , (1, Gen.list (Range.linear 0 50) genExpr <&>
          \l -> (Fix $ ASTList l, InputList $ renderExpr <$> l))
    ]

-- NB. these ASTs do not in general typecheck!
genExpr :: MonadGen m => m AST
genExpr = Gen.recursive Gen.choice
    [ Fix . ASTLit <$> genLiteral
    , Fix . ASTVar <$> genIdentifier
    , pure $ Fix ASTNull
    ]
    [ Fix . ASTList <$> Gen.list (Range.linear 0 50) genExpr
    , mkTable
        <$> Gen.element [Nonrecursive, Recursive]
        <*> genTable
    , Gen.subterm genExpr $ Fix . ASTFun "Table" . pure
    , do
        i <- genIdentifier
        t <- Gen.maybe genType 
        Gen.subterm2 genExpr genExpr $ (Fix .) . ASTLet i t
    , Gen.list (Range.linear 0 10) genIdentifier >>=
        \is -> Gen.subterm genExpr (Fix . ASTLam is)
    , genIdentifier >>= \i -> Gen.subterm genExpr (Fix . flip ASTField i)
    , do
        o <- Gen.element ["=","<>","+","-","*","/",">","<","&&","||"]
        Gen.subterm2 genExpr genExpr (\x y -> Fix $ ASTOp o x y)
    , genUnitDef >>= \u -> Gen.subterm genExpr (Fix . flip ASTUnit u)
    , genType >>= \t -> Gen.subterm genExpr (Fix . flip ASTTApp t)
    ]

genLiteral :: MonadGen m => m Literal
genLiteral = Gen.choice
    [ LNum <$> Gen.double (Range.exponentialFloat (-1e20) 1e20)
    , LBool <$> Gen.bool
    , LText <$> Gen.string (Range.exponential 0 500) Gen.unicode
    ]

mkTable :: Recursivity -> [(String, Either AST [AST])] -> AST
mkTable rec r = Fix $ ASTFun "Table" [Fix $ ASTRecord rec (mkCol <$> Map.fromList r) (fst <$> r)]
  where
    mkCol :: Either AST [AST] -> AST
    mkCol = either id (Fix . ASTList)

genTable :: MonadGen m => m [(String, Either AST [AST])]
genTable = do
    rows <- Gen.int $ Range.linear 0 100
    m <- Gen.map (Range.linear 0 30) $ (,)
        <$> genIdentifier
        <*> Gen.either (Gen.filterT (/=Fix (ASTList [])) genExpr) (Gen.list (Range.singleton rows) genExpr)
    Gen.shuffle $ Map.toList m

genType :: MonadGen m => m PType
genType = generalise <$> go
  where
    go :: MonadGen m => m Type
    go = Gen.recursive Gen.choice
        [ TNum <$> genUnitDef
        , pure TBool
        , pure TText
        , TVarR <$> genIdentifier
        ]
        [ Gen.subtermM go $ \t -> flip TFun t <$> Gen.list (Range.linear 0 10) go
        , Gen.subterm go TList
        , fmap TRecord $ Gen.map (Range.linear 0 50) $ (,) <$> genIdentifier <*> go
        , fmap TTable  $ Gen.map (Range.linear 0 50) $ (,) <$> genIdentifier <*> go
        ]

genUnitDef :: MonadGen m => m UnitDef
genUnitDef = Gen.recursive Gen.choice
    [ ULeaf <$> genIdentifier
    , (ULeaf .) . (++) <$> Gen.element prefixes <*> genIdentifier
    , UFactor <$> Gen.double (Range.exponentialFloat 1e-15 1e20)
    , UVarR <$> genIdentifier
    ]
    [ Gen.subterm2 genUnitDef genUnitDef UMul
    , Gen.subterm2 genUnitDef genUnitDef UDiv
    , Gen.int (Range.linear (-10) 10) >>= \x -> Gen.subterm genUnitDef (flip UExp x)
    ]
  where
    prefixes = ["Y","Z","E","P","T","G","M","k","h","da","d","c","m","μ","u","n","p","f","a","z","y"]

genIdentifier :: MonadGen m => m String
genIdentifier = (:) <$> Gen.alpha <*> Gen.string (Range.linear 1 40)
    (Gen.frequency
        [ ((26*2)+10, Gen.alphaNum)
        , (1, pure '_')
        ])
