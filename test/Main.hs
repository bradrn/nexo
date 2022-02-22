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
import Nexo.Core.Type
import Nexo.Core.Unit
import Nexo.Expr.Type
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
        testEvalExpr "1"    @?= Just (Forall [] $ CNum cUno, VNum 1)
        testEvalExpr "1.23" @?= Just (Forall [] $ CNum cUno, VNum 1.23)
        testEvalExpr "-1.23" @?= Just (Forall [] $ CNum cUno, VNum (-1.23))
    , testCase "Bools" $ do
        testEvalExpr "True"  @?= Just (Forall [] CBool, VBool True)
        testEvalExpr "False" @?= Just (Forall [] CBool, VBool False)
    , testCase "Text" $ do
        testEvalExpr "\"\""            @?= Just (Forall [] CText, VText "")
        testEvalExpr "\"Hello world\"" @?= Just (Forall [] CText, VText "Hello world")
    , testCase "Null" $ do
        testEvalExpr "" @?= Just (Forall ["a"] $ CVarR "a", VNull)
        testEvalExpr "Null" @?= Just (Forall ["a"] $ CVarR "a", VNull)
    ]

values :: TestTree
values = testGroup "Values"
    [ testCase "Lists" $ do
        testEvalExpr "List(1,2,3)"    @?= Just (Forall [] $ CList (CNum cUno), VList (VNum <$> [1,2,3]))
        testEvalExpr "List(1,2,True)" @?= Nothing
    , testCase "Records" $ do
        testEvalExpr "(x: 1, y: True)" @?= Just
            ( Forall [] $ CRecord (Map.fromList [("x", CNum cUno), ("y", CBool)])
            , VRecord (Map.fromList [("x", VNum 1  ), ("y", VBool True)])
            )
        testEvalExpr "(x: 1, y: True).x" @?= Just (Forall [] $ CNum cUno, VNum 1)
        testEvalExpr "(x: List(1,2,3), y: True).x" @?= Just (Forall [] $ CList (CNum cUno), VList $ VNum <$> [1, 2, 3])
    , testCase "Tables" $ do
        testEvalExpr "Table(rec (x: List(1,2,3), y: (x+1)))" @?= Just
            ( Forall [] $ CTable $ Map.fromList [("x", CNum cUno), ("y", CNum cUno)]
            , VTable $ Map.fromList [("x", VNum <$> [1,2,3]), ("y", VNum <$> [2,3,4])]
            )
        testEvalExpr "Table(rec (x: (y-1), y: List(2,3,4)))" @?= Just
            ( Forall [] $ CTable $ Map.fromList [("x", CNum cUno), ("y", CNum cUno)]
            , VTable $ Map.fromList [("x", VNum <$> [1,2,3]), ("y", VNum <$> [2,3,4])]
            )
        testEvalExpr "Table(rec (x: 1, y: (x+1)))" @?= Nothing
        testEvalExpr "Table(rec (x: (y-1), y: List(2,3,4))).x" @?= Just ( Forall [] $ CList (CNum cUno), VList $ VNum <$> [1,2,3])
        testEvalExpr "Table(rec (x: List(List(1)), y: (x+1)))" @?= Just
            ( Forall [] $ CTable $ Map.fromList
                  [ ("x", CList (CNum cUno))
                  , ("y", CList (CNum cUno))
                  ]
            , VTable $ Map.fromList [("x",[VList [VNum 1]]), ("y",[VList [VNum 2]])]
            )
        testEvalExpr "Table(rec (y: (z+1), z: List(List(1))))" @?= Just
            ( Forall [] $ CTable $ Map.fromList
                  [ ("y", CList (CNum cUno))
                  , ("z", CList (CNum cUno))
                  ]
            , VTable $ Map.fromList [("y",[VList [VNum 2]]), ("z",[VList [VNum 1]])]
            )
        testEvalExpr "Table((x: List(1, Null)))" @?= Just
            ( Forall [] $ CTable $ Map.fromList [("x", CNum cUno)]
            , VTable $ Map.fromList [("x", [VNum 1, VNull])]
            )
    ]

functions :: TestTree
functions = testGroup "Functions"
    [ testCase "Functions" $ do
        testEvalExpr "If(True , 1, 2)" @?= Just (Forall [] $ CNum cUno, VNum 1)
        testEvalExpr "If(False, 1, 2)" @?= Just (Forall [] $ CNum cUno, VNum 2)
        testEvalExpr "Power(2, 8)" @?= Just (Forall [] $ CNum cUno, VNum 256)
        testEvalExpr "Mean(List(1,2,3,4,5))" @?= Just (Forall [] $ CNum cUno, VNum 3)
    , testCase "Operators" $ do
        testEvalExpr "1 + 1" @?= Just (Forall [] $ CNum cUno, VNum 2)
        testEvalExpr "2 * 3" @?= Just (Forall [] $ CNum cUno, VNum 6)
        testEvalExpr "5 < 1" @?= Just (Forall [] CBool, VBool False)
    , testCase "Broadcasting" $ do
        testEvalExpr "10 + List(1,2,3)" @?= Just (Forall [] $ CList $ CNum cUno, VList $ VNum <$> [11,12,13])
        testEvalExpr "List(10,20,30) + List(1,2,3)" @?= Just (Forall [] $ CList $ CNum cUno, VList $ VNum <$> [11,22,33])
        testEvalExpr "If(List(True,False), List(List(1,2),List(3,4)), List(10,20))" @?= Just
            ( Forall [] $ CList $ CList $ CNum cUno
            , VList [VList $ VNum <$> [1,20], VList $ VNum <$> [3,20]])
    , testCase "Null handling" $ do
        testEvalExpr "10 + Null"          @?= Just (Forall [] $ CNum cUno, VNull)
        testEvalExpr "Power(2, Null)"     @?= Just (Forall [] $ CNum cUno, VNull)
        testEvalExpr "If(True, 1, Null)"  @?= Just (Forall [] $ CNum cUno, VNum 1)
        testEvalExpr "If(False, 1, Null)" @?= Just (Forall [] $ CNum cUno, VNull)
    , testCase "Ascription" $ do
        testEvalExpr "1 : Num" @?= Just (Forall [] $ CNum cUno, VNum 1)
        testEvalExpr "1 : Bool" @?= Nothing
        testEvalExpr "List(True,False) : List(Bool)" @?= Just (Forall [] $ CList CBool, VList $ VBool <$> [True,False])
        testEvalExpr "List(True,False) : Bool" @?= Nothing
        testEvalExpr "(x: 1, y: True) : (x: Num, y: Bool)" @?= Just
            ( Forall [] $ CRecord (Map.fromList [("x", CNum cUno), ("y", CBool)])
            , VRecord (Map.fromList [("x", VNum 1  ), ("y", VBool True)])
            )
        fst <$> testEvalExpr "(x -> (x+1)) : Num -> Num" @?= Just (Forall [] $ CFun [CNum cUno] (CNum cUno))
        fst <$> testEvalExpr "(x -> (x+1)) : 't -> 't" @?= Nothing
        fst <$> testEvalExpr "((x,y) -> (x+y)) : Num<'u> -> Num<'u>" @?= Nothing
        fst <$> testEvalExpr "((x,y) -> (x+y)) : (Num<'u>, Num<'u>) -> Num<'u>" @?=
            Just (Forall ["u"] $ CFun [CNum $ CUnit (uVarR "u"), CNum $ CUnit (uVarR "u")] (CNum $ CUnit (uVarR "u")))
    , testCase "Let" $ do
        testEvalExpr "Let(x, 1, x)" @?= Just (Forall [] $ CNum cUno, VNum 1)
        testEvalExpr "Let(x : Num, 1, x)" @?= Just (Forall [] $ CNum cUno, VNum 1)
        testEvalExpr "Let(x : Bool, 1, x)" @?= Nothing
        testEvalExpr "Let(x : Num<m>, 1 km, x)" @?= Just (Forall [] $ CNum (cULeaf "m"), VNum 1000)
        testEvalExpr "Let(x : Num<m>, 1 km, x + 1 m)" @?= Just (Forall [] $ CNum (cULeaf "m"), VNum 1001)
    , testCase "Lambdas" $ do
        fmap fst (testEvalExpr "a -> a") @?= Just (Forall ["a"] $ CFun [CVarR "a"] $ CVarR "a")
        fmap fst (testEvalExpr "(a,b) -> a") @?= Just (Forall ["a","b"] $ CFun [CVarR "a",CVarR "b"] $ CVarR "a")
        fmap fst (testEvalExpr "a -> x") @?= Nothing
        fmap fst (testEvalExpr "num -> (num+1)") @?= Just (Forall [] $ CFun [CNum cUno] $ CNum cUno)
        fmap fst (testEvalExpr "(num,num2) -> (num+num2)") @?= Just (Forall ["c"] $ CFun [CNum $ CUnit (uVarR "c"), CNum $ CUnit (uVarR "c")] $ CNum $ CUnit (uVarR "c"))
    ]

units :: TestTree
units = testGroup "Units"
    [ testCase "Unit application" $ do
        testEvalExpr "1 m"       @?= Just (Forall [] $ CNum (cULeaf "m"), VNum 1)
        testEvalExpr "1 km"      @?= Just (Forall [] $ CNum $ CUnit (1000, Map.singleton (Left "m") 1), VNum 1)
        testEvalExpr "1 m^-1"    @?= Just (Forall [] $ CNum $ CUnit (1, Map.singleton (Left "m") (-1)), VNum 1)
        testEvalExpr "List(1,2,3) s" @?= Just (Forall [] $ CList (CNum (cULeaf "s")), VList (VNum <$> [1,2,3]))
    , testCase "Unit computation" $ do
        testEvalExpr "(1 km) : Num<m>" @?= Just (Forall [] $ CNum (cULeaf "m"), VNum 1000)
        testEvalExpr "1 km : Num<s>" @?= Nothing
        testEvalExpr "(1 m^-1) : Num<km^-1>" @?= Just (Forall [] (CNum $ CUnit (1e-3, Map.singleton (Left "m") (-1))),VNum 1000)
        testEvalExpr "1 m + 2 m" @?= Just (Forall [] $ CNum (cULeaf "m"), VNum 3)
        testEvalExpr "1 km + 2 m" @?= Just (Forall [] $ CNum $ CUnit (1000, Map.singleton (Left "m") 1), VNum 1.002)
        testEvalExpr "1 s + 1 h" @?= Just (Forall [] $ CNum (cULeaf "s"), VNum 3601)
        testEvalExpr "1 m + 2 s" @?= Nothing
        testEvalExpr "1 m * 2 s" @?= Just (Forall [] $ CNum $ CUnit (1, Map.fromList [(Left "m", 1), (Left "s", 1)]), VNum 2)
        testEvalExpr "List(1,2,3) m + List(4,5,6) km" @?= Just (Forall [] (CList (CNum (cULeaf "m"))),VList [VNum 4001,VNum 5002,VNum 6003])
        testEvalExpr "List(1 m, 2 km)" @?= Just (Forall [] (CList (CNum (cULeaf "m"))),VList [VNum 1,VNum 2000])
        fst <$> testEvalExpr "x -> (x+1)" @?= Just (Forall [] (CFun [CNum cUno] (CNum cUno)))
        fst <$> testEvalExpr "x -> (x+1m)" @?= Just (Forall [] (CFun [CNum (cULeaf "m")] (CNum (cULeaf "m"))))
        testEvalExpr "Table(rec (x: List(1) m, y: (1 km + x)))" @?= Just
            ( Forall [] $ CTable $ Map.fromList
                  [ ("x", CNum (cULeaf "m"))
                  , ("y", CNum $ CUnit (1000, Map.singleton (Left "m") 1))
                  ]
            , VTable $ Map.fromList [("x",[VNum 1]), ("y",[VNum 1.001])]
            )
    ]

multis :: TestTree
multis = testGroup "Multiple cells"
    [ testCase "Cell references" $ do
        testEvalExprs [("test", "ref+1"), ("ref", "2")] @?=
            Just (Forall [] $ CNum cUno, VNum 3)
        testEvalExprs [("test", "ref+1"), ("ref", "2 m")] @?= Nothing
    , testCase "Custom functions" $
        testEvalExprs [("test", "AddOne(1)"), ("AddOne", "n -> (n+1)")] @?=
            Just (Forall [] $ CNum cUno, VNum 2)
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
          \l -> (Fix $ ASTFun "List" l, InputList $ renderExpr <$> l))
    ]

-- NB. these ASTs do not in general typecheck!
genExpr :: MonadGen m => m AST
genExpr = Gen.recursive Gen.choice
    [ Fix . ASTLit <$> genLiteral
    , Fix . ASTVar <$> genIdentifier
    , pure $ Fix ASTNull
    ]
    [ mkTable
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
    mkCol = either id (Fix . ASTFun "List")

genTable :: MonadGen m => m [(String, Either AST [AST])]
genTable = do
    rows <- Gen.int $ Range.linear 0 100
    m <- Gen.map (Range.linear 0 30) $ (,)
        <$> genIdentifier
        <*> Gen.either (Gen.filterT (/=Fix (ASTFun "List" [])) genExpr) (Gen.list (Range.singleton rows) genExpr)
    Gen.shuffle $ Map.toList m

genType :: MonadGen m => m Type
genType = Gen.recursive Gen.choice
    [ TNum <$> genUnitDef
    , pure TBool
    , pure TText
    , TVar <$> genIdentifier
    ]
    [ Gen.subtermM genType $ \t -> flip TFun t <$> Gen.list (Range.linear 0 10) genType
    , Gen.subterm genType TList
    , fmap TRecord $ Gen.map (Range.linear 0 50) $ (,) <$> genIdentifier <*> genType
    , fmap TTable  $ Gen.map (Range.linear 0 50) $ (,) <$> genIdentifier <*> genType
    ]

genUnitDef :: MonadGen m => m UnitDef
genUnitDef = Gen.recursive Gen.choice
    [ ULeaf <$> genIdentifier
    , (ULeaf .) . (++) <$> Gen.element prefixes <*> genIdentifier
    , UFactor <$> Gen.double (Range.exponentialFloat 1e-15 1e20)
    , UVar <$> genIdentifier
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
