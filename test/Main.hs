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
import qualified Nexo.Expr.Type as Expr
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
        testEvalExpr "1"    @?= Just (Forall [] $ TNum tUno, VNum 1)
        testEvalExpr "1.23" @?= Just (Forall [] $ TNum tUno, VNum 1.23)
        testEvalExpr "-1.23" @?= Just (Forall [] $ TNum tUno, VNum (-1.23))
    , testCase "Bools" $ do
        testEvalExpr "True"  @?= Just (Forall [] TBool, VBool True)
        testEvalExpr "False" @?= Just (Forall [] TBool, VBool False)
    , testCase "Text" $ do
        testEvalExpr "\"\""            @?= Just (Forall [] TText, VText "")
        testEvalExpr "\"Hello world\"" @?= Just (Forall [] TText, VText "Hello world")
    , testCase "Null" $ do
        testEvalExpr "" @?= Just (Forall ["a"] $ TVarR "a", VNull)
        testEvalExpr "Null" @?= Just (Forall ["a"] $ TVarR "a", VNull)
    ]

values :: TestTree
values = testGroup "Values"
    [ testCase "Lists" $ do
        testEvalExpr "List(1,2,3)"    @?= Just (Forall [] $ TList (TNum tUno), VList (VNum <$> [1,2,3]))
        testEvalExpr "List(1,2,True)" @?= Nothing
    , testCase "Records" $ do
        testEvalExpr "(x: 1, y: True)" @?= Just
            ( Forall [] $ TRecord (Map.fromList [("x", TNum tUno), ("y", TBool)])
            , VRecord (Map.fromList [("x", VNum 1  ), ("y", VBool True)])
            )
        testEvalExpr "(x: 1, y: True).x" @?= Just (Forall [] $ TNum tUno, VNum 1)
        testEvalExpr "(x: List(1,2,3), y: True).x" @?= Just (Forall [] $ TList (TNum tUno), VList $ VNum <$> [1, 2, 3])
    , testCase "Tables" $ do
        testEvalExpr "Table(rec (x: List(1,2,3), y: (x+1)))" @?= Just
            ( Forall [] $ TTable $ Map.fromList [("x", TNum tUno), ("y", TNum tUno)]
            , VTable $ Map.fromList [("x", VNum <$> [1,2,3]), ("y", VNum <$> [2,3,4])]
            )
        testEvalExpr "Table(rec (x: (y-1), y: List(2,3,4)))" @?= Just
            ( Forall [] $ TTable $ Map.fromList [("x", TNum tUno), ("y", TNum tUno)]
            , VTable $ Map.fromList [("x", VNum <$> [1,2,3]), ("y", VNum <$> [2,3,4])]
            )
        testEvalExpr "Table(rec (x: 1, y: (x+1)))" @?= Nothing
        testEvalExpr "Table(rec (x: (y-1), y: List(2,3,4))).x" @?= Just ( Forall [] $ TList (TNum tUno), VList $ VNum <$> [1,2,3])
        testEvalExpr "Table(rec (x: List(List(1)), y: (x+1)))" @?= Just
            ( Forall [] $ TTable $ Map.fromList
                  [ ("x", TList (TNum tUno))
                  , ("y", TList (TNum tUno))
                  ]
            , VTable $ Map.fromList [("x",[VList [VNum 1]]), ("y",[VList [VNum 2]])]
            )
        testEvalExpr "Table(rec (y: (z+1), z: List(List(1))))" @?= Just
            ( Forall [] $ TTable $ Map.fromList
                  [ ("y", TList (TNum tUno))
                  , ("z", TList (TNum tUno))
                  ]
            , VTable $ Map.fromList [("y",[VList [VNum 2]]), ("z",[VList [VNum 1]])]
            )
        testEvalExpr "Table((x: List(1, Null)))" @?= Just
            ( Forall [] $ TTable $ Map.fromList [("x", TNum tUno)]
            , VTable $ Map.fromList [("x", [VNum 1, VNull])]
            )
    ]

functions :: TestTree
functions = testGroup "Functions"
    [ testCase "Functions" $ do
        testEvalExpr "If(True , 1, 2)" @?= Just (Forall [] $ TNum tUno, VNum 1)
        testEvalExpr "If(False, 1, 2)" @?= Just (Forall [] $ TNum tUno, VNum 2)
        testEvalExpr "Power(2, 8)" @?= Just (Forall [] $ TNum tUno, VNum 256)
        testEvalExpr "Mean(List(1,2,3,4,5))" @?= Just (Forall [] $ TNum tUno, VNum 3)
    , testCase "Operators" $ do
        testEvalExpr "1 + 1" @?= Just (Forall [] $ TNum tUno, VNum 2)
        testEvalExpr "2 * 3" @?= Just (Forall [] $ TNum tUno, VNum 6)
        testEvalExpr "5 < 1" @?= Just (Forall [] TBool, VBool False)
    , testCase "Broadcasting" $ do
        testEvalExpr "10 + List(1,2,3)" @?= Just (Forall [] $ TList $ TNum tUno, VList $ VNum <$> [11,12,13])
        testEvalExpr "List(10,20,30) + List(1,2,3)" @?= Just (Forall [] $ TList $ TNum tUno, VList $ VNum <$> [11,22,33])
        testEvalExpr "If(List(True,False), List(List(1,2),List(3,4)), List(10,20))" @?= Just
            ( Forall [] $ TList $ TList $ TNum tUno
            , VList [VList $ VNum <$> [1,20], VList $ VNum <$> [3,20]])
    , testCase "Null handling" $ do
        testEvalExpr "10 + Null"          @?= Just (Forall [] $ TNum tUno, VNull)
        testEvalExpr "Power(2, Null)"     @?= Just (Forall [] $ TNum tUno, VNull)
        testEvalExpr "If(True, 1, Null)"  @?= Just (Forall [] $ TNum tUno, VNum 1)
        testEvalExpr "If(False, 1, Null)" @?= Just (Forall [] $ TNum tUno, VNull)
    , testCase "Ascription" $ do
        testEvalExpr "1 : Num" @?= Just (Forall [] $ TNum tUno, VNum 1)
        testEvalExpr "1 : Bool" @?= Nothing
        testEvalExpr "List(True,False) : List(Bool)" @?= Just (Forall [] $ TList TBool, VList $ VBool <$> [True,False])
        testEvalExpr "List(True,False) : Bool" @?= Nothing
        testEvalExpr "(x: 1, y: True) : (x: Num, y: Bool)" @?= Just
            ( Forall [] $ TRecord (Map.fromList [("x", TNum tUno), ("y", TBool)])
            , VRecord (Map.fromList [("x", VNum 1  ), ("y", VBool True)])
            )
        fst <$> testEvalExpr "(x -> (x+1)) : Num -> Num" @?= Just (Forall [] $ TFun [TNum tUno] (TNum tUno))
        fst <$> testEvalExpr "(x -> (x+1)) : 't -> 't" @?= Nothing
        fst <$> testEvalExpr "((x,y) -> (x+y)) : Num<'u> -> Num<'u>" @?= Nothing
        fst <$> testEvalExpr "((x,y) -> (x+y)) : (Num<'u>, Num<'u>) -> Num<'u>" @?=
            Just (Forall ["u"] $ TFun [TNum $ TUnit (uVarR "u"), TNum $ TUnit (uVarR "u")] (TNum $ TUnit (uVarR "u")))
    , testCase "Let" $ do
        testEvalExpr "Let(x, 1, x)" @?= Just (Forall [] $ TNum tUno, VNum 1)
        testEvalExpr "Let(x : Num, 1, x)" @?= Just (Forall [] $ TNum tUno, VNum 1)
        testEvalExpr "Let(x : Bool, 1, x)" @?= Nothing
        testEvalExpr "Let(x : Num<m>, 1 km, x)" @?= Just (Forall [] $ TNum (tULeaf "m"), VNum 1000)
        testEvalExpr "Let(x : Num<m>, 1 km, x + 1 m)" @?= Just (Forall [] $ TNum (tULeaf "m"), VNum 1001)
    , testCase "Lambdas" $ do
        fmap fst (testEvalExpr "a -> a") @?= Just (Forall ["a"] $ TFun [TVarR "a"] $ TVarR "a")
        fmap fst (testEvalExpr "(a,b) -> a") @?= Just (Forall ["a","b"] $ TFun [TVarR "a",TVarR "b"] $ TVarR "a")
        fmap fst (testEvalExpr "a -> x") @?= Nothing
        fmap fst (testEvalExpr "num -> (num+1)") @?= Just (Forall [] $ TFun [TNum tUno] $ TNum tUno)
        fmap fst (testEvalExpr "(num,num2) -> (num+num2)") @?= Just (Forall ["c"] $ TFun [TNum $ TVar (Rigid "c"), TNum $ TVar (Rigid "c")] $ TNum $ TVar (Rigid "c"))
    ]

units :: TestTree
units = testGroup "Units"
    [ testCase "Unit application" $ do
        testEvalExpr "1 m"       @?= Just (Forall [] $ TNum (tULeaf "m"), VNum 1)
        testEvalExpr "1 km"      @?= Just (Forall [] $ TNum $ TUnit (1000, Map.singleton (Left "m") 1), VNum 1)
        testEvalExpr "1 m^-1"    @?= Just (Forall [] $ TNum $ TUnit (1, Map.singleton (Left "m") (-1)), VNum 1)
        testEvalExpr "List(1,2,3) s" @?= Just (Forall [] $ TList (TNum (tULeaf "s")), VList (VNum <$> [1,2,3]))
    , testCase "Unit computation" $ do
        testEvalExpr "(1 km) : Num<m>" @?= Just (Forall [] $ TNum (tULeaf "m"), VNum 1000)
        testEvalExpr "1 km : Num<s>" @?= Nothing
        testEvalExpr "(1 m^-1) : Num<km^-1>" @?= Just (Forall [] (TNum $ TUnit (1e-3, Map.singleton (Left "m") (-1))),VNum 1000)
        testEvalExpr "1 m + 2 m" @?= Just (Forall [] $ TNum (tULeaf "m"), VNum 3)
        testEvalExpr "1 km + 2 m" @?= Just (Forall [] $ TNum $ TUnit (1000, Map.singleton (Left "m") 1), VNum 1.002)
        testEvalExpr "1 s + 1 h" @?= Just (Forall [] $ TNum (tULeaf "s"), VNum 3601)
        testEvalExpr "1 m + 2 s" @?= Nothing
        testEvalExpr "1 m * 2 s" @?= Just (Forall [] $ TNum $ TUnit (1, Map.fromList [(Left "m", 1), (Left "s", 1)]), VNum 2)
        testEvalExpr "List(1,2,3) m + List(4,5,6) km" @?= Just (Forall [] (TList (TNum (tULeaf "m"))),VList [VNum 4001,VNum 5002,VNum 6003])
        testEvalExpr "List(1 m, 2 km)" @?= Just (Forall [] (TList (TNum (tULeaf "m"))),VList [VNum 1,VNum 2000])
        fst <$> testEvalExpr "x -> (x+1)" @?= Just (Forall [] (TFun [TNum tUno] (TNum tUno)))
        fst <$> testEvalExpr "x -> (x+1m)" @?= Just (Forall [] (TFun [TNum (tULeaf "m")] (TNum (tULeaf "m"))))
        testEvalExpr "Table(rec (x: List(1) m, y: (1 km + x)))" @?= Just
            ( Forall [] $ TTable $ Map.fromList
                  [ ("x", TNum (tULeaf "m"))
                  , ("y", TNum $ TUnit (1000, Map.singleton (Left "m") 1))
                  ]
            , VTable $ Map.fromList [("x",[VNum 1]), ("y",[VNum 1.001])]
            )
    ]

multis :: TestTree
multis = testGroup "Multiple cells"
    [ testCase "Cell references" $ do
        testEvalExprs [("test", "ref+1"), ("ref", "2")] @?=
            Just (Forall [] $ TNum tUno, VNum 3)
        testEvalExprs [("test", "ref+1"), ("ref", "2 m")] @?= Nothing
    , testCase "Custom functions" $
        testEvalExprs [("test", "AddOne(1)"), ("AddOne", "n -> (n+1)")] @?=
            Just (Forall [] $ TNum tUno, VNum 2)
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

genWidgetWithExpr :: MonadGen m => m (Expr.AST, Widget)
genWidgetWithExpr = Gen.frequency
    [ (3, genExpr <&> \expr -> (expr, ValueCell $ renderExpr expr))
    , (2, genTable <&> \tbl ->
              (mkTable Expr.Recursive tbl
              , Table $ second (bimap renderExpr (fmap renderExpr)) <$> tbl
              )
      )
    , (1, Gen.list (Range.linear 0 50) genExpr <&>
          \l -> (Fix $ Expr.ASTFun "List" l, InputList $ renderExpr <$> l))
    ]

-- NB. these ASTs do not in general typecheck!
genExpr :: MonadGen m => m Expr.AST
genExpr = Gen.recursive Gen.choice
    [ Fix . Expr.ASTLit <$> genLiteral
    , Fix . Expr.ASTVar <$> genIdentifier
    , pure $ Fix Expr.ASTNull
    ]
    [ mkTable
        <$> Gen.element [Expr.Nonrecursive, Expr.Recursive]
        <*> genTable
    , Gen.subterm genExpr $ Fix . Expr.ASTFun "Table" . pure
    , do
        i <- genIdentifier
        t <- Gen.maybe genType 
        Gen.subterm2 genExpr genExpr $ (Fix .) . Expr.ASTLet i t
    , Gen.list (Range.linear 0 10) genIdentifier >>=
        \is -> Gen.subterm genExpr (Fix . Expr.ASTLam is)
    , genIdentifier >>= \i -> Gen.subterm genExpr (Fix . flip Expr.ASTField i)
    , do
        o <- Gen.element ["=","<>","+","-","*","/",">","<","&&","||"]
        Gen.subterm2 genExpr genExpr (\x y -> Fix $ Expr.ASTOp o x y)
    , genUnitDef >>= \u -> Gen.subterm genExpr (Fix . flip Expr.ASTUnit u)
    , genType >>= \t -> Gen.subterm genExpr (Fix . flip Expr.ASTTApp t)
    ]

genLiteral :: MonadGen m => m Expr.Literal
genLiteral = Gen.choice
    [ Expr.Num <$> Gen.double (Range.exponentialFloat (-1e20) 1e20)
    , Expr.Bool <$> Gen.bool
    , Expr.Text <$> Gen.string (Range.exponential 0 500) Gen.unicode
    ]

mkTable :: Expr.Recursivity -> [(String, Either Expr.AST [Expr.AST])] -> Expr.AST
mkTable rec r = Fix $ Expr.ASTFun "Table" [Fix $ Expr.ASTRecord rec (mkCol <$> Map.fromList r) (fst <$> r)]
  where
    mkCol :: Either Expr.AST [Expr.AST] -> Expr.AST
    mkCol = either id (Fix . Expr.ASTFun "List")

genTable :: MonadGen m => m [(String, Either Expr.AST [Expr.AST])]
genTable = do
    rows <- Gen.int $ Range.linear 0 100
    m <- Gen.map (Range.linear 0 30) $ (,)
        <$> genIdentifier
        <*> Gen.either (Gen.filterT (/=Fix (Expr.ASTFun "List" [])) genExpr) (Gen.list (Range.singleton rows) genExpr)
    Gen.shuffle $ Map.toList m

genType :: MonadGen m => m Expr.Type
genType = Gen.recursive Gen.choice
    [ Expr.TNum <$> genUnitDef
    , pure Expr.TBool
    , pure Expr.TText
    , Expr.TVar <$> genIdentifier
    ]
    [ Gen.subtermM genType $ \t -> flip Expr.TFun t <$> Gen.list (Range.linear 0 10) genType
    , Gen.subterm genType Expr.TList
    , fmap Expr.TRecord $ Gen.map (Range.linear 0 50) $ (,) <$> genIdentifier <*> genType
    , fmap Expr.TTable  $ Gen.map (Range.linear 0 50) $ (,) <$> genIdentifier <*> genType
    ]

genUnitDef :: MonadGen m => m Expr.Unit
genUnitDef = Gen.recursive Gen.choice
    [ Expr.ULeaf <$> genIdentifier
    , (Expr.ULeaf .) . (++) <$> Gen.element prefixes <*> genIdentifier
    , Expr.UFactor <$> Gen.double (Range.exponentialFloat 1e-15 1e20)
    , Expr.UVar <$> genIdentifier
    ]
    [ Gen.subterm2 genUnitDef genUnitDef Expr.UMul
    , Gen.subterm2 genUnitDef genUnitDef Expr.UDiv
    , Gen.int (Range.linear (-10) 10) >>= \x -> Gen.subterm genUnitDef (flip Expr.UExp x)
    ]
  where
    prefixes = ["Y","Z","E","P","T","G","M","k","h","da","d","c","m","μ","u","n","p","f","a","z","y"]

genIdentifier :: MonadGen m => m String
genIdentifier = (:) <$> Gen.alpha <*> Gen.string (Range.linear 1 40)
    (Gen.frequency
        [ ((26*2)+10, Gen.alphaNum)
        , (1, pure '_')
        ])
