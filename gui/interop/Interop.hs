{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Interop where

import Control.Monad ((<=<), zipWithM, (>=>))
import Control.Monad.Trans.Except (runExceptT)
import Data.Fix (Fix(Fix))
import Data.IORef
import Data.List (genericLength, genericIndex)
import Data.Traversable (for)
import Foreign
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)

import qualified Data.Map.Strict as Map

import qualified Nexo.Core.Type as Core
import Nexo.Expr.Parse
import qualified Nexo.Expr.Type as Expr
import Nexo.Expr.Type.Annotated (delocalise)
import Nexo.Interpret (Value(..), render)
import Nexo.Render (renderMonomorphicType, renderCoreType)
import Nexo.Sheet
import Nexo.Sheet.Parse (parseSheet)
import Nexo.Sheet.Render (renderSheet)
import Nexo.Sheet.Import (getImport)

hsNewSheet :: IO (StablePtr (IORef Sheet))
hsNewSheet = newStablePtr =<< newIORef (Sheet [] Nothing Map.empty)

cFalse, cTrue :: CBool
cFalse = 0
cTrue = 1

hsParseExpr :: CString -> Ptr CBool -> IO (StablePtr (Expr.AST, Widget))
hsParseExpr cinput successPtr = do
    input <- GHC.peekCString utf8 cinput
    case parseMaybe pExpr input of
        Nothing -> poke successPtr cFalse >> newStablePtr (Fix Expr.ASTNull, ValueCell "")
        Just (delocalise -> xp) -> poke successPtr cTrue >> newStablePtr (xp, ValueCell input)

hsParseLiteralList :: CInt -> Ptr CString -> Ptr CBool -> IO (StablePtr (Expr.AST, Widget))
hsParseLiteralList clen cinput successPtr = do
    cinputs <- peekArray (fromIntegral clen) cinput
    inputs <- traverse (GHC.peekCString utf8) cinputs
    case traverse (parseMaybe pLit) inputs of
        Nothing -> poke successPtr cFalse >> newStablePtr (Fix Expr.ASTNull, ValueCell "")
        Just values ->
            let xp = Fix . Expr.ASTFun "List" $ Fix . Expr.ASTLit <$> values
            in poke successPtr cTrue >> newStablePtr (xp, InputList inputs)

hsParseTable :: CInt -> Ptr CString -> Ptr (Ptr CString) -> Ptr (Ptr CString) -> Ptr CInt -> Ptr (Ptr CString) -> Ptr CBool -> IO (StablePtr (Expr.AST, Widget))
hsParseTable clen cheader ctype cformula ccollen ccol successPtr = do
    cheaders <- peekArray (fromIntegral clen) cheader
    headers <- traverse (GHC.peekCString utf8) cheaders

    ctypes <- peekArray (fromIntegral clen) ctype
    types <- traverse (traverse (GHC.peekCString utf8) <=< ptrToMaybe) ctypes
    let ptypes = fmap (>>= parseMaybe pType) types

    cformulae <- peekArray (fromIntegral clen) cformula
    formulae <- traverse (traverse (GHC.peekCString utf8) <=< ptrToMaybe) cformulae
    let formulaeExprs = fmap (>>= parseMaybe pExpr) formulae

    ccollens <- peekArray (fromIntegral clen) ccollen
    ccols <- peekArray (fromIntegral clen) ccol
    ccolss <- traverse (uncurry peekArray) $ zip (fromIntegral <$> ccollens) ccols
    colss <- (traverse.traverse) (GHC.peekCString utf8) ccolss
    let colExprss = (fmap.traverse) (parseMaybe pExpr) colss

    let columns = zipWithM mkColumnExpr
            ((fmap.fmap) delocalise formulaeExprs)
            ((fmap.fmap.fmap) delocalise colExprss)
        typedColumns = zipWithM mkTypedColumn ptypes =<< columns

    case typedColumns of
        Nothing -> poke successPtr cFalse >> newStablePtr (Fix Expr.ASTNull, ValueCell "")
        Just typedColumns' ->
            let xp = Fix $ Expr.ASTFun "Table" [Fix $ Expr.ASTRecord Expr.Recursive (Map.fromList $ zip headers typedColumns') headers]
            in poke successPtr cTrue >> newStablePtr (xp, Table $ zip headers $ zipWith mkColumnEither formulae colss)
  where
    ptrToMaybe :: Storable a => Ptr a -> IO (Maybe a)
    ptrToMaybe p
        | p == nullPtr = pure Nothing
        | otherwise    = Just <$> peek p

    mkColumnExpr :: Maybe Expr.AST -> Maybe [Expr.AST] -> Maybe Expr.AST
    mkColumnExpr (Just x) _         = Just x
    mkColumnExpr _       (Just col) = Just $ Fix $ Expr.ASTFun "List" col
    mkColumnExpr Nothing Nothing    = Nothing

    mkTypedColumn :: Maybe Expr.Type -> Expr.AST -> Maybe Expr.AST
    mkTypedColumn Nothing  x = Just x
    mkTypedColumn (Just t) x = Just $ Fix $ Expr.ASTTApp x (Expr.TList t)

    mkColumnEither :: Maybe String -> [String] -> Either String [String]
    mkColumnEither (Just x) _ = Left x
    mkColumnEither Nothing  x = Right x

hsMaybeParseType :: CString -> IO (StablePtr (Maybe Expr.Type))
hsMaybeParseType cinput = do
    input <- GHC.peekCString utf8 cinput
    newStablePtr $ parseMaybe pType input

hsNothing :: IO (StablePtr (Maybe a))
hsNothing = newStablePtr Nothing

hsMkCell :: CString -> StablePtr (Maybe Expr.Type) -> StablePtr (Expr.AST, Widget) -> IO (StablePtr Cell)
hsMkCell cname tptr xptr = do
    name <- GHC.peekCString utf8 cname
    type_ <- deRefStablePtr tptr
    (expr, widget) <- deRefStablePtr xptr
    newStablePtr $ Cell name type_ widget expr Invalidated

hsInsert :: CInt -> StablePtr Cell -> StablePtr (IORef Sheet) -> IO ()
hsInsert k cptr sptr = do
    cell <- deRefStablePtr cptr
    sref <- deRefStablePtr sptr
    modifyIORef' sref $ insert (fromIntegral k) cell

hsEvalSheet :: CString -> StablePtr (IORef Sheet) -> IO CBool
hsEvalSheet cdir ptr = do
    dir <-
        if cdir == nullPtr
        then pure Nothing
        else Just <$> GHC.peekCString utf8 cdir
    ref <- deRefStablePtr ptr
    s <- readIORef ref
    runExceptT (evalSheet (getImport dir) s) >>= \case
        Left _ -> pure cFalse
        Right s' -> cTrue <$ writeIORef ref s'

hsCellIndices :: StablePtr (IORef Sheet) -> Ptr CInt -> IO (Ptr CInt)
hsCellIndices ptr lptr = do
    ref <- deRefStablePtr ptr
    Sheet _ _ s <- readIORef ref
    poke lptr $ fromIntegral $ length s
    newArray $ fromIntegral <$> Map.keys s

hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr ValueState')
hsQuery k ptr successPtr = do
    ref <- deRefStablePtr ptr
    Sheet _ _ s <- readIORef ref
    case Map.lookup (fromIntegral k) s of
        Nothing -> poke successPtr cFalse >> newStablePtr Invalidated
        Just cl -> poke successPtr cTrue  >> newStablePtr (cellValue cl)

hsQueryCell :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr Cell)
hsQueryCell k ptr successPtr = do
    ref <- deRefStablePtr ptr
    Sheet _ _ s <- readIORef ref
    case Map.lookup (fromIntegral k) s of
        Nothing -> poke successPtr cFalse >> newStablePtr (Cell "" Nothing (ValueCell "") (Fix Expr.ASTNull) Invalidated)
        Just cl -> poke successPtr cTrue  >> newStablePtr cl

hsWidgetType :: StablePtr Cell -> IO CInt
hsWidgetType = fmap cellWidget . deRefStablePtr >=> pure . \case
   ValueCell _ -> 0
   InputList _ -> 1
   Table _ -> 2

hsWidgetCols :: StablePtr Cell -> IO CInt
hsWidgetCols = fmap cellWidget . deRefStablePtr >=> pure . \case
    Table t -> genericLength t
    _ -> 1

-- Warning: this assumes the 'Cell' is fully evaluated!
hsWidgetRows :: StablePtr Cell -> IO CInt
hsWidgetRows = deRefStablePtr >=> pure . \case
    Cell{cellWidget = ValueCell _} -> 1
    Cell{cellWidget = InputList l} -> genericLength l
    Cell{cellWidget = Table _
        ,cellValue = ValuePresent _ (VTable (Map.elems -> (col:_)))}
        -> genericLength col + 2
    Cell{cellWidget = Table _} -> 2  -- header rows

hsExprAt :: CInt -> CInt -> StablePtr Cell -> IO CString
hsExprAt row col = fmap cellWidget . deRefStablePtr >=> GHC.newCString utf8 . \case
   ValueCell c -> c
   InputList l -> genericIndex l row
   Table t -> case row of
       0 -> fst $ genericIndex t col
       1 -> case snd $ genericIndex t col of
           Left f -> f
           Right _ -> ""
       (subtract 2 -> row') -> case snd $ genericIndex t col of
           Left _ -> ""
           Right l -> genericIndex l row'

hsCellName :: StablePtr Cell -> IO CString
hsCellName = GHC.newCString utf8 . cellName <=< deRefStablePtr

hsCellType :: StablePtr Cell -> IO CString
hsCellType = GHC.newCString utf8 . maybe "" renderMonomorphicType . cellType <=< deRefStablePtr

-- partial function - assumes that the column exists
hsCellTypeOfColumn :: CString -> StablePtr Cell -> IO CString
hsCellTypeOfColumn ccol ccell = do
    cell <- deRefStablePtr ccell
    col <- GHC.peekCString utf8 ccol
    GHC.newCString utf8 $ case cellValue cell of
        ValuePresent t _ -> renderCoreType $ typeAtColumn col t
        _ -> ""
  where
    typeAtColumn :: String -> Core.PType -> Core.Type
    typeAtColumn col (Core.Forall _ (Core.TTable  rec)) = rec Map.! col
    typeAtColumn col (Core.Forall _ (Core.TRecord rec)) = rec Map.! col
    typeAtColumn _ _ = error "hsCellTypeOfColumn: unknown column"

hsParseSheet :: CString -> Ptr CBool -> IO (StablePtr (IORef Sheet))
hsParseSheet cinput successPtr = do
    input <- GHC.peekCString utf8 cinput
    case parseSheet input of
        Nothing -> do
            poke successPtr cFalse
            hsNewSheet
        Just s -> do
            poke successPtr cTrue
            newStablePtr =<< newIORef s

hsRenderSheet :: StablePtr (IORef Sheet) -> IO CString
hsRenderSheet = GHC.newCString utf8 . renderSheet <=< readIORef <=< deRefStablePtr

hsDisplayError :: StablePtr ValueState' -> IO CString
hsDisplayError ptr = deRefStablePtr ptr >>= \case
    ValuePresent _ _ -> GHC.newCString utf8 ""
    vs -> GHC.newCString utf8 $ display vs

hsExtractTopLevelType :: StablePtr ValueState' -> IO CInt
hsExtractTopLevelType ptr = deRefStablePtr ptr >>= \case
    ValuePresent (Core.Forall _ t) _ -> return $ case t of
        Core.TNum _ -> 1
        Core.TBool -> 2
        Core.TText -> 3
        Core.TVar _ -> 4
        Core.TList _ -> 5
        Core.TRecord _ -> 6
        Core.TFun _ _ -> 7
        Core.TTable _ -> 8
        Core.TUnit _ -> 0  -- shouldn't occur as a top-level type, treat as absent value if found
    _ -> return 0

hsExtractValue :: StablePtr ValueState' -> IO (StablePtr Value')
hsExtractValue ptr = deRefStablePtr ptr >>= \case
    ValuePresent _ v -> newStablePtr v
    _ -> error "hsExtractValue: attempted to get nonexistent value"

hsRenderValue :: StablePtr Value' -> IO CString
hsRenderValue = GHC.newCString utf8 . render <=< deRefStablePtr

hsValueToList :: StablePtr Value' -> Ptr CInt -> IO (Ptr (StablePtr Value'))
hsValueToList ptr lptr = deRefStablePtr ptr >>= \case
    VList vs -> do
        poke lptr $ genericLength vs
        vs' <- traverse newStablePtr vs
        newArray vs'
    _ -> error "hsValueToList: tried to convert non-list to list"

hsValueToTable
    :: StablePtr Value'                   -- ^ input: table
    -> Ptr CInt                           -- ^ output: number of columns
    -> Ptr (Ptr CString)                  -- ^ output: list of column headings
    -> Ptr (Ptr CInt)                     -- ^ output: list of column lengths
    -> IO (Ptr (Ptr (StablePtr Value')))  -- ^ output: list of columns, each a list of values
hsValueToTable ptr lptr headsptr clsptr = deRefStablePtr ptr >>= \case
    VTable (unzip . Map.toList -> (heads,vs)) -> do
        poke lptr $ genericLength vs
        poke headsptr =<< newArray =<< traverse (GHC.newCString utf8) heads
        (vs', cls) <- fmap unzip $ for vs $ \col -> do
            col' <- newArray =<< traverse newStablePtr col
            pure (col', genericLength col)
        poke clsptr =<< newArray cls
        newArray vs'
    _ -> error "hsValueToTable: tried to convert non-table to table"

hsNullStablePtr :: IO (StablePtr ())
hsNullStablePtr = newStablePtr ()

foreign export ccall hsNewSheet :: IO (StablePtr (IORef Sheet))
foreign export ccall hsParseExpr :: CString -> Ptr CBool -> IO (StablePtr (Expr.AST, Widget))
foreign export ccall hsParseLiteralList :: CInt -> Ptr CString -> Ptr CBool -> IO (StablePtr (Expr.AST, Widget))
foreign export ccall hsParseTable :: CInt -> Ptr CString -> Ptr (Ptr CString) -> Ptr (Ptr CString) -> Ptr CInt -> Ptr (Ptr CString) -> Ptr CBool -> IO (StablePtr (Expr.AST, Widget))
foreign export ccall hsMaybeParseType :: CString -> IO (StablePtr (Maybe Expr.Type))
foreign export ccall hsNothing :: IO (StablePtr (Maybe a))
foreign export ccall hsMkCell :: CString -> StablePtr (Maybe Expr.Type) -> StablePtr (Expr.AST, Widget) -> IO (StablePtr Cell)
foreign export ccall hsInsert :: CInt -> StablePtr Cell -> StablePtr (IORef Sheet) -> IO ()
foreign export ccall hsEvalSheet :: CString -> StablePtr (IORef Sheet) -> IO CBool
foreign export ccall hsCellIndices :: StablePtr (IORef Sheet) -> Ptr CInt -> IO (Ptr CInt)
foreign export ccall hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr ValueState')
foreign export ccall hsQueryCell :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr Cell)
foreign export ccall hsWidgetType :: StablePtr Cell -> IO CInt
foreign export ccall hsWidgetCols :: StablePtr Cell -> IO CInt
foreign export ccall hsWidgetRows :: StablePtr Cell -> IO CInt
foreign export ccall hsExprAt :: CInt -> CInt -> StablePtr Cell -> IO CString
foreign export ccall hsCellName :: StablePtr Cell -> IO CString
foreign export ccall hsCellType :: StablePtr Cell -> IO CString
foreign export ccall hsCellTypeOfColumn :: CString -> StablePtr Cell -> IO CString
foreign export ccall hsParseSheet :: CString -> Ptr CBool -> IO (StablePtr (IORef Sheet))
foreign export ccall hsRenderSheet :: StablePtr (IORef Sheet) -> IO CString
foreign export ccall hsDisplayError :: StablePtr ValueState' -> IO CString
foreign export ccall hsExtractTopLevelType :: StablePtr ValueState' -> IO CInt
foreign export ccall hsExtractValue :: StablePtr ValueState' -> IO (StablePtr Value')
foreign export ccall hsRenderValue :: StablePtr Value' -> IO CString
foreign export ccall hsValueToList :: StablePtr Value' -> Ptr CInt -> IO (Ptr (StablePtr Value'))
foreign export ccall hsValueToTable :: StablePtr Value' -> Ptr CInt -> Ptr (Ptr CString) -> Ptr (Ptr CInt) -> IO (Ptr (Ptr (StablePtr Value')))
foreign export ccall hsNullStablePtr :: IO (StablePtr ())
