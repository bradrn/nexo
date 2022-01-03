{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Interop where

import Control.Monad ((<=<), zipWithM)
import Control.Monad.Free
import Data.Fix (Fix(Fix))
import Data.IORef
import Data.List (genericLength)
import Data.Traversable (for)
import Foreign
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Interpret (Value(..), render)
import Nexo.Sheet

hsNewSheet :: IO (StablePtr (IORef Sheet))
hsNewSheet = newStablePtr =<< newIORef (Sheet Map.empty)

cFalse, cTrue :: CBool
cFalse = 0
cTrue = 1

hsParseExpr :: CString -> Ptr CBool -> IO (StablePtr (Expr, Free ExprF String))
hsParseExpr cinput successPtr = do
    input <- GHC.peekCString utf8 cinput
    case parseMaybe pExpr input of
        Nothing -> poke successPtr cFalse >> newStablePtr (Fix XNull, Free XNull)
        Just xp -> poke successPtr cTrue  >> newStablePtr (xp, partialise xp)

hsParseLiteralList :: CInt -> Ptr CString -> Ptr CBool -> IO (StablePtr (Expr, Free ExprF String))
hsParseLiteralList clen cinput successPtr = do
    cinputs <- peekArray (fromIntegral clen) cinput
    inputs <- traverse (GHC.peekCString utf8) cinputs
    case traverse (parseMaybe pLit) inputs of
        Nothing -> poke successPtr cFalse >> newStablePtr (Fix XNull, Free XNull)
        Just values ->
            let xp  = Fix  . XList $ Fix . XLit <$> values
                xpf = Free . XList $ Pure <$> inputs
            in poke successPtr cTrue >> newStablePtr (xp, xpf)

hsParseTable :: CInt -> Ptr CString -> Ptr (Ptr CString) -> Ptr CInt -> Ptr (Ptr CString) -> Ptr CBool -> IO (StablePtr (Expr, Free ExprF String))
hsParseTable clen cheader cformula ccollen ccol successPtr = do
    cheaders <- peekArray (fromIntegral clen) cheader
    headers <- traverse (GHC.peekCString utf8) cheaders

    cformulae <- peekArray (fromIntegral clen) cformula
    formulae <- traverse (traverse (GHC.peekCString utf8) <=< ptrToMaybe) cformulae
    let formulaeExprs = fmap (>>= parseMaybe pExpr) formulae

    ccollens <- peekArray (fromIntegral clen) ccollen
    ccols <- peekArray (fromIntegral clen) ccol
    ccolss <- traverse (uncurry peekArray) $ zip (fromIntegral <$> ccollens) ccols
    colss <- (traverse.traverse) (GHC.peekCString utf8) ccolss
    let colExprss = (fmap.traverse) (parseMaybe pExpr) colss

    let columns'        = zipWithM mkColumnExpr formulaeExprs colExprss
        columnsPartial' = zipWithM mkColumnPartialExpr formulae colss

    case (,) <$> columns' <*> columnsPartial' of
        Nothing -> poke successPtr cFalse >> newStablePtr (Fix XNull, Free XNull)
        Just (columns, columnsPartial) ->
            let xp  = Fix  $ XTable $ Fix  $ XRecord Recursive (Map.fromList $ zip headers columns) headers
                xpf :: Free ExprF String
                xpf = Free $ XTable $ Free $ XRecord Recursive (Map.fromList $ zip headers columnsPartial) headers
            in poke successPtr cTrue >> newStablePtr (xp, xpf)
  where
    ptrToMaybe :: Storable a => Ptr a -> IO (Maybe a)
    ptrToMaybe p
        | p == nullPtr = pure Nothing
        | otherwise    = Just <$> peek p

    mkColumnExpr :: Maybe Expr -> Maybe [Expr] -> Maybe Expr
    mkColumnExpr (Just x) _         = Just x
    mkColumnExpr _       (Just col) = Just $ Fix $ XList col
    mkColumnExpr Nothing Nothing    = Nothing

    mkColumnPartialExpr :: Maybe String -> [String] -> Maybe (Free ExprF String)
    mkColumnPartialExpr (Just x) _   = Just (Pure x)
    mkColumnPartialExpr _        col = Just $ Free $ XList (Pure <$> col)

hsMaybeParseType :: CString -> IO (StablePtr (Maybe PType))
hsMaybeParseType cinput = do
    input <- GHC.peekCString utf8 cinput
    newStablePtr $ parseMaybe pPType input

hsNothing :: IO (StablePtr (Maybe a))
hsNothing = newStablePtr Nothing

hsMkCell :: CString -> CInt -> StablePtr (Maybe PType) -> StablePtr (Expr, Free ExprF String) -> IO (StablePtr Cell)
hsMkCell cname cwidget tptr xptr = do
    name <- GHC.peekCString utf8 cname
    type_ <- deRefStablePtr tptr
    (expr, raw) <- deRefStablePtr xptr
    let w = case cwidget of
            0 -> ValueCell
            1 -> InputList
            2 -> Table
            _ -> error "hsMkCell: unknown widget type"
    newStablePtr $ Cell name type_ (Just raw) expr Invalidated w

hsInsert :: CInt -> StablePtr Cell -> StablePtr (IORef Sheet) -> IO ()
hsInsert k cptr sptr = do
    cell <- deRefStablePtr cptr
    sref <- deRefStablePtr sptr
    modifyIORef' sref $ insert (fromIntegral k) cell

hsEvalSheet :: StablePtr (IORef Sheet) -> IO ()
hsEvalSheet ptr = do
    ref <- deRefStablePtr ptr
    modifyIORef' ref evalSheet

hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr ValueState')
hsQuery k ptr successPtr = do
    ref <- deRefStablePtr ptr
    Sheet s <- readIORef ref
    case Map.lookup (fromIntegral k) s of
        Nothing -> poke successPtr cFalse >> newStablePtr Invalidated
        Just cl -> poke successPtr cTrue  >> newStablePtr (cellValue cl)

hsDisplayError :: StablePtr ValueState' -> IO CString
hsDisplayError ptr = deRefStablePtr ptr >>= \case
    ValuePresent _ _ -> GHC.newCString utf8 ""
    vs -> GHC.newCString utf8 $ display vs

hsExtractTopLevelType :: StablePtr ValueState' -> IO CInt
hsExtractTopLevelType ptr = deRefStablePtr ptr >>= \case
    ValuePresent (Forall _ _ t) _ -> return $ case t of
        TNum _ -> 1
        TBool -> 2
        TText -> 3
        TVar _ -> 4
        TList _ -> 5
        TRecord _ -> 6
        TFun _ _ -> 7
        TTable _ -> 8
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
foreign export ccall hsParseExpr :: CString -> Ptr CBool -> IO (StablePtr (Expr, Free ExprF String))
foreign export ccall hsParseLiteralList :: CInt -> Ptr CString -> Ptr CBool -> IO (StablePtr (Expr, Free ExprF String))
foreign export ccall hsParseTable :: CInt -> Ptr CString -> Ptr (Ptr CString)-> Ptr CInt -> Ptr (Ptr CString) -> Ptr CBool -> IO (StablePtr (Expr, Free ExprF String))
foreign export ccall hsMaybeParseType :: CString -> IO (StablePtr (Maybe PType))
foreign export ccall hsNothing :: IO (StablePtr (Maybe a))
foreign export ccall hsMkCell :: CString -> CInt -> StablePtr (Maybe PType) -> StablePtr (Expr, Free ExprF String) -> IO (StablePtr Cell)
foreign export ccall hsInsert :: CInt -> StablePtr Cell -> StablePtr (IORef Sheet) -> IO ()
foreign export ccall hsEvalSheet :: StablePtr (IORef Sheet) -> IO () 
foreign export ccall hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr ValueState')
foreign export ccall hsDisplayError :: StablePtr ValueState' -> IO CString
foreign export ccall hsExtractTopLevelType :: StablePtr ValueState' -> IO CInt
foreign export ccall hsExtractValue :: StablePtr ValueState' -> IO (StablePtr Value')
foreign export ccall hsRenderValue :: StablePtr Value' -> IO CString
foreign export ccall hsValueToList :: StablePtr Value' -> Ptr CInt -> IO (Ptr (StablePtr Value'))
foreign export ccall hsValueToTable :: StablePtr Value' -> Ptr CInt -> Ptr (Ptr CString) -> Ptr (Ptr CInt) -> IO (Ptr (Ptr (StablePtr Value')))
foreign export ccall hsNullStablePtr :: IO (StablePtr ())
