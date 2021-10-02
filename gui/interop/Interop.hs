{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module Interop where

import Control.Monad ((<=<))
import Data.Fix (Fix(Fix))
import Data.IORef
import Data.List (genericLength)
import Foreign
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)

import qualified Data.Map.Strict as Map

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Sheet

hsNewSheet :: IO (StablePtr (IORef Sheet))
hsNewSheet = newStablePtr =<< newIORef (Sheet Map.empty)

cFalse, cTrue :: CBool
cFalse = 0
cTrue = 1

hsParseExpr :: CString -> Ptr CBool -> IO (StablePtr Expr)
hsParseExpr cinput successPtr = do
    input <- GHC.peekCString utf8 cinput
    case parseMaybe pExpr input of
        Nothing -> poke successPtr cFalse >> newStablePtr zeroExpr
        Just xp -> poke successPtr cTrue  >> newStablePtr xp

hsParseLiteralList :: CInt -> Ptr CString -> Ptr CBool -> IO (StablePtr Expr)
hsParseLiteralList clen cinput successPtr = do
    cinputs <- peekArray (fromIntegral clen) cinput
    inputs <- traverse (GHC.peekCString utf8) cinputs
    case traverse (parseMaybe pValue) inputs of
        Nothing -> poke successPtr cFalse >> newStablePtr zeroExpr
        Just values ->
            let xp = Fix . XList $ Fix . XLit <$> values
            in poke successPtr cTrue >> newStablePtr xp

hsMaybeParseType :: CString -> IO (StablePtr (Maybe PType))
hsMaybeParseType cinput = do
    input <- GHC.peekCString utf8 cinput
    newStablePtr $ parseMaybe pPType input

hsMkCell :: CString -> StablePtr (Maybe PType) -> StablePtr Expr -> IO (StablePtr Cell)
hsMkCell cname tptr xptr = do
    name <- GHC.peekCString utf8 cname
    type_ <- deRefStablePtr tptr
    expr  <- deRefStablePtr xptr
    newStablePtr $ Cell name type_ expr Invalidated

hsInsert :: CInt -> StablePtr Cell -> StablePtr (IORef Sheet) -> IO ()
hsInsert k cptr sptr = do
    cell <- deRefStablePtr cptr
    sref <- deRefStablePtr sptr
    modifyIORef' sref $ insert (fromIntegral k) cell

hsEvalSheet :: StablePtr (IORef Sheet) -> IO ()
hsEvalSheet ptr = do
    ref <- deRefStablePtr ptr
    modifyIORef' ref evalSheet

hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr ValueState)
hsQuery k ptr successPtr = do
    ref <- deRefStablePtr ptr
    Sheet s <- readIORef ref
    case Map.lookup (fromIntegral k) s of
        Nothing -> poke successPtr cFalse >> newStablePtr Invalidated
        Just cl -> poke successPtr cTrue  >> newStablePtr (cellValue cl)

hsDisplayError :: StablePtr ValueState -> IO CString
hsDisplayError ptr = deRefStablePtr ptr >>= \case
    ValuePresent _ _ -> GHC.newCString utf8 ""
    vs -> GHC.newCString utf8 $ display vs

hsExtractTopLevelType :: StablePtr ValueState -> IO CInt
hsExtractTopLevelType ptr = deRefStablePtr ptr >>= \case
    ValuePresent (Forall _ _ t) _ -> return $ case t of
        TNum _ -> 1
        TBool -> 2
        TText -> 3
        TVar _ -> 4
        TList _ -> 5
        TRecord _ -> 6
        TFun _ _ -> 7
    _ -> return 0

hsExtractValue :: StablePtr ValueState -> IO (StablePtr Value)
hsExtractValue ptr = deRefStablePtr ptr >>= \case
    ValuePresent _ v -> newStablePtr v
    _ -> error "hsExtractValue: attempted to get nonexistent value"

hsRenderValue :: StablePtr Value -> IO CString
hsRenderValue = GHC.newCString utf8 . render <=< deRefStablePtr

hsValueToList :: StablePtr Value -> Ptr CInt -> IO (Ptr (StablePtr Value))
hsValueToList ptr lptr = deRefStablePtr ptr >>= \case
    VList vs -> do
        poke lptr $ genericLength vs
        vs' <- traverse newStablePtr vs
        newArray vs'
    _ -> error "hsValueToList: tried to convert non-list to list"

hsNullStablePtr :: IO (StablePtr ())
hsNullStablePtr = newStablePtr ()

foreign export ccall hsNewSheet :: IO (StablePtr (IORef Sheet))
foreign export ccall hsParseExpr :: CString -> Ptr CBool -> IO (StablePtr Expr)
foreign export ccall hsParseLiteralList :: CInt -> Ptr CString -> Ptr CBool -> IO (StablePtr Expr)
foreign export ccall hsMaybeParseType :: CString -> IO (StablePtr (Maybe PType))
foreign export ccall hsMkCell :: CString -> StablePtr (Maybe PType) -> StablePtr Expr -> IO (StablePtr Cell)
foreign export ccall hsInsert :: CInt -> StablePtr Cell -> StablePtr (IORef Sheet) -> IO ()
foreign export ccall hsEvalSheet :: StablePtr (IORef Sheet) -> IO () 
foreign export ccall hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr CBool -> IO (StablePtr ValueState)
foreign export ccall hsDisplayError :: StablePtr ValueState -> IO CString
foreign export ccall hsExtractTopLevelType :: StablePtr ValueState -> IO CInt
foreign export ccall hsExtractValue :: StablePtr ValueState -> IO (StablePtr Value)
foreign export ccall hsRenderValue :: StablePtr Value -> IO CString
foreign export ccall hsValueToList :: StablePtr Value -> Ptr CInt -> IO (Ptr (StablePtr Value))
foreign export ccall hsNullStablePtr :: IO (StablePtr ())
