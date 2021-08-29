{-# LANGUAGE ForeignFunctionInterface #-}

module Interop where

import Data.IORef
import Foreign
import Foreign.C hiding (newCString, peekCString) -- hide these so we don't accidentally use them
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)

import qualified Data.Map.Strict as Map

import Brassica.Interpret

hsNewSheet :: IO (StablePtr (IORef Sheet))
hsNewSheet = newStablePtr =<< newIORef (Sheet Map.empty)

hsParseExpr :: CString -> Ptr Bool -> IO (StablePtr Expr)
hsParseExpr cinput successPtr = do
    input <- GHC.peekCString utf8 cinput
    case parseMaybe pExpr input of
        Nothing -> poke successPtr False >> newStablePtr zeroExpr
        Just xp -> poke successPtr True  >> newStablePtr xp

hsMaybeParseType :: CString -> IO (StablePtr (Maybe Type))
hsMaybeParseType cinput = do
    input <- GHC.peekCString utf8 cinput
    newStablePtr $ parseMaybe pType input

hsMkCell :: CString -> StablePtr (Maybe Type) -> StablePtr Expr -> IO (StablePtr Cell)
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

hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr Bool -> IO (StablePtr ValueState)
hsQuery k ptr successPtr = do
    ref <- deRefStablePtr ptr
    Sheet s <- readIORef ref
    case Map.lookup (fromIntegral k) s of
        Nothing -> poke successPtr False >> newStablePtr Invalidated
        Just cl -> poke successPtr True  >> newStablePtr (cellValue cl)

hsDisplay :: StablePtr ValueState -> IO CString
hsDisplay ptr = GHC.newCString utf8 =<< display <$> deRefStablePtr ptr

foreign export ccall hsNewSheet :: IO (StablePtr (IORef Sheet))
foreign export ccall hsParseExpr :: CString -> Ptr Bool -> IO (StablePtr Expr)
foreign export ccall hsMaybeParseType :: CString -> IO (StablePtr (Maybe Type))
foreign export ccall hsMkCell :: CString -> StablePtr (Maybe Type) -> StablePtr Expr -> IO (StablePtr Cell)
foreign export ccall hsInsert :: CInt -> StablePtr Cell -> StablePtr (IORef Sheet) -> IO ()
foreign export ccall hsEvalSheet :: StablePtr (IORef Sheet) -> IO () 
foreign export ccall hsQuery :: CInt -> StablePtr (IORef Sheet) -> Ptr Bool -> IO (StablePtr ValueState)
foreign export ccall hsDisplay :: StablePtr ValueState -> IO CString
