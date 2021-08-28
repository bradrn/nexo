{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Data.Bool (bool)
import Data.IORef
import Graphics.UI.Threepenny.Core hiding (many)

import qualified Data.Map.Strict as Map
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (transpose, sort)

import Brassica.Interpret

readonly :: WriteAttr Element Bool
readonly = mkWriteAttr $ set' (attr "readonly") . bool "0" "1"

wCell :: Behavior String -> Int -> UI (Element, Event (Int, String, Maybe Type, Expr))
wCell bValue identifier = do
    iName <- UI.input
    iType <- UI.input
    iExpr <- UI.input
    iValue <- UI.input
        # set readonly True
        # sink value bValue

    iResult <- grid
        [ [string "Name" , element iName]
        , [string "Type" , element iType]
        , [string "Expr" , element iExpr]
        , [string "Value", element iValue]
        ]

    bName <- stepper "" $ UI.valueChange iName
    bType <- stepper "" $ UI.valueChange iType
    bExpr <- stepper "" $ UI.valueChange iExpr

    let bTypeParsed = parseMaybe pType <$> bType
        bExprParsed = parseMaybe pExpr <$> bExpr

    let eBlur = unions $ UI.blur <$> [iName, iType, iExpr]
        eResult = whenMaybe $ (identifier,,,)
            <$> bName
            <*> bTypeParsed
            <*> bExprParsed
            <@ eBlur

    return (iResult, eResult)
  where
    whenMaybe = filterJust . fmap (\(i, n, t, x) -> (i, n, t,) <$> x)

wSheet :: UI Element
wSheet = do
    iCellsRef <- liftIO $ newIORef []
    container <- UI.div

    iAdd <- UI.button # set UI.text "Add"

    let updateChildren :: ([Element] -> [Element]) -> UI ()
        updateChildren f = do
            liftIO $ modifyIORef' iCellsRef f
            iCells <- liftIO $ readIORef iCellsRef
            layedout <- row $ element <$> iCells
            _ <- element container # set children [layedout]
            pure ()

    trValuesRef <- liftIO $ newIORef Map.empty
    (eCells, trCells) <- liftIO newEvent

    curIdent <- liftIO $ newIORef 0
    on UI.click iAdd $ \_ -> do
        (ebCell, trbCell) <- liftIO newEvent
        bCell <- stepper "" ebCell

        curIdent' <- liftIO $ readIORef curIdent

        (iCell, eCell) <- wCell bCell curIdent'

        updateChildren (++ [iCell])

        liftIO $ do
            modifyIORef' trValuesRef $ Map.insert curIdent' trbCell
            modifyIORef' curIdent (+1)
        onEvent eCell $ liftIO . trCells

    let updateSheet :: (Int, String, Maybe Type, Expr) -> Sheet -> Sheet
        updateSheet (ident, name, type_, expr) s =
            evalSheet $ insert ident (Cell name type_ expr Invalidated) s

    eSheet <- accumE (Sheet Map.empty) $ updateSheet <$> eCells
    _ <- onEvent eSheet $ \(Sheet s) -> liftIO $ do
        trValues <- readIORef trValuesRef
        flip Map.traverseWithKey s $ \ident cell ->
            case Map.lookup ident trValues of
                Just trValue -> trValue (display $ cellValue cell)
                Nothing -> pure ()

    column [element iAdd, element container]

main :: IO ()
main = startGUI defaultConfig $ \window -> do
    iSheet <- wSheet
    _ <- getBody window #+ [element iSheet]
    pure ()
