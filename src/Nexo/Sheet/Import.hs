{-# LANGUAGE NamedFieldPuns #-}

module Nexo.Sheet.Import (getImport, toCellMap) where

import Control.Monad.Except (ExceptT(..), throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), (<.>), dropFileName)

import Nexo.Core.Type (PType)
import Nexo.Error (Error(..))
import Nexo.Interpret (Value)
import Nexo.Sheet
import Nexo.Sheet.Parse (parseSheet)
import Data.Traversable (for)
import Debug.Trace

data ImportError
    = NonexistentSheet String
    | ErrorInImport String Error
    deriving (Show)

getImport :: Maybe FilePath -> String -> ExceptT ImportError IO (Map.Map String (PType, Value GlobalEnv))
getImport curSheetDir wanted = do
    searchPath <- toSearchPath <$> liftIO getExecutablePath
    traceShowM searchPath
    found <- firstForElse (throwError $ NonexistentSheet wanted) searchPath $ \dir -> do
        let fullPath = dir </> wanted <.> ".nexo"
        liftIO $ tag fullPath <$> doesFileExist fullPath
    contents <- liftIO $ readFile found
    case parseSheet contents of
        Nothing -> throwError $ ErrorInImport wanted ParseError
        Just sheet -> toCellMap wanted =<< evalSheet (getImport curSheetDir) sheet
  where
    toSearchPath :: FilePath -> [FilePath]
    toSearchPath exeName =
        let libDir = dropFileName exeName </> "lib"
        in maybe id (:) curSheetDir [libDir]

    firstForElse :: Monad m => m b -> [a] -> (a -> m (Maybe b)) -> m b
    firstForElse err [] _ = err
    firstForElse err (a:as) m = m a >>= maybe (firstForElse err as m) pure

    tag :: a -> Bool -> Maybe a
    tag _ False = Nothing
    tag a True = Just a

toCellMap
    :: Monad m
    => String
    -> Sheet
    -> ExceptT ImportError m (Map.Map String (PType, Value GlobalEnv))
toCellMap wanted (Sheet _ _ s) = fmap Map.fromList $
    for (Map.elems s) $ \Cell{cellName, cellValue} ->
        case cellValue of
            ValuePresent ty val -> pure (cellName, (ty, val))
            ValueError _ e -> throwError $ ErrorInImport wanted e
            Invalidated -> error "toCellMap: bug in evaluator"
