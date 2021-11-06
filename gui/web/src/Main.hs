{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Monad.Fix (MonadFix)
import Data.String (IsString(fromString))
import Data.Text (Text, pack, unpack)
import Text.Lucius (renderCss, lucius)

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Sheet

import Reflex.Dom hiding (display)
import Data.Maybe (fromMaybe)

cell ::
    ( DomBuilder t m
    , MonadHold t m
    , PostBuild t m
    )
    => Dynamic t Text
    -> m (Dynamic t Cell)
cell valueDyn = elClass "form" "cell" $ do
    label "Name"
    iName <- inputElement def
    br
    label "Type"
    iType <- inputElement def
    br
    label "Expr"
    iExpr <- inputElement def
    br
    label "Value"
    dynText valueDyn

    let nameEv = _inputElement_input iName
        typeEv = _inputElement_input iType
        exprEv = _inputElement_input iExpr

    let typeParsedEv = parseMaybe pPType . unpack <$> typeEv
        exprParsedEv = mapMaybe (parseMaybe pExpr . unpack) exprEv

    nameDyn <- holdDyn "" nameEv
    typeDyn <- holdDyn Nothing typeParsedEv
    exprDyn <- holdDyn zeroExpr exprParsedEv

    return $ Cell <$> (unpack <$> nameDyn) <*> typeDyn <*> exprDyn <*> pure Invalidated
  where
    label = el "label" . text
    br = el "br" blank

sheet :: forall t m.
    ( DomBuilder t m
    , MonadHold t m
    , MonadFix m
    , PostBuild t m
    , Reflex t
    ) => m ()
sheet = do
    addEv <- button "Add"
    el "br" blank

    cellsCurIx <- count addEv

    rec
        let -- partial: assumes the identifier is valid
            getCellValue :: Int -> Dynamic t Text
            getCellValue ident = ffor sheetDyn $ \(Sheet s) ->
                pack $ fromMaybe "" $ display . cellValue <$> Map.lookup ident s

        cellsDyn <- listHoldWithKey Map.empty (mkDiff <$> updated cellsCurIx) $
            \i _ -> cell (getCellValue i)
        let cellsEv = switchDyn $ updateds <$> cellsDyn
        sheetDyn <- foldDyn (\v -> evalSheet . uncurry insert v) (Sheet Map.empty) cellsEv
    blank
  where
    assoc :: Functor f => (c, f a) -> f (c, a)
    assoc (i, fa) = (i,) <$> fa

    -- | Given a map of 'Dynamic's, return an 'Event' which fires
    -- whenever one of the 'Dynamic' fires, labelling with the key
    updateds :: Map.Map c (Dynamic t a) -> Event t (c, a)
    updateds = leftmost . fmap (updated . assoc) . Map.toList

    mkDiff :: Int -> Map.Map Int (Maybe ())
    mkDiff = flip Map.singleton (Just ())

style :: IsString s => s
style = fromString $ TL.unpack $ renderCss $ ($ undefined) [lucius|
.cell {
    display: inline-block;
    border: 1px solid;
}
|]

main :: IO ()
main = mainWidgetWithCss style sheet
