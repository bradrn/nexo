{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Applicative (liftA2)
import Control.Monad.Fix (MonadFix)
#if MIN_VERSION_recursion_schemes(5,2,0)
import Data.Fix (Fix(..))
#else
import Data.Functor.Foldable (Fix(..))
#endif
import Data.Foldable (traverse_)
import Data.String (IsString(fromString))
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Lucius (renderCss, lucius)

import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL

import Nexo.Expr.Parse
import Nexo.Expr.Type
import Nexo.Interpret (Value(..), render)
import Nexo.Sheet

import Reflex.Dom hiding (display, Value)

label :: DomBuilder t m => Text -> m ()
label = el "label" . text

br :: DomBuilder t m => m ()
br = el "br" blank

assoc :: Functor f => (c, f a) -> f (c, a)
assoc (i, fa) = (i,) <$> fa

inputElementList ::
    ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , Reflex t
    ) => m (Dynamic t [Text])
inputElementList = mdo
    elems <- listHoldWithKey (0 =: ()) (updated elemsDiff) $ \_ _ -> do
        iElem <- inputElement def <* br
        let elemValue = _inputElement_value iElem
        elemEdited <- holdDyn False =<< headE (True <$ updated elemValue)
        pure (elemValue, elemEdited)

    let listValuesNested = Map.elems <$> elems
        listValues = fmap (mapMaybe removeUnedited) . traverse (uncurry $ liftA2 (,)) =<< listValuesNested

        listEditedNested = Map.toList . fmap snd <$> elems
    listUneditedIx <- holdUniqDyn $ listEditedNested >>= fmap findUneditedIx . traverse assoc
    let elemsDiff = mkDiff <$> listUneditedIx

    return listValues
  where
    removeUnedited :: (a, Bool) -> Maybe a
    removeUnedited (_, False) = Nothing
    removeUnedited (a, True ) = Just a

    findUneditedIx :: Num a => [(a, Bool)] -> a
    findUneditedIx ((i,False):_) = i
    findUneditedIx [(i,True)] = i+1
    findUneditedIx ((_,True):xs) = findUneditedIx xs
    findUneditedIx [] = error "findUneditedIx: empty list"

    mkDiff :: Int -> Map.Map Int (Maybe ())
    mkDiff = flip Map.singleton (Just ())

inputList ::
    ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PostBuild t m
    )
    => m (Dynamic t Cell)
inputList = elClass "form" "cell" $ do
    label "Name"
    iName <- inputElement def
    br
    listDyn <- inputElementList

    let nameDyn = _inputElement_value iName
        exprParsedEv = Fix . XList <$> mapMaybe (traverse (parseMaybe pExpr . unpack)) (updated listDyn)

    exprDyn <- holdDyn zeroExpr exprParsedEv

    return $ Cell <$> (unpack <$> nameDyn) <*> constDyn Nothing <*> exprDyn <*> pure Invalidated

table ::
    forall m t e.
    ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PostBuild t m
    )
    => Dynamic t (Maybe (ValueState e))
    -> m (Dynamic t Cell)
table valueDyn = do
    label "Table Name"
    iName <- inputElement def
    let nameDyn = _inputElement_value iName
    br

    divClass "column" $ do
        label "Name" >> br
        label "Formula" >> br
        label "Value"

    rec -- only so addEv can be defined later
        cellsCurIx <- updated <$> count @_ @_ @Int addEv
        columnsDynNested <- listHoldWithKey Map.empty ((=: Just ()) <$> cellsCurIx) $
            \_ _ -> singleColumn >>= holdDyn ("", zeroExpr)
        let columnsDyn = joinDynThroughMap columnsDynNested
        addEv <- button "Add"
    pure $ mkCell <$> nameDyn <*> columnsDyn
  where
    mkCell :: Text -> Map.Map a (Text, Expr) -> Cell
    mkCell name columns =
        let columns' = Map.mapKeys unpack $ Map.fromList $ Map.elems columns
        in Cell
        { cellName = unpack name
        , cellType = Nothing -- TODO: handle types properly
        , cellExpr = Fix $ XTable columns'
        , cellValue = Invalidated
        }

    singleColumn ::
        ( DomBuilder t m
        , MonadFix m
        , MonadHold t m
        , PostBuild t m
        )
        => m (Event t (Text, Expr))
    singleColumn = divClass "column" $ do
        iName <- inputElement def
        br
        let nameDyn = _inputElement_value iName

        -- label "Type"
        -- iType <- inputElement def
        -- br

        iFormula <- inputElement def
        let formulaDyn = _inputElement_value iFormula
            noFormula = T.null <$> formulaDyn
            formulaParsedDyn =
                mapMaybe (parseMaybe pExpr . unpack) $ updated formulaDyn
        contentWidgetNestedEv <- dyn $ ffor noFormula $ \case
            True -> do
                br
                listDyn <- inputElementList
                let exprParsedEv = Fix . XList <$> mapMaybe (traverse (parseMaybe pExpr . unpack)) (updated listDyn)
                exprDyn <- holdDyn zeroExpr exprParsedEv
                pure $ updated $ (,) <$> nameDyn <*> exprDyn
            False -> do
                valueDisplay $ ffor (zipDyn nameDyn valueDyn) $ \case
                    (name, Just (ValuePresent pt (VTable tbl))) ->
                        ValuePresent pt . VList <$> Map.lookup (unpack name) tbl
                    _ -> Nothing
                pure $ attach (current nameDyn) formulaParsedDyn
        switchHold never contentWidgetNestedEv

valueDisplay
    :: (Adjustable t m, DomBuilder t m, PostBuild t m)
    => Dynamic t (Maybe (ValueState a)) -> m ()
valueDisplay = dyn_ . fmap (maybe blank errorOrGo)
  where
    errorOrGo (ValuePresent _ v) = go v
    errorOrGo e = text $ pack $ display e

    go (VList vs) = traverse_ ((br<*) . go) vs
    go v = text $ pack $ render v

cell ::
    ( DomBuilder t m
    , MonadHold t m
    , PostBuild t m
    )
    => Dynamic t (Maybe (ValueState a))
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
    valueDisplay valueDyn

    let nameEv = _inputElement_input iName
        typeEv = _inputElement_input iType
        exprEv = _inputElement_input iExpr

    let typeParsedEv = parseMaybe pPType . unpack <$> typeEv
        exprParsedEv = mapMaybe (parseMaybe pExpr . unpack) exprEv

    nameDyn <- holdDyn "" nameEv
    typeDyn <- holdDyn Nothing typeParsedEv
    exprDyn <- holdDyn zeroExpr exprParsedEv

    return $ Cell <$> (unpack <$> nameDyn) <*> typeDyn <*> exprDyn <*> pure Invalidated

data WidgetType = CellWidget | InputList | TableWidget

sheet :: forall t m.
    ( DomBuilder t m
    , MonadHold t m
    , MonadFix m
    , PostBuild t m
    , Reflex t
    ) => m ()
sheet = do
    addEv <- button "Add"
    (cellTypeSelect, _) <- selectElement def {
            _selectElementConfig_initialValue = "cell"
        } $ do
        elAttr "option" ("value" =: "cell") $ text "Cell"
        elAttr "option" ("value" =: "inputList") $ text "Input list"
        elAttr "option" ("value" =: "table") $ text "Table"
    let cellTypeSelectValue = current $ _selectElement_value cellTypeSelect
    el "br" blank

    cellsCurIx <- updated <$> count addEv
    let cellsDiffEv = ffor (attach cellTypeSelectValue cellsCurIx) $ \case
            ("cell"     , i) -> Map.singleton i $ Just CellWidget
            ("inputList", i) -> Map.singleton i $ Just InputList
            ("table"    , i) -> Map.singleton i $ Just TableWidget
            _ -> error "sheet: unknown widget type"

    rec
        let getCellValue :: Int -> Dynamic t (Maybe (ValueState (ValueEnv Eval)))
            getCellValue ident = ffor sheetDyn $ \(Sheet s) ->
                cellValue <$> Map.lookup ident s

        cellsDyn <- listHoldWithKey Map.empty cellsDiffEv $ \i -> \case
            CellWidget  -> cell  (getCellValue i)
            TableWidget -> table (getCellValue i)
            InputList  -> inputList
        let cellsEv = switchDyn $ updateds <$> cellsDyn
        sheetDyn <- foldDyn (\v -> evalSheet . uncurry insert v) (Sheet Map.empty) cellsEv
    blank
  where
    -- | Given a map of 'Dynamic's, return an 'Event' which fires
    -- whenever one of the 'Dynamic' fires, labelling with the key
    updateds :: Map.Map c (Dynamic t a) -> Event t (c, a)
    updateds = leftmost . fmap (updated . assoc) . Map.toList

style :: IsString s => s
style = fromString $ TL.unpack $ renderCss $ ($ undefined) [lucius|
.cell {
    display: inline-block;
    border: 1px solid;
    vertical-align: top;
}
.column {
    display: inline-block;
    vertical-align: top;
}
|]

main :: IO ()
main = mainWidgetWithCss style sheet
