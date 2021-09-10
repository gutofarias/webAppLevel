module MyCharts exposing (..)

import Chart as C
import Chart.Attributes as CA
import DataConvert as DC exposing (..)

import Html exposing (Html,div,text,input,option,span,label,select,button)
import Html.Attributes exposing (style, placeholder, value, selected)
import Html.Events exposing (onInput,onClick)

import Element as E
import Element.Border as EB
import Element.Font as EF
import Element.Input as EI
import UI

import MyCharts.Chart as MChart
import MyCharts.Chart.Curve as Curve


------------------------------------------------
-- Model
------------------------------------------------

type alias Model = List MChart.Model

------------------------------------------------
-- init
------------------------------------------------
    
init : Model
init =
    [MChart.init Nothing]

------------------------------------------------
-- Msg
------------------------------------------------

type Msg
    = AddChart MChart.ChartID
    | RemoveChart MChart.ChartID
    | ChartMsg MChart.ChartID MChart.Msg
      
      
------------------------------------------------
-- update
------------------------------------------------
      
update : Msg -> Model -> Model 
update msg model =
    case msg of
        AddChart chartID ->
            let 
                newChartParam = MChart.addModel chartID
                newChartsParam =  newChartParam::model 
            in
                List.sortBy (\cp -> cp.chartID) newChartsParam

        RemoveChart chartID ->
            List.filter (\chartModel ->
                            not <| (.chartID chartModel) == chartID) model

        ChartMsg chartID chartMsg ->
            List.map 
                (\chartModel ->
                     if (chartModel.chartID == chartID) then
                         MChart.update chartMsg chartModel
                     else
                         chartModel)
                    model


------------------------------------------------
-- view
------------------------------------------------

view : DC.ChartData -> MChart.Model -> (Msg -> msg) -> E.Element msg
view chartData chartModel msgToMainMsg =
    let
        curves = .curves chartModel
        chartID = .chartID chartModel
        editingCurves = .editingCurves chartModel
    in
        UI.element [] <|
        case editingCurves of
            True ->
                E.column [E.centerX, E.spacing  20, E.width E.fill]
                    ([ E.row [E.width E.fill]
                           [ removeChartButton chartID msgToMainMsg
                           , buttonToggleChart editingCurves (msgToMainMsg << (ChartMsg chartID))
                           ]
                    ]
                    ++ MChart.viewCurves chartData (msgToMainMsg << (ChartMsg chartID)) curves)

            False -> 
                E.column [E.centerX, E.width E.fill, E.height E.fill]
                    [ E.row [E.width E.fill]
                           [ removeChartButton chartID msgToMainMsg
                           , buttonToggleChart editingCurves (msgToMainMsg << (ChartMsg chartID))
                           ]
                    , E.el[E.centerX, E.width <| E.px 400] <| E.html <| MChart.view chartModel (msgToMainMsg << (ChartMsg chartID)) chartData curves
                    ]
                
                    
buttonToggleChart : Bool -> (MChart.Msg -> msg) -> E.Element msg
buttonToggleChart editingCurves chartMsgToMainMsg =
    let
        text = case editingCurves of
            True -> "Show Charts"
            False -> "Edit Curves"
    in
        EI.button [E.alignRight]
            { onPress = Just (chartMsgToMainMsg MChart.ToggleEditCurve)
            , label = E.el [] <| E.text text
            } 

removeChartButton : MChart.ChartID -> (Msg -> msg) -> E.Element msg
removeChartButton chartID msgToMainMsg = 
    EI.button [E.alignLeft]
        { onPress = Just (msgToMainMsg <| RemoveChart chartID)
        , label = E.el [] <| E.text "Remove Chart"
        } 
        
        
------------------------------------------------
-- Auxiliary Functions
------------------------------------------------

chartsTuple : Model -> ChartData -> (Msg -> msg) -> List (E.Element msg, E.Element msg)
chartsTuple model chartData msgToMainMsg =
    let
        maxChartID = maxChartIDFromListCharts model
        listMaybe = fillWithMaybe model 1
        newListMaybe = 
            if (modBy 2 maxChartID == 0) then
                listMaybe ++ [Nothing,Nothing]
            else 
                listMaybe ++ [Nothing]

        listElement = listMaybeChartToListElement newListMaybe chartData msgToMainMsg 
    in 
        listToTuple listElement
        
            
listMaybeChartToListElement : List (Maybe MChart.Model) -> ChartData -> (Msg -> msg) -> List (E.Element msg)
listMaybeChartToListElement listMaybe chartData msgToMainMsg =
    let
        indexedFunc index maybeChart = 
            case maybeChart of
                Just chartModel ->
                    view chartData chartModel msgToMainMsg
                Nothing ->
                    UI.addNewElementSpace (msgToMainMsg (AddChart (index+1)))
    in 
    List.indexedMap indexedFunc listMaybe
        
   
listToTuple : List (E.Element msg) -> List (E.Element msg,E.Element msg)
listToTuple listElement =
    case listElement of
        (a::b::xs) -> (a,b)::listToTuple xs
        _ -> []

                       
maxChartIDFromListCharts : Model -> Int
maxChartIDFromListCharts model =
    List.foldr (\cp acc -> max cp.chartID acc) 0 model 


fillWithMaybe : Model -> Int -> List (Maybe MChart.Model)
fillWithMaybe model index = 
    case model of
        [] -> []
        (x::xs) -> 
            let
                chartID = .chartID x
                newIndex = index + 1
            in
                if (index == chartID) then
                    (Just x)::(fillWithMaybe xs newIndex)
                else if (index > chartID) then
                    []
                else
                    Nothing ::(fillWithMaybe model newIndex)
