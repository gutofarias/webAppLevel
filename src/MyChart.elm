module MyChart exposing (..)

import Chart as C
import Chart.Attributes as CA
import DataConvert as DC exposing (..)

import Html exposing (Html,div,text,input,option,span,label,select,button)
import Html.Attributes exposing (style, placeholder, value, selected)
import Html.Events exposing (onInput,onClick)

------------------------------------------------
-- Data Types Charts
------------------------------------------------

type alias AxisFunc data = data -> Float
type alias AxesFunc data =
    (AxisFunc data, AxisFunc data)

type alias Curve data =
    { curveID : CurveID
    , axesFunc : AxesFunc data
    }

-- initCurve : Maybe (Curve DC.ChartDatum) -> Curve DC.ChartDatum
-- initCurve maybeLastCurve =
--     case maybeLastCurve of
--         Nothing ->
--             {curveID = 1, axesFunc = (t,x1)}
--             -- {curveID = 1, axesFunc = (t,t)}
--         Just lastCurve ->
--             let
--                 lastCurveID = .curveID lastCurve
--                 curveID = lastCurveID + 1
--             in
--                 {curveID = curveID, axesFunc = (t,x1)}
--                 -- {curveID = curveID, axesFunc = (t,t)}

initCurve : Maybe (Curve DC.ChartDatum) -> Curve DC.ChartDatum
initCurve maybeLastCurve =
    case maybeLastCurve of
        Nothing ->
            curveIStoCurve <| initCurveIStates Nothing

        Just lastCurve ->
            let
                lastCurveID = .curveID lastCurve
                sameIDCurveIS = {curveID = lastCurveID, axesString = ("t","x1")}
            in
                curveIStoCurve <| initCurveIStates <| Just sameIDCurveIS
                    
type alias CurveID = Int
type alias ChartID = Int
    
    
------------------------------------------------
------------------------------------------------
-- ChartParam
------------------------------------------------
------------------------------------------------
        
type alias ChartParam data =
    { chartID : ChartID
    , curves : List (Curve data)
    }
    
-- initChartParam : Maybe (ChartParam DC.ChartDatum) -> ChartParam DC.ChartDatum 
-- initChartParam maybeLastChartParam =
--     case maybeLastChartParam of
--         Nothing ->
--             {chartID = 1, curves = [(initCurve Nothing)]}

--         Just lastChartParam ->
--             let 
--                 lastChartID = .chartID lastChartParam
--                 chartID = lastChartID + 1
--             in
--                 {chartID = chartID, curves = [(initCurve Nothing)]}

initChartParam : Maybe (ChartParam DC.ChartDatum) -> ChartParam DC.ChartDatum 
initChartParam maybeLastChartParam =
    case maybeLastChartParam of
        Nothing ->
            chartIStoChartParam <| initChartIStates Nothing

        Just lastChartParam ->
            let
                lastChartID = .chartID lastChartParam
                chartID = lastChartID + 1
                sameIDChartIS = {chartID = chartID, curvesString = [(initCurveIStates Nothing)]}
            in
                chartIStoChartParam <| initChartIStates <| Just sameIDChartIS



------------------------------------------------
------------------------------------------------
-- ChartIStates
------------------------------------------------
------------------------------------------------

type alias AxesString = (String,String)
type alias CurveIStates = {curveID : CurveID, axesString : AxesString}
    
-- nao estou precisando no momento
type alias ChartIStates = { chartID : ChartID, curvesString : List CurveIStates }

initCurveIStates : Maybe CurveIStates -> CurveIStates
initCurveIStates maybeLastCurveIS =
    case maybeLastCurveIS of
        Nothing ->
            {curveID = 1, axesString = ("t", "x1")}
                
        Just lastCurveIS ->
            let
                lastCurveID = .curveID lastCurveIS
                curveID = lastCurveID + 1
            in
                {curveID = curveID, axesString = ("t", "x1")}

curveIStoCurve : CurveIStates -> Curve DC.ChartDatum
curveIStoCurve curveIStates = 
    let
        curveID = .curveID curveIStates
        xFunc = stringToAxisFunc <| Tuple.first <| .axesString curveIStates
        yFunc = stringToAxisFunc <| Tuple.second <| .axesString curveIStates
    in
        {curveID = curveID, axesFunc = (xFunc,yFunc)}
                    
initChartIStates : Maybe ChartIStates -> ChartIStates
initChartIStates maybeChartIS =
    case maybeChartIS of
        Nothing ->
            {chartID = 1, curvesString = [(initCurveIStates Nothing)]}

        Just lastChartIS ->
            let
                lastChartID = .chartID lastChartIS
                chartID = lastChartID + 1
            in
             {chartID = chartID, curvesString = [(initCurveIStates Nothing)]}

chartIStoChartParam : ChartIStates -> ChartParam DC.ChartDatum
chartIStoChartParam chartIS =
    let
        chartID = .chartID chartIS
        curves = List.map curveIStoCurve <| .curvesString chartIS
    in
        {chartID = chartID, curves = curves}

                 
------------------------------------------------
------------------------------------------------
-- ChartInteract
------------------------------------------------
------------------------------------------------

type AxisType 
    = XAxis
    | YAxis

type ChartInteract
    = ChangeAxis CurveID AxisType String
    | AddCurve 
    | RemoveCurve CurveID

type ChartsInteract
    = AddChart
    | RemoveChart ChartID
      
      
------------------------------------------------
-- ChartsInteract
------------------------------------------------

chartsInteractAction : List (ChartParam DC.ChartDatum) -> ChartsInteract -> List (ChartParam DC.ChartDatum)
chartsInteractAction chartsParam  chartsInteract =
    case chartsInteract of
        AddChart ->
            let 
                maybeLastChartParam = lastElem chartsParam
                newChartParam = initChartParam maybeLastChartParam
            in
                chartsParam ++ [newChartParam]

        RemoveChart chartID ->
            List.filter (\chartParam ->
                            not <| (.chartID chartParam) == chartID) chartsParam

------------------------------------------------
-- ChartInteract
------------------------------------------------
      
chartIndividualInteractAction : ChartID -> List (ChartParam DC.ChartDatum) -> ChartInteract -> List (ChartParam DC.ChartDatum)
chartIndividualInteractAction chartID chartsParam chartInteract = 
    
    case (chartFromChartID chartID chartsParam) of
        
        Nothing ->
            chartsParam
                
        Just chartParam ->
            let
                newChartParam = chartInteractAction chartParam chartInteract
            in
                List.map (changeChartParam chartID newChartParam) chartsParam

            
chartInteractAction : ChartParam DC.ChartDatum -> ChartInteract -> ChartParam DC.ChartDatum
chartInteractAction chartParam chartInteract = 
    case chartInteract of
        ChangeAxis curveID axisType valueStr ->
            let
                curves = .curves chartParam
                axisFunc = stringToAxisFunc valueStr
                newCurves =
                    List.map (changeCurveAxis curveID axisType axisFunc) curves
            in
                {chartParam | curves = newCurves}


        AddCurve ->
            let
                curves = .curves chartParam
                maybeLastCurve = lastElem curves
                newCurve = initCurve maybeLastCurve
                newCurves = curves ++ (newCurve :: [])
            in
                {chartParam | curves = newCurves}

        RemoveCurve curveID ->
            let
                curves = .curves chartParam
                newCurves =
                    List.filter
                        (\c -> if (.curveID c) == curveID then
                                    False else True) curves
            in
                {chartParam | curves = newCurves}
     

changeChartParam : ChartID -> ChartParam DC.ChartDatum -> ChartParam DC.ChartDatum -> ChartParam DC.ChartDatum
changeChartParam chartID newChartParam chartParam =
    if (chartID == (.chartID chartParam)) then
        newChartParam
    else
        chartParam
        
            
changeCurveAxis : CurveID -> AxisType -> AxisFunc DC.ChartDatum -> Curve DC.ChartDatum -> Curve DC.ChartDatum 
changeCurveAxis curveID axisType axisFunc curve =
    let
       (xAxis, yAxis) = .axesFunc curve 
       otherCurveID = .curveID curve
    in
        if curveID == otherCurveID then
            case axisType of
                XAxis -> {curve | axesFunc = (axisFunc,yAxis)}
                YAxis -> {curve | axesFunc = (xAxis,axisFunc)}
        else
            curve

                
------------------------------------------------
------------------------------------------------
-- View
------------------------------------------------
------------------------------------------------
                
------------------------------------------------
-- ChartsView
------------------------------------------------

chartsView : DC.ChartData -> List (ChartParam DC.ChartDatum) -> (ChartsInteract -> msg) -> (ChartID -> ChartInteract -> msg) -> List (Html msg)
chartsView chartData chartsParam chartsIToMsg chartIDTochartIToMsg =
    case chartsParam of
        [] -> addChartButtonView chartsIToMsg :: []
        [chartParam] ->
            (div []
                [ removeChartButtonView chartsIToMsg (.chartID chartParam)
                , addChartButtonView chartsIToMsg
                , chartView chartData chartParam chartIDTochartIToMsg    
                ]) :: []
        (chartParam :: ls) ->
            (div []
                [ removeChartButtonView chartsIToMsg (.chartID chartParam)
                , chartView chartData chartParam chartIDTochartIToMsg    
                ])
            :: chartsView chartData ls chartsIToMsg chartIDTochartIToMsg


    
addChartButtonView : (ChartsInteract -> msg) -> Html msg
addChartButtonView chartsIToMsg =
    button [ onClick <| chartsIToMsg AddChart ] [ text "AddChart" ]
        
removeChartButtonView : (ChartsInteract -> msg) -> ChartID -> Html msg
removeChartButtonView chartsIToMsg chartID =
    button [ onClick <| chartsIToMsg (RemoveChart chartID) ] [ text "RemoveChart" ]

        
------------------------------------------------
-- ChartView
------------------------------------------------
        
chartView : DC.ChartData -> ChartParam DC.ChartDatum -> (ChartID -> ChartInteract -> msg) -> Html msg
chartView chartData chartParam chartIDTochartIToMsg =
    let
        curves = .curves chartParam
        chartID = .chartID chartParam
    in
    div []
        ([ chartContainerView <| chart5View chartData curves ]
        ++ chartCurvesView chartData (chartIDTochartIToMsg chartID) curves)

chart5View : DC.ChartData -> List (Curve DC.ChartDatum) -> Html msg
chart5View chartData curves =     
    C.chart
        [ CA.height 300
        , CA.width 300
        , CA.margin { top = 10, bottom = 20, left = 25, right = 20 }
        , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
        ]
        ([ C.xLabels []
        , C.yLabels [ CA.withGrid ]
        ] ++
                List.map (curveToChartSeriesView chartData) curves)
 
            
chartContainerView chart =
  div [  style "height" "300px"
      , style "width" "300px"]
      [
        chart
      ]
      
curveToChartSeriesView : DC.ChartData -> Curve DC.ChartDatum ->  C.Element DC.ChartDatum msg
curveToChartSeriesView chartData curve = 
    let 
        (xfunc,yfunc) = .axesFunc curve
    in
    
        C.series xfunc
        [ C.interpolated yfunc [ -- CA.monotone
                            ] [ ] --CA.circle ]
        ]

        chartData
            
chartCurvesView : DC.ChartData -> (ChartInteract -> msg) -> List (Curve data) -> List (Html msg)
chartCurvesView chartData chartIToMsg curves =
    case curves of
        [] ->  addCurveButtonView chartIToMsg :: []
        [c] -> 
            (div [] [ chartCurveSelectionView chartData chartIToMsg c
                    , removeCurveButtonView chartIToMsg (.curveID c)
                    , addCurveButtonView chartIToMsg]) :: []
        (c::cs) ->
            (div [] [ chartCurveSelectionView chartData chartIToMsg c
                    , removeCurveButtonView chartIToMsg (.curveID c)]) 
                :: chartCurvesView chartData chartIToMsg cs


addCurveButtonView : (ChartInteract -> msg) -> Html msg
addCurveButtonView chartIToMsg =
    button [ onClick <| chartIToMsg AddCurve ] [ text "+" ]
        
removeCurveButtonView : (ChartInteract -> msg) -> CurveID -> Html msg
removeCurveButtonView chartIToMsg curveID =
    button [ onClick <| chartIToMsg (RemoveCurve curveID ) ] [ text "-" ]
    
chartCurveSelectionView :  DC.ChartData -> (ChartInteract -> msg) -> Curve data -> Html msg
chartCurveSelectionView chartData chartIToMsg curve =
    let
       axesFunc = .axesFunc curve 
       curveID = .curveID curve
    in 
        span []
            [ label [] [text <| "Curve " ++ (String.fromInt curveID)]
            , select [onInput <| chartIToMsg << (ChangeAxis curveID XAxis)] (chartAxesOptionsView chartData "t")
            , select [onInput <| chartIToMsg << (ChangeAxis curveID YAxis)] (chartAxesOptionsView chartData "x1")
            ]
    
 
chartAxesOptionsView : DC.ChartData -> String -> List (Html msg)
chartAxesOptionsView chartData selStr =
    case chartData of
        [] -> []
        chartDatum :: ls ->
            case chartDatum of
                DC.T1S datum -> 
                    [ chartAxisOptionView "t" "t" selStr
                    , chartAxisOptionView "x1" "x" selStr]
                    

chartAxisOptionView : String -> String -> String -> Html msg
chartAxisOptionView val txt selStr =
        if (selStr == val) then
            option [value val, selected True] [text txt]
            -- option [value val] [text txt]
        else
            option [value val] [text txt]

            
------------------------------------------------
-- Auxiliary Functions
------------------------------------------------
            
stringToAxisFunc : String -> AxisFunc DC.ChartDatum
stringToAxisFunc str =
    case str of
        "t" -> t
        "x1" -> x1
        _ -> t
             
x1 : DC.ChartDatum -> Float
x1 chartDatum = 
    case chartDatum of
        T1S datum -> datum.x1

t : DC.ChartDatum -> Float
t chartDatum = 
    case chartDatum of
        T1S datum -> datum.t
                     
lastElem : List a -> Maybe a
lastElem list =
    case list of
        [] ->
            Nothing
        [last] ->
            Just last
        head :: rest ->
            lastElem rest

chartFromChartID : ChartID -> List (ChartParam DC.ChartDatum) -> Maybe (ChartParam DC.ChartDatum)
chartFromChartID chartID chartsParam = 
    let
        filteredList = List.filter
                        (\chartParam -> (.chartID chartParam) == chartID) 
                            chartsParam
    in
        List.head filteredList

            
                
                
-- newCurves =
--     List.map (changeCurveAxis curveID axisType axisFunc) curves
