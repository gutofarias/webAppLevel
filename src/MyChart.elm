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

type alias AxesString = (String,String)
    
type alias Curve =
    { curveID : CurveID
    , axesString : AxesString
    }

initCurve : Maybe Curve -> Curve 
initCurve maybeLastCurve =
    case maybeLastCurve of
        Nothing ->
            {curveID = 1, axesString = ("t","x1")}
                
        Just lastCurve ->
            let
                lastCurveID = .curveID lastCurve
                curveID = lastCurveID + 1
            in
                {curveID = curveID, axesString = ("t","x1")}

type alias CurveID = Int
type alias ChartID = Int
    
    
------------------------------------------------
------------------------------------------------
-- ChartParam
------------------------------------------------
------------------------------------------------
        
type alias ChartParam =
    { chartID : ChartID
    , curves : List Curve 
    }
    
initChartParam : Maybe ChartParam -> ChartParam 
initChartParam maybeLastChartParam =
    case maybeLastChartParam of
        Nothing ->
            {chartID = 1, curves = [(initCurve Nothing)]}

        Just lastChartParam ->
            let 
                lastChartID = .chartID lastChartParam
                chartID = lastChartID + 1
            in
                {chartID = chartID, curves = [(initCurve Nothing)]}


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

chartsInteractAction : List ChartParam -> ChartsInteract -> List ChartParam 
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
      
chartIndividualInteractAction : ChartID -> List ChartParam -> ChartInteract -> List ChartParam 
chartIndividualInteractAction chartID chartsParam chartInteract = 
    
    case (chartFromChartID chartID chartsParam) of
        
        Nothing ->
            chartsParam
                
        Just chartParam ->
            let
                newChartParam = chartInteractAction chartParam chartInteract
            in
                List.map (changeChartParam chartID newChartParam) chartsParam

            
chartInteractAction : ChartParam -> ChartInteract -> ChartParam 
chartInteractAction chartParam chartInteract = 
    case chartInteract of
        ChangeAxis curveID axisType valueStr ->
            let
                curves = .curves chartParam
                axisString = valueStr
                newCurves =
                    List.map (changeCurveAxis curveID axisType axisString) curves
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
     

changeChartParam : ChartID -> ChartParam -> ChartParam -> ChartParam 
changeChartParam chartID newChartParam chartParam =
    if (chartID == (.chartID chartParam)) then
        newChartParam
    else
        chartParam
        
            
changeCurveAxis : CurveID -> AxisType -> String -> Curve -> Curve 
changeCurveAxis curveID axisType axisString curve =
    let
       (xAxis, yAxis) = .axesString curve 
       otherCurveID = .curveID curve
    in
        if curveID == otherCurveID then
            case axisType of
                XAxis -> {curve | axesString = (axisString,yAxis)}
                YAxis -> {curve | axesString = (xAxis,axisString)}
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

chartsView : DC.ChartData -> List ChartParam -> (ChartsInteract -> msg) -> (ChartID -> ChartInteract -> msg) -> List (Html msg)
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
        
chartView : DC.ChartData -> ChartParam -> (ChartID -> ChartInteract -> msg) -> Html msg
chartView chartData chartParam chartIDTochartIToMsg =
    let
        curves = .curves chartParam
        chartID = .chartID chartParam
    in
    div []
        ([ chartContainerView <| chart5View chartData curves ]
        ++ chartCurvesView chartData (chartIDTochartIToMsg chartID) curves)

chart5View : DC.ChartData -> List Curve -> Html msg
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
                -- Usa o filteMap porque o resultado das curvas é um maybe. So plota a curva se o resultado for um Just. 
                List.filterMap (curveToChartSeriesView chartData) curves)
 
            
chartContainerView chart =
  div [  style "height" "300px"
      , style "width" "300px"]
      [
        chart
      ]
      
curveToChartSeriesView : DC.ChartData -> Curve -> Maybe (C.Element DC.ChartDatum msg)
curveToChartSeriesView chartData curve = 
    let 
        (xstr,ystr) = .axesString curve
                      
        -- stringToAxisFunc me retorna um Maybe (AxisFuncMaybe ChartDatum) 
        -- type alias AxisFuncMaybe data = data -> Maybe Float
        maybeXfuncMaybeFloat = stringToAxisFunc xstr
        maybeYfuncMaybeFloat = stringToAxisFunc ystr
                               
        -- Maybe (ChartDatum -> Maybe Float)
        -- Dois maybes, o de fora é pq eu nao sei se o string retorna uma funcao valida, o de dentro é porque eu não sei se o chartDatum permite que eu aplique a função. Posso ter um TS1 e pedir u1, por exemplo
    in
        case (maybeXfuncMaybeFloat, maybeYfuncMaybeFloat) of

            -- (ChartDatum -> Maybe Float)
            -- Analisa se existe a função para a dada string
            (Just xfuncMaybeFloat, Just yfuncMaybeFloat) ->
                let
                    -- Maybe (ChartDatum -> Float)
                    -- Analisa se o dado é suportado pela funcao usando o chartData 
                    maybeXfunc = funcMaybeToMaybeFunc2 chartData xfuncMaybeFloat
                    maybeYfunc = funcMaybeToMaybeFunc2 chartData yfuncMaybeFloat
                in
                    case (maybeXfunc, maybeYfunc) of
                        -- Caso tudo esteja ok consegue pegar a funcao e retornar o Just
                        (Just xfunc, Just yfunc) ->
                            Just  
                            (C.series xfunc
                            [ C.interpolated yfunc [ -- CA.monotone
                                                ] [ ] --CA.circle ]
                            ]

                            chartData)

                        -- caso contrario retorna nothing
                        _ -> Nothing
            -- caso contrario retorna nothing
            _ -> Nothing
                         
            
chartCurvesView : DC.ChartData -> (ChartInteract -> msg) -> List Curve -> List (Html msg)
chartCurvesView chartData chartIToMsg curves =
    case curves of
        [] ->  addCurveButtonView chartIToMsg :: []
        [c] -> 
            (div [style "height" "30px"] [ chartCurveSelectionView chartData chartIToMsg c
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
    
chartCurveSelectionView :  DC.ChartData -> (ChartInteract -> msg) -> Curve -> Html msg
chartCurveSelectionView chartData chartIToMsg curve =
    let
       (xstr,ystr) = .axesString curve 
       curveID = .curveID curve
    in 
        span []
            [ label [] [text <| "Curve " ++ (String.fromInt curveID)]
            , select [onInput <| chartIToMsg << (ChangeAxis curveID XAxis)] (chartAxesOptionsView chartData xstr)
            , select [onInput <| chartIToMsg << (ChangeAxis curveID YAxis)] (chartAxesOptionsView chartData ystr)
            ]
    
 
chartAxesOptionsView : DC.ChartData -> String -> List (Html msg)
chartAxesOptionsView chartData selStr =
    case chartData of
        [] -> []
        chartDatum :: ls ->
            case chartDatum of
                DC.TS1 datum -> 
                    [ chartAxisOptionView "t" "t" selStr
                    , chartAxisOptionView "x1" "x" selStr]
                DC.TS1E1R1U1 datum ->
                    [ chartAxisOptionView "t" "t" selStr
                    , chartAxisOptionView "x1" "x" selStr
                    , chartAxisOptionView "e1" "e" selStr
                    , chartAxisOptionView "r1" "r" selStr
                    , chartAxisOptionView "u1" "u" selStr]
                DC.TS1E1R1U4 datum ->
                    [ chartAxisOptionView "t" "t" selStr
                    , chartAxisOptionView "x1" "x" selStr
                    , chartAxisOptionView "e1" "e" selStr
                    , chartAxisOptionView "r1" "r" selStr
                    , chartAxisOptionView "u1" "u" selStr
                    , chartAxisOptionView "u2" "up" selStr
                    , chartAxisOptionView "u3" "ui" selStr
                    , chartAxisOptionView "u4" "ud" selStr]

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
lastElem : List a -> Maybe a
lastElem list =
    case list of
        [] ->
            Nothing
        [last] ->
            Just last
        head :: rest ->
            lastElem rest

chartFromChartID : ChartID -> List ChartParam -> Maybe ChartParam 
chartFromChartID chartID chartsParam = 
    let
        filteredList = List.filter
                        (\chartParam -> (.chartID chartParam) == chartID) 
                            chartsParam
    in
        List.head filteredList

