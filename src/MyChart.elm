module MyChart exposing (..)

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
    , editingCurves : Bool 
    }
    
initChartParam : Maybe ChartParam -> ChartParam 
initChartParam maybeLastChartParam =
    case maybeLastChartParam of
        Nothing ->
            { chartID = 1
            , curves = [(initCurve Nothing)]
            , editingCurves = False
            }

        Just lastChartParam ->
            let 
                lastChartID = .chartID lastChartParam
                chartID = lastChartID + 1
            in
                { chartID = chartID
                , curves = [(initCurve Nothing)]
                , editingCurves = False
                }

addChartParam : ChartID -> ChartParam
addChartParam chartID =
    { chartID = chartID
    , curves = [(initCurve Nothing)]
    , editingCurves = False
    }

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
    | ToggleEditCurve 

type ChartsInteract
    = AddChart ChartID
    | RemoveChart ChartID
      
      
------------------------------------------------
-- ChartsInteract
------------------------------------------------

chartsInteractAction : List ChartParam -> ChartsInteract -> List ChartParam 
chartsInteractAction chartsParam  chartsInteract =
    case chartsInteract of
        AddChart chartID ->
            let 
                newChartParam = addChartParam chartID
                newChartsParam =  newChartParam::chartsParam 
            in
                List.sortBy (\cp -> cp.chartID) newChartsParam
                

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

        ToggleEditCurve ->
            let
                editingCurves = .editingCurves chartParam
            in 
                {chartParam | editingCurves = (not editingCurves)}
     

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

    
------------------------------------------------
-- ChartView
------------------------------------------------

buttonToggleChart : Bool -> (ChartInteract -> msg) -> E.Element msg
buttonToggleChart editingCurves chartInteractToMsg =
    let
        text = case editingCurves of
            True -> "Show Charts"
            False -> "Edit Curves"
    in
        EI.button [E.alignRight]
            { onPress = Just (chartInteractToMsg ToggleEditCurve)
            , label = E.el [] <| E.text text
            } 

removeChartButton : ChartID -> (ChartsInteract -> msg) -> E.Element msg
removeChartButton chartID chartsInteractToMsg = 
    EI.button [E.alignLeft]
        { onPress = Just (chartsInteractToMsg <| RemoveChart chartID)
        , label = E.el [] <| E.text "Remove Chart"
        } 

        
chartViewElement : DC.ChartData -> ChartParam -> (ChartID -> ChartInteract -> msg) -> (ChartsInteract -> msg) -> E.Element msg
chartViewElement chartData chartParam chartIDTochartIToMsg chartsInteractToMsg =
    let
        curves = .curves chartParam
        chartID = .chartID chartParam
        editingCurves = .editingCurves chartParam
    in
        UI.element [] <|
        case editingCurves of
            True ->
                E.column [E.centerX, E.spacing  20, E.width E.fill]
                    ([ E.row [E.width E.fill]
                           [ removeChartButton chartID chartsInteractToMsg
                           , buttonToggleChart editingCurves (chartIDTochartIToMsg chartID)
                           ]
                    ]
                    ++ chartCurvesViewElement chartData (chartIDTochartIToMsg chartID) curves)

            False -> 
                E.column [E.centerX, E.width E.fill, E.height E.fill]
                    [ E.row [E.width E.fill]
                           [ removeChartButton chartID chartsInteractToMsg
                           , buttonToggleChart editingCurves (chartIDTochartIToMsg chartID)
                           ]
                    , E.el[E.centerX, E.width <| E.px 400] <| E.html <| chart5View chartData curves
                    ]

chart5View : DC.ChartData -> List Curve -> Html msg
chart5View chartData curves =     
    C.chart
        [ CA.height 270
        , CA.width 300
        , CA.margin { top = 10, bottom = 20, left = 25, right = 20 }
        , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
        ]
        ([ C.xLabels []
        , C.yLabels [ CA.withGrid ]
        ] ++
                -- Usa o filteMap porque o resultado das curvas é um maybe. So plota a curva se o resultado for um Just. 
                List.filterMap (curveToChartSeriesView chartData) curves)
 
            
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
                         

chartCurvesViewElement : DC.ChartData -> (ChartInteract -> msg) -> List Curve -> List (E.Element msg)
chartCurvesViewElement chartData chartIToMsg curves =
    let
        width = 0
        spacing = E.spacing 10 
        -- align = E.alignRight
        align = E.centerX
    in
    case curves of
        [] ->
            (E.row [ align
                   , spacing
                   ]
                   [ UI.addCurveButton (chartIToMsg AddCurve)
                   -- , UI.addCurveButtonSpace
                   -- , UI.elementWidth width
                   ]) :: []
        [c] -> 
            (E.row [ align
                   , spacing
                   ]
                 [ chartCurveSelectionViewElement chartData chartIToMsg c
                 , UI.removeCurveButton (chartIToMsg (RemoveCurve (.curveID c)))
                 , UI.addCurveButton (chartIToMsg AddCurve)
                 , UI.elementNoneWidth width
                 ]) :: []
        (c::cs) ->
            (E.row [ align
                   , spacing
                   ]
                 [ (chartCurveSelectionViewElement chartData chartIToMsg c)
                 , UI.removeCurveButton (chartIToMsg (RemoveCurve (.curveID c)))
                 , UI.addCurveButtonSpace
                 , UI.elementNoneWidth width
                      ]) 
                :: chartCurvesViewElement chartData chartIToMsg cs

addCurveButtonViewElement : (ChartInteract -> msg) -> E.Element msg
addCurveButtonViewElement chartIToMsg =
    E.html <|
    button [ onClick <| chartIToMsg AddCurve ] [ text "+" ]
        
removeCurveButtonViewElement : (ChartInteract -> msg) -> CurveID -> E.Element msg
removeCurveButtonViewElement chartIToMsg curveID =
    E.html <|
    button [ onClick <| chartIToMsg (RemoveCurve curveID ) ] [ text "-" ]
        
        
chartCurveSelectionViewElement :  DC.ChartData -> (ChartInteract -> msg) -> Curve -> E.Element msg
chartCurveSelectionViewElement chartData chartIToMsg curve =
    let
       (xstr,ystr) = .axesString curve 
       curveID = .curveID curve
    in 
        E.column [E.centerX] 
            [ E.row [E.height <| E.px 50 ]
                 [ E.el [] (E.text <| "Curve " ++  ((String.fromInt curveID) ++ "   " ))
                 , UI.select (chartIToMsg << ChangeAxis curveID XAxis) (chartAxesOptionsView chartData xstr) 
                 , UI.select (chartIToMsg << ChangeAxis curveID YAxis) (chartAxesOptionsView chartData ystr) 
                 ]
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

listMaybeChartToListElement : List (Maybe ChartParam) -> ChartData -> (ChartID -> ChartInteract -> msg) -> (ChartsInteract -> msg) -> List (E.Element msg)
listMaybeChartToListElement listMaybe chartData chartIDTochartIToMsg chartsInteractToMsg =
    let
        indexedFunc index maybeChart = 
            case maybeChart of
                Just chartParam ->
                    chartViewElement chartData chartParam chartIDTochartIToMsg chartsInteractToMsg
                Nothing ->
                    UI.addNewElementSpace (chartsInteractToMsg (AddChart (index+1)))
    in 
    List.indexedMap indexedFunc listMaybe
   
listToTuple : List (E.Element msg) -> List (E.Element msg,E.Element msg)
listToTuple listElement =
    case listElement of
        (a::b::xs) -> (a,b)::listToTuple xs
        _ -> []

chartsTuple : List ChartParam -> ChartData -> (ChartID -> ChartInteract -> msg) -> (ChartsInteract -> msg) -> List (E.Element msg, E.Element msg)
chartsTuple chartsParam chartData chartIDTochartIToMsg chartsInteractToMsg =
    let
        maxChartID = maxChartIDFromListCharts chartsParam
        listMaybe = fillWithMaybe chartsParam 1
        newListMaybe = 
            if (modBy 2 maxChartID == 0) then
                listMaybe ++ [Nothing,Nothing]
            else 
                listMaybe ++ [Nothing]

        listElement = listMaybeChartToListElement newListMaybe chartData chartIDTochartIToMsg chartsInteractToMsg 
    in 
        listToTuple listElement

                       
maxChartIDFromListCharts : List ChartParam -> Int
maxChartIDFromListCharts chartsParam =
    List.foldr (\cp acc -> max cp.chartID acc) 0 chartsParam 


fillWithMaybe : List ChartParam -> Int -> List (Maybe ChartParam)
fillWithMaybe chartsParam index = 
    case chartsParam of
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
                    Nothing ::(fillWithMaybe chartsParam newIndex)
