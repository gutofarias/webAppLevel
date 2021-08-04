module MyChart exposing (..)

import Chart as C
import Chart.Attributes as CA
import DataConvert as DC exposing (..)

import Html exposing (Html,div,text,input)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onInput)

type alias AxisFunc data = data -> Float
    
type alias AxesFunc data =
    (AxisFunc data, AxisFunc data)

type alias Curve data =
    { curveID : CurveID
    , axesFunc : AxesFunc data
    }

type alias CurveID = Int
type alias ChartID = Int
        
type alias ChartParam data =
    { chartID : ChartID
    , curves : List (Curve data)
    }

type alias AxesString = (String,String)
type alias ChartIStates = { axes : AxesString }

type AxisType 
    = XAxis
    | YAxis

type ChartInteract
    = ChangeAxis CurveID AxisType
    | AddCurve 

chartContainer chart =
  div [  style "height" "300px"
      , style "width" "300px"]
      [
        chart
      ]
    

-- type alias Curve data =
--     { curveID : CurveID
--     , axesFunc : AxesFunc data
--     }
        
-- type alias ChartParam data =
--     { 
--         curves : List (Curve data)
--     }

initCurve : Maybe (Curve DC.ChartDatum) -> Curve DC.ChartDatum
initCurve maybeLastCurve =
    case maybeLastCurve of
        Nothing ->
            {curveID = 1, axesFunc = (t,x1)}
        Just lastCurve ->
            let
                lastCurveID = .curveID lastCurve
                curveID = lastCurveID + 1
            in
                {curveID = curveID, axesFunc = (t,x1)}

changeCurveAxis : AxisType -> AxisFunc DC.ChartDatum -> Curve DC.ChartDatum -> Curve DC.ChartDatum 
changeCurveAxis axisType axisFunc curve =
    let
       (xAxis, yAxis) = .axesFunc curve 
    in
        case axisType of
            XAxis -> {curve | axesFunc = (axisFunc,yAxis)}
            YAxis -> {curve | axesFunc = (xAxis,axisFunc)}

chart4 : DC.ChartData -> Curve DC.ChartDatum -> Html msg
chart4 chartData curve =     
    let 
        (xfunc,yfunc) = .axesFunc curve
    in
        C.chart
            [ CA.height 300
            , CA.width 300
            , CA.margin { top = 10, bottom = 20, left = 25, right = 20 }
            , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
            ]
            [ C.xLabels []
            , C.yLabels [ CA.withGrid ]
            , C.series xfunc
                [ C.interpolated yfunc [ -- CA.monotone
                                    ] [ ] --CA.circle ]
                ]

                chartData
            ]

chart5 : DC.ChartData -> List (Curve DC.ChartDatum) -> Html msg
chart5 chartData curves =     
    C.chart
        [ CA.height 300
        , CA.width 300
        , CA.margin { top = 10, bottom = 20, left = 25, right = 20 }
        , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
        ]
        ([ C.xLabels []
        , C.yLabels [ CA.withGrid ]
        ] ++
                List.map (curveToChartSeries chartData) curves)
 
            
curveToChartSeries : DC.ChartData -> Curve DC.ChartDatum ->  C.Element DC.ChartDatum msg
curveToChartSeries chartData curve = 
    let 
        (xfunc,yfunc) = .axesFunc curve
    in
    
        C.series xfunc
        [ C.interpolated yfunc [ -- CA.monotone
                            ] [ ] --CA.circle ]
        ]

        chartData
            
            
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
