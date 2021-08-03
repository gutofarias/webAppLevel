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
        
type alias ChartParam data =
    { 
        curves : List (Curve data)
    }

funcT : AxisFunc DC.DatumT1S
funcT = .t
        
-- funcX1 : data -> Float
-- funcX1 data = data.x1
       
initCurve = 
    { curveID = 0
    , axesFunc = ( funcT, funcT)
    }

type alias ChartIStates = { axes : List String }

type alias CurveID = Int
type AxisType 
    = XAxis
    | YAxis

type ChartInteract
    = Axes CurveID AxisType

chartContainer chart =
  div [  style "height" "300px"
      , style "width" "300px"]
      [
        chart
      ]
    

chart4 chartData =     
    let 
        data = decodeData chartData
    in 
        C.chart
            [ CA.height 300
            , CA.width 300
            , CA.margin { top = 10, bottom = 20, left = 25, right = 20 }
            , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
            ]
            [ C.xLabels []
            , C.yLabels [ CA.withGrid ]
            , C.series .t
                [ C.interpolated .x1 [ -- CA.monotone
                                    ] [ ] --CA.circle ]
                ]

                data
            ]

decodeData chartData =
    case chartData of
        DC.T1S data -> data
        _ -> []
