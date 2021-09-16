module MyCharts.Chart exposing (..)


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

import MyCharts.Chart.Curve as Curve
import MyCharts.Chart.Zoom as Zoom

import Zoom as EVZoom

import Svg.Attributes as SA

------------------------------------------------
-- Model
------------------------------------------------
        
type alias ChartID = Int
    
type alias Model =
    { chartID : ChartID
    , curves : List Curve.Model 
    , editingCurves : Bool 
    , zoomModel : Zoom.Model
    , zoom : EVZoom.Zoom
    }
    
    
------------------------------------------------
-- init
------------------------------------------------

init : Maybe Model -> Model 
init maybeLastChart =
    case maybeLastChart of
        Nothing ->
            { chartID = 1
            , curves = [(Curve.init Nothing)]
            , editingCurves = False
            , zoomModel = Zoom.init
            , zoom = myInitZoom
            }

        Just lastChart ->
            let 
                lastChartID = .chartID lastChart
                chartID = lastChartID + 1
            in
                { chartID = chartID
                , curves = [(Curve.init Nothing)]
                , editingCurves = False
                , zoomModel = Zoom.init
                , zoom = myInitZoom
                }

                
addModel : ChartID -> Model
addModel chartID =
    { chartID = chartID
    , curves = [(Curve.init Nothing)]
    , editingCurves = False
    , zoomModel = Zoom.init
    , zoom = myInitZoom
    }

myInitZoom : EVZoom.Zoom
myInitZoom =
    EVZoom.init {width = UI.elementWidth, height = UI.elementHeight}
        |> EVZoom.scaleExtent 0.5 1000.0
        -- |> EVZoom.translateExtent ((-0.1*UI.elementWidth, -0.1*UI.elementHeight),(1.1*UI.elementWidth,1.1*UI.elementHeight))

------------------------------------------------
-- Msg
------------------------------------------------
    
type Msg
    = AddCurve 
    | RemoveCurve Curve.CurveID
    | CurveMsg Curve.CurveID Curve.Msg
    | ToggleEditCurve 
    | ZoomMsg Zoom.Msg
    | EVZoomMsg EVZoom.OnZoom
      
      
------------------------------------------------
-- update
------------------------------------------------
      
update : Msg -> Model -> Model 
update msg model = 
    case msg of
        CurveMsg curveID curveMsg ->
            let
                curves = .curves model
                newCurves =
                    List.map
                        (\curve ->
                             if (curve.curveID == curveID) then
                                Curve.update curveMsg curve
                             else 
                                curve)
                            curves
            in
                {model | curves = newCurves}


        AddCurve ->
            let
                curves = .curves model
                maybeLastCurve = lastElem curves
                newCurve = Curve.init maybeLastCurve
                newCurves = curves ++ (newCurve :: [])
            in
                {model | curves = newCurves}

        RemoveCurve curveID ->
            let
                curves = .curves model
                newCurves =
                    List.filter
                        (\c -> if (.curveID c) == curveID then
                                    False else True) curves
            in
                {model | curves = newCurves}

        ToggleEditCurve ->
            let
                editingCurves = .editingCurves model
            in 
                {model | editingCurves = (not editingCurves)}
     
        ZoomMsg zoomMsg ->
            let
                zoomModel = .zoomModel model
                newZoomModel = Zoom.update zoomMsg zoomModel
            in
                {model | zoomModel = newZoomModel}
            
        EVZoomMsg evZoomMsg ->
            {model | zoom = EVZoom.update evZoomMsg model.zoom}

subscriptions : Model -> (Msg -> msg) -> Sub msg
subscriptions model msgToMainMsg =
    EVZoom.subscriptions model.zoom (msgToMainMsg << EVZoomMsg)

------------------------------------------------
-- view
------------------------------------------------

-- zoomAttrs EVZoom.Zoom -> 
zoomAttrs zoom = 
    let
        record = EVZoom.asRecord zoom
        scale = record.scale
        x = record.translate.x -- / (20.0*scale)
        y = record.translate.y -- / (20.0*scale)
        -- width = UI.elementWidth
        width = 360
        -- height = UI.elementHeight
        height = 330
        
        axisFunc widthHeight translation iAxis = 
            let 
                range = (iAxis.dataMax - iAxis.dataMin)
                newRange = range/scale 
                -- trans = translation*range/400.0
                trans = translation*range/(scale*widthHeight) -- - (scale - 1.0)*range/width
                center = (iAxis.dataMin + iAxis.dataMax)/2.0 + trans/2
                -- center = (iAxis.min + iAxis.max)/2.0 + trans
                newMin = center - newRange/2
                -- newMin = iAxis.min/scale + trans
                newMax = center + newRange/2
                -- newMax = iAxis.max/scale + trans
            in 
                {iAxis | min = newMin, max = newMax}
                    
    in
        -- SA.viewBox "0 0 500 500"
        -- [ rangeX, rangeY ]
        [ CA.range [axisFunc width -x]
        , CA.domain [axisFunc height y]
        ]

view : Model -> (Msg -> msg) -> DC.ChartData -> List Curve.Model -> Html msg
view model msgToMainMsg chartData curves =     
    let
        zoomModel = .zoomModel model
        evZoom = .zoom model
        record = EVZoom.asRecord evZoom
        x = record.translate.x
        y = record.translate.y
        scale = record.scale
    in 
    C.chart
        ([ CA.height 380
        , CA.width 400
        , CA.padding { top = 10, bottom = 0, left = 0, right = 20 }
        , CA.margin { top = 40, bottom = 20, left = 50, right = 0 }
        , CA.attrs [EVZoom.transform evZoom]
        , CA.attrs <| EVZoom.events evZoom (msgToMainMsg << EVZoomMsg)
         -- , CA.range [(\axis -> {axis | min = 5})] 
         -- , CA.range [ CA.lowest -2 CA.more ]
         -- , CA.attrs [zoomAttrs evZoom]
         -- , CA.range [ CA.lowest 1 CA.exactly
         --            , CA.highest 9 CA.exactly ]
         --     |> CA.moveRight 5
         -- , CA.domain [ CA.lowest 3 CA.exactly
         --            , CA.highest 6 CA.exactly ]
                       
        ] --
             ++ (zoomAttrs evZoom)
             -- ++ Zoom.chartAttrs zoomModel (msgToMainMsg << ZoomMsg))
             
             )
        ([ C.xLabels [ CA.pinned .min, CA.withGrid ]
         , C.yLabels [ CA.pinned .min, CA.withGrid ]
         , C.xAxis [ CA.pinned .min -- (\c -> (c.min + 11*(c.max - c.min)/380)) 
                   ]
         , C.yAxis []
         , C.xTicks [ CA.pinned .min 
                    ]
         , C.yTicks []
         , C.legendsAt .min .max  -- (\c -> (c.min + c.max)/2)
             [ CA.row
             , CA.alignLeft
             -- , CA.column
             , CA.moveRight 20
             -- , CA.moveLeft 80
             , CA.moveUp 28
             , CA.spacing 15
             ] [ CA.width 20
               ]
        ] ++
                -- Usa o filteMap porque o resultado das curvas é um maybe. So plota a curva se o resultado for um Just. 
             (case chartData of
                 [] -> []
                 chartDatum::cds -> 
                    List.filterMap (Curve.curveToChartSeries chartDatum chartData) curves)
          -- ++ Zoom.chartElements zoomModel (msgToMainMsg << ZoomMsg)
        )
 
            
------------------------------------------------
-- viewCurves
------------------------------------------------

viewCurves : DC.ChartData -> (Msg -> msg) -> List Curve.Model -> List (E.Element msg)
viewCurves chartData chartIToMsg curves =
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
                 [ curveSelectionView chartData chartIToMsg c
                 , UI.removeCurveButton (chartIToMsg (RemoveCurve (.curveID c)))
                 , UI.addCurveButton (chartIToMsg AddCurve)
                 , UI.elementNoneWidth width
                 ]) :: []
        (c::cs) ->
            (E.row [ align
                   , spacing
                   ]
                 [ (curveSelectionView chartData chartIToMsg c)
                 , UI.removeCurveButton (chartIToMsg (RemoveCurve (.curveID c)))
                 , UI.addCurveButtonSpace
                 , UI.elementNoneWidth width
                      ]) 
                :: viewCurves chartData chartIToMsg cs

                    
curveSelectionView :  DC.ChartData -> (Msg -> msg) -> Curve.Model -> E.Element msg
curveSelectionView chartData chartIToMsg curve =
    let
       (xstr,ystr) = .axesString curve 
       curveID = .curveID curve
    in 
        E.column [E.centerX] 
            [ E.row [E.height <| E.px 50 ]
                 [ E.el [] (E.text <| "Curve " ++  ((String.fromInt curveID) ++ "   " ))
                 , UI.select (chartIToMsg << (CurveMsg curveID) << Curve.XAxis )  (Curve.view chartData xstr) 
                 , UI.select (chartIToMsg << (CurveMsg curveID) <<  Curve.YAxis ) (Curve.view chartData ystr) 
                 ]
            ]

lastElem : List a -> Maybe a
lastElem list =
    case list of
        [] ->
            Nothing
        [last] ->
            Just last
        head :: rest ->
            lastElem rest

chartFromChartID : ChartID -> List Model -> Maybe Model 
chartFromChartID chartID chartsParam = 
    let
        filteredList = List.filter
                        (\model -> (.chartID model) == chartID) 
                            chartsParam
    in
        List.head filteredList
