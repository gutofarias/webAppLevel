module Main exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Models as M

import Browser
import Html exposing (Html, button, div, text, pre, input, label, select, option)
import Html.Attributes exposing (style, placeholder, value, for, name, selected)
import Html.Events exposing (onClick, onInput)

import Chart as C
import Chart.Attributes as CA

import MyChart as MC





------------------------------------------------
-- main
------------------------------------------------

main =
  Browser.sandbox { init = init, update = update, view = view }

      
      
      
      
------------------------------------------------
-- Model and Data Types
------------------------------------------------

type alias DataILevel = {initState:Float, levelParam:M.LevelParam}

type alias Model =
    { chartData : DC.ChartData
    , modelParam : M.ModelParam
    , edoParam : Edo.EdoParam
    , interactStates : InteractStates
    , str : String
    , chartParam : MC.ChartParam DC.DatumT1S
    }

    
type Interact
    = Edo Edo.EdoInteract
    | Models M.ModelInteract
    | MChart MC.ChartInteract
        
        
type alias InteractStates =
    { edoIStates : Edo.EdoIStates
    , modelIStates : M.ModelIStates
    , chartIStates : MC.ChartIStates
    }

updateEdoIStates : Edo.EdoIStates -> InteractStates -> InteractStates
updateEdoIStates edoIStates interactStates =
    {interactStates | edoIStates = edoIStates}
    
updateModelIStates : M.ModelIStates -> InteractStates -> InteractStates
updateModelIStates modelIStates interactStates =
    {interactStates | modelIStates = modelIStates}


------------------------------------------------
-- init
------------------------------------------------

init : Model 
init =
    let
        ag = 1.0
        ap = 0.1
        levelGeoParam = M.LevelGeoParam ag ap
        h0 = 10.0
        levelInitState = M.LevelInitState h0
        levelParam = {initState = levelInitState, geoParam = levelGeoParam}
        tini = 0.0 
        tfim = 10.0
        passoInt = 0.01 
        relSaida = 2
        edoParam = Edo.EdoParam tini tfim passoInt relSaida Edo.rungeKutta
        chartData = DC.Nodata
        levelIStates = {h0 = (String.fromFloat h0), ag =  (String.fromFloat ag), ap = (String.fromFloat ap)}
        edoIStates = {tini = (String.fromFloat tini), tfim = (String.fromFloat tfim)}
        chartIStates = {axes = [ "t", "x1" ]}
        interactStates = {edoIStates = edoIStates, modelIStates = M.LevelIS levelIStates, chartIStates = chartIStates}
    in
            { chartData = chartData
            , modelParam = M.LevelP levelParam
            , edoParam = edoParam
            , interactStates = interactStates
            , str = "teste"
            , chartParam = {curves = []}
            }

        
        
        
        
------------------------------------------------
-- update and Msg
------------------------------------------------
            
type Msg
    = RunEdo
    | ChangeNumericInput Interact String 
    | UpdateParameters 
    | ChangeStr String
    | UpdateChart

update : Msg -> Model -> Model
update msg model =
  case msg of
      
    RunEdo -> 
      let  
          newModelAlmost = update UpdateParameters model
          newModel = update UpdateChart newModelAlmost
          edoParam = .edoParam newModel 
          modelParam = .modelParam newModel
          data = M.runEdoModel modelParam edoParam
      in
          { newModel | chartData = data }
            
    ChangeNumericInput interact valueStr ->
        let
           interactStates = .interactStates model
           edoIStates = .edoIStates interactStates
           modelIStates = .modelIStates interactStates
        in
            case interact of
                Edo edoInteract ->
                    let
                        edoIStatesNew = Edo.changeEdoIStates edoIStates edoInteract valueStr
                        interactStatesNew = {interactStates | edoIStates = edoIStatesNew}
                    in
                        {model | interactStates = interactStatesNew}
                        
                Models modelInteract ->
                    let
                        modelIStatesNew = M.changeModelIStates modelIStates modelInteract valueStr
                        interactStatesNew = {interactStates | modelIStates = modelIStatesNew}
                    in 
                        {model | interactStates = interactStatesNew}

                MChart _ -> model

    UpdateParameters ->
        let
            interactStates = .interactStates model
            edoIStates = .edoIStates interactStates
            modelIStates = .modelIStates interactStates
            edoParam = .edoParam model
            modelParam = .modelParam model
            edoParamNew = Edo.updateEdoParam edoParam edoIStates
            modelParamNew = M.updateModelParam modelParam modelIStates
        in
            {model | edoParam = edoParamNew, modelParam = modelParamNew}

    ChangeStr str -> 
         {model | str = str}

    UpdateChart ->
        let
            funcT = .t
            funcX1 = .x1
            curve : MC.Curve DC.DatumT1S
            curve = {curveID = 1, axesFunc = (funcT,funcX1)}
            curves = [curve]
            chartParam = {curves = curves}
        in
            {model | chartParam = chartParam}
                        

        
        
------------------------------------------------
-- View
------------------------------------------------
        
view : Model -> Html Msg
view model =
  let
    interactStates = .interactStates model
    edoIStates = .edoIStates interactStates
    modelIStates = .modelIStates interactStates
    chartParam = .chartParam model
    curves = .curves chartParam
  in
      
    div []
        ([ Edo.viewEdoIStates edoIStates (ChangeNumericInput << Edo)
        , M.viewModelIStates modelIStates (ChangeNumericInput << Models)
        , div [] []
        , button [ onClick RunEdo ] [ text "Edo" ]
        , chartContainer <| chart4 model.chartData 
        , label [for "location" ] [ text "Your closest center:" ]
        , select [ name "location" , onInput ChangeStr]
            [ option [value "ny"] [text "New York"]
            , option [value "il"] [text "Chicago"]
            ]
        , label [] [ text model.str]
        -- , div [] []
        -- , label [] [text "Curve 1 "]
        -- , select [] (chartAxesOptions <| .chartData model)
        -- , select [] (chartAxesOptions <| .chartData model)
        ] ++ 
         List.map (chartCurve (.chartData model) (.chartIStates <| .interactStates model)) curves)
         
chartCurve :  DC.ChartData -> MC.ChartIStates -> MC.Curve data -> Html msg
chartCurve chartData chartIStates curve =
    let
       (fstStr,sndStr) = case (.axes chartIStates) of
                             a :: b :: [] -> (a,b)
                             _ -> ("","")
       axesFunc = .axesFunc curve 
       -- testStr = axisFuncToStr <| Tuple.first axesFunc
    in 
        div []
            [ label [] [text <| "Curve " ++ (String.fromInt <| .curveID curve)]
            , select [] (chartAxesOptions chartData fstStr)
            , select [] (chartAxesOptions chartData sndStr)
            ]
    
-- -- axisFuncToStr : MC.AxisFunc data -> String
-- axisFuncToStr axisFunc = 
--     case (round <| axisFunc datumExample) of
--         0 -> "t"
--         1 -> "x1"
--         _ -> "x2"

-- datumExample =  
--     { t = 0.0
--     , x1 = 1.0
--     , x2 = 2.0
--     , x3 = 3.0
--     , x4 = 4.0
--     , x5 = 5.0
--     }

 
chartAxesOptions : DC.ChartData -> String -> List (Html msg)
chartAxesOptions chartData selStr =
    case chartData of
        DC.T1S data -> 
            [ chartAxisOption "t" "t" selStr
            , chartAxisOption "x1" "x" selStr]
        _ ->
            []

chartAxisOption : String -> String -> String -> Html msg
chartAxisOption val txt selStr =
        if (selStr == val) then
            option [value val, selected True] [text txt]
        else
            option [value val] [text txt]

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
