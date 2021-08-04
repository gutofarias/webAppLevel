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
    , chartParam : MC.ChartParam DC.ChartDatum
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
        chartData = []
        levelIStates = {h0 = (String.fromFloat h0), ag =  (String.fromFloat ag), ap = (String.fromFloat ap)}
        edoIStates = {tini = (String.fromFloat tini), tfim = (String.fromFloat tfim)}
        chartIStates = {axes = ( "t", "x1" )}
        interactStates = {edoIStates = edoIStates, modelIStates = M.LevelIS levelIStates, chartIStates = chartIStates}
    in
            { chartData = chartData
            , modelParam = M.LevelP levelParam
            , edoParam = edoParam
            , interactStates = interactStates
            , str = "teste"
            , chartParam = {chartID = 0, curves = []}
            }

        
        
        
        
------------------------------------------------
-- update and Msg
------------------------------------------------
            
type Msg
    = RunEdo
    | ChangeInteract Interact String 
    | UpdateParameters 
    | ChangeStr String
    | UpdateChart MC.CurveID

update : Msg -> Model -> Model
update msg model =
  case msg of
      
    RunEdo -> 
      let  
          newModel = update UpdateParameters model
          -- newModel = update UpdateChart newModelAlmost
          edoParam = .edoParam newModel 
          modelParam = .modelParam newModel
          data = M.runEdoModel modelParam edoParam
      in
          { newModel | chartData = data }
            
    ChangeInteract interact valueStr ->
        let
           interactStates = .interactStates model
           edoIStates = .edoIStates interactStates
           modelIStates = .modelIStates interactStates
           chartIStates = .chartIStates interactStates
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

                MChart chartInteract -> 
                    case chartInteract of
                        MC.ChangeAxis curveID axisType ->
                            case axisType of 
                                MC.XAxis ->
                                    let
                                       chartParam = .chartParam model
                                       yaxis = Tuple.second <| .axes chartIStates
                                       newAxes = (valueStr,yaxis)
                                       newChartIStates =
                                           {chartIStates | axes = newAxes}
                                       newInteractStates =
                                           {interactStates | chartIStates = newChartIStates}
                                       newModel =
                                           {model | interactStates = newInteractStates}
                                    in
                                        update (UpdateChart curveID) newModel

                                MC.YAxis ->
                                    let
                                       xaxis = Tuple.first <| .axes chartIStates
                                       newAxes = (xaxis,valueStr)
                                       newChartIStates =
                                           {chartIStates | axes = newAxes}
                                       newInteractStates =
                                           {interactStates | chartIStates = newChartIStates}
                                       newModel =
                                           {model | interactStates = newInteractStates}
                                    in
                                        update (UpdateChart curveID) newModel

                        MC.AddCurve ->
                            let
                                chartParam = .chartParam model
                                curves = .curves chartParam
                                maybeLastCurve = MC.lastElem curves
                                newCurve = MC.initCurve maybeLastCurve
                                newCurves = curves ++ (newCurve :: [])
                                newChartParam = {chartParam | curves = newCurves}
                            in
                                {model | chartParam = newChartParam}

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

    UpdateChart curveID ->
        let
            interactStates = .interactStates model
            chartIStates = .chartIStates interactStates
            axesStr = .axes chartIStates
            chartParam = .chartParam model
            axesFunc = axesStringToAxesFunc axesStr
            curve = {curveID = 1, axesFunc = axesFunc}
            curves = [curve]
            newChartParam = {chartParam | curves = curves}
        in
            {model | chartParam = newChartParam}
                        

        
axesStringToAxesFunc : MC.AxesString -> MC.AxesFunc DC.ChartDatum
axesStringToAxesFunc axesString =
    let
        (xAxisStr,yAxisStr) = axesString
        xfunc = stringToAxisFunc xAxisStr
        yfunc = stringToAxisFunc yAxisStr
    in
        (xfunc,yfunc)


stringToAxisFunc : String -> MC.AxisFunc DC.ChartDatum
stringToAxisFunc str =
    case str of
        "t" -> MC.t
        "x1" -> MC.x1
        _ -> MC.t
        
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
        [
        div []
            [ Edo.viewEdoIStates edoIStates (ChangeInteract << Edo)
            , M.viewModelIStates modelIStates (ChangeInteract << Models)
            , div [] []
            , button [ onClick RunEdo ] [ text "Edo" ]
            , button [ onClick <| UpdateChart 1 ] [ text "UpdateChart" ]
            , button [ onClick <| ChangeInteract  (MChart MC.AddCurve) "" ] [ text "AddCurve" ]
            , label [for "location" ] [ text "Your closest center:" ]
            , select [ name "location" , onInput ChangeStr]
                [ option [value "ny"] [text "New York"]
                , option [value "il"] [text "Chicago"]
                ]
            , label [] [ text model.str]
            ]   
        , div []
            [ chartContainer <| MC.chart5 model.chartData curves ]
        , div []
            ( List.map (chartCurveSelection (.chartData model) (.chartIStates <| .interactStates model)) curves )
        ]
         
chartCurveSelection :  DC.ChartData -> MC.ChartIStates -> MC.Curve data -> Html Msg
chartCurveSelection chartData chartIStates curve =
    let
       (fstStr,sndStr) = .axes chartIStates
       axesFunc = .axesFunc curve 
       curveID = .curveID curve
    in 
        div []
            [ label [] [text <| "Curve " ++ (String.fromInt curveID)]
            , select [onInput <| ChangeInteract <| MChart (MC.ChangeAxis curveID MC.XAxis)] (chartAxesOptions chartData fstStr)
            , select [onInput <| ChangeInteract <| MChart (MC.ChangeAxis curveID MC.YAxis)] (chartAxesOptions chartData sndStr)
            ]
    

 
chartAxesOptions : DC.ChartData -> String -> List (Html msg)
chartAxesOptions chartData selStr =
    case chartData of
        [] -> []
        chartDatum :: ls ->
            case chartDatum of
                DC.T1S datum -> 
                    [ chartAxisOption "t" "t" selStr
                    , chartAxisOption "x1" "x" selStr]

chartAxisOption : String -> String -> String -> Html msg
chartAxisOption val txt selStr =
        if (selStr == val) then
            option [value val, selected True] [text txt]
        else
            option [value val] [text txt]

chartContainer : Html msg -> Html msg
chartContainer chart =
  div [  style "height" "300px"
      , style "width" "300px"]
      [
        chart
      ]
    
