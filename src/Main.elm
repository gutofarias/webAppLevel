module Main exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Models as M

import Browser
import Html exposing (Html, button, div, text, pre, input, label, select, option, span)
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
    -- , str : String
    , chartsParam : List MC.ChartParam 
    }

    
type Interact
    = Edo Edo.EdoInteract
    | Models M.ModelInteract
    | MChart MC.ChartID MC.ChartInteract
    | MCharts MC.ChartsInteract
        
        
type alias InteractStates =
    { edoIStates : Edo.EdoIStates
    , modelIStates : M.ModelIStates
    -- , chartIStates : MC.ChartIStates
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
        interactStates = {edoIStates = edoIStates, modelIStates = M.LevelIS levelIStates}
    in
            { chartData = chartData
            , modelParam = M.LevelP levelParam
            , edoParam = edoParam
            , interactStates = interactStates
            -- , str = "teste"
            , chartsParam = [(MC.initChartParam Nothing)]
            }

        
        
        
        
------------------------------------------------
-- update and Msg
------------------------------------------------
            
type Msg
    = RunEdo
    | ChangeInteract Interact
    | UpdateParameters 
    -- | ChangeStr String

update : Msg -> Model -> Model
update msg model =
  case msg of
      
    RunEdo -> 
      let  
          newModel = update UpdateParameters model
          edoParam = .edoParam newModel 
          modelParam = .modelParam newModel
          data = M.runEdoModel modelParam edoParam
      in
          { newModel | chartData = data }
            
    ChangeInteract interact ->
        let
           interactStates = .interactStates model
           edoIStates = .edoIStates interactStates
           modelIStates = .modelIStates interactStates
        in
            case interact of
                Edo edoInteract ->
                    let
                        edoIStatesNew = Edo.changeEdoIStates edoIStates edoInteract 
                        interactStatesNew = {interactStates | edoIStates = edoIStatesNew}
                    in
                        {model | interactStates = interactStatesNew}
                        
                Models modelInteract ->
                    let
                        modelIStatesNew = M.changeModelIStates modelIStates modelInteract
                        interactStatesNew = {interactStates | modelIStates = modelIStatesNew}
                    in 
                        {model | interactStates = interactStatesNew}

                MChart chartID chartInteract -> 
                    let
                        chartsParam = .chartsParam model
                        newChartsParam = MC.chartIndividualInteractAction chartID chartsParam chartInteract
                    in
                        {model | chartsParam = newChartsParam}

                MCharts chartsInteract ->
                    let
                        chartsParam = .chartsParam model
                        newChartsParam = MC.chartsInteractAction chartsParam chartsInteract
                    in
                        {model | chartsParam = newChartsParam}

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

    -- ChangeStr str -> 
    --      {model | str = str}

        
        
------------------------------------------------
-- View
------------------------------------------------
        
view : Model -> Html Msg
view model =
  let
    interactStates = .interactStates model
    edoIStates = .edoIStates interactStates
    modelIStates = .modelIStates interactStates
    chartData = .chartData model
    chartsParam = .chartsParam model
  in
    div []
        [
        div []
            (
            [ Edo.viewEdoIStates edoIStates (ChangeInteract << Edo)
            , M.viewModelIStates modelIStates (ChangeInteract << Models)
            , div [] []
            , button [ onClick RunEdo ] [ text "Edo" ]
            -- , label [] [ text model.str]
            ]
            ++ MC.chartsView chartData chartsParam (ChangeInteract << MCharts) (fcomposition23 ChangeInteract MChart)
            )
        -- , MC.chartView chartData chartParam (fcomposition23 ChangeInteract MChart)
        ]
         
fcomposition23 : (a -> b) -> (c -> d -> a) -> c -> d -> b 
fcomposition23 f2 f3 c = 
    f2 << (f3 c)

