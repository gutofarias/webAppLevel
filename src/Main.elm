module Main exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Models as M
import Controller as Control 
import Reference as Ref

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
    , controlParam : Control.ControlParam
    , refParam : Ref.RefParam
    }

    
type Interact
    = Edo Edo.EdoInteract
    | Models M.ModelInteract
    | MChart MC.ChartID MC.ChartInteract
    | MCharts MC.ChartsInteract
    | Control Control.ControlInteract  
    | Ref Ref.RefInteract
        
        
type alias InteractStates =
    { edoIStates : Edo.EdoIStates
    , modelIStates : M.ModelIStates
    -- , chartIStates : MC.ChartIStates
    , controlIStates : Control.ControlIStates
    , refIStates : Ref.RefIStates
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
        passoInt = 0.001 
        relSaida = 100
        edoParam = Edo.EdoParam tini tfim passoInt relSaida [] Edo.rungeKutta
        chartData = []
        levelIStates = {h0 = (String.fromFloat h0), ag =  (String.fromFloat ag), ap = (String.fromFloat ap)}
        edoIStates = {tini = (String.fromFloat tini), tfim = (String.fromFloat tfim)}
        controlIStates = Control.PidIS Control.initPidIStates
        refIStates = Ref.Step1IS Ref.initStep1IStates
        interactStates = {edoIStates = edoIStates, modelIStates = M.LevelIS levelIStates, controlIStates = controlIStates, refIStates = refIStates}
    in
            { chartData = chartData
            , modelParam = M.LevelP levelParam
            , edoParam = edoParam
            , interactStates = interactStates
            -- , str = "teste"
            , chartsParam = [(MC.initChartParam Nothing)]
            , controlParam = Control.PidP Control.initPidParam
            , refParam = Ref.Step1P Ref.initStep1Param
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
          controlParam = .controlParam newModel
          refParam = .refParam newModel
          controller = Control.controllerFromControlParam controlParam
          refFunc = Ref.refFromRefParam refParam 
          refFuncAndController = {refFunc = refFunc,controller = controller}
          maybeRefFuncAndController = Just refFuncAndController
          data = M.runEdoModel modelParam edoParam maybeRefFuncAndController
      in
          { newModel | chartData = data }
            
    ChangeInteract interact ->
        let
           interactStates = .interactStates model
           edoIStates = .edoIStates interactStates
           modelIStates = .modelIStates interactStates
           controlIStates = .controlIStates interactStates
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
                            
                Control controlInteract ->
                    let 
                        controlIStatesNew =
                            Control.changeControlIStates controlIStates controlInteract 
                        interactStatesNew = {interactStates | controlIStates = controlIStatesNew}
                    in
                        {model | interactStates = interactStatesNew}

                Ref refInteract ->
                    let
                        refIStates = .refIStates interactStates
                        refIStatesNew = Ref.changeRefIStates refIStates refInteract 
                        interactStatesNew = {interactStates | refIStates = refIStatesNew}
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
            controlIStates = .controlIStates interactStates
            refIStates = .refIStates interactStates
                         
            edoParam = .edoParam model
            modelParam = .modelParam model
            controlParam = .controlParam model
            refParam = .refParam model
                       
            edoParamNew = Edo.updateEdoParam edoParam edoIStates
            modelParamNew = M.updateModelParam modelParam modelIStates
            controlParamNew = Control.updateControlParam controlParam controlIStates
            refParamNew = Ref.updateRefParam refParam refIStates
        in
            {model | edoParam = edoParamNew, modelParam = modelParamNew, controlParam = controlParamNew, refParam = refParamNew}

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
    controlIStates = .controlIStates interactStates
    refIStates = .refIStates interactStates
    chartData = .chartData model
    chartsParam = .chartsParam model
  in
    div []
        [
        div []
            (
            [ Edo.viewEdoIStates edoIStates (ChangeInteract << Edo)
            , M.viewModelIStates modelIStates (ChangeInteract << Models)
            , Control.viewControlIStates controlIStates (ChangeInteract << Control)
            , div [] [Ref.viewRefIStates refIStates (ChangeInteract << Ref)]
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

