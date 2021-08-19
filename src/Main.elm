module Main exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Models as M
import Controller as Control 
import Reference as Ref

import Browser
import Browser.Events
import Html exposing (Html, button, div, text, pre, input, label, select, option, span, section)
import Html.Attributes exposing (style, placeholder, value, for, name, selected)
import Html.Events exposing (onClick, onInput)

import Chart as C
import Chart.Attributes as CA
import MyChart as MC


------------------------------------------------
-- main
------------------------------------------------

main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

      
      
------------------------------------------------
-- Model and Data Types
------------------------------------------------

type alias DataILevel = {initState:Float, levelParam:M.LevelParam}

type BigModel
    = SolvingEdo Model
    | Animation Model Edo.EdoParam Edo.State

modelFromBigModel : BigModel -> Model
modelFromBigModel bigModel = 
    case bigModel of
        SolvingEdo model -> model
        Animation model _ _ -> model

updatingBigModelFromModel : BigModel -> Model -> BigModel
updatingBigModelFromModel bigModel newModel = 
    case bigModel of
        SolvingEdo _ -> SolvingEdo newModel
        Animation _ animatingEdoParam xs -> Animation newModel animatingEdoParam xs

type alias Model =
    { chartData : DC.ChartData
    , modelParam : M.ModelParam
    , edoParam : Edo.EdoParam
    , edoIStates : Edo.EdoIStates
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
        
        
------------------------------------------------
-- init
------------------------------------------------

init : () -> (BigModel, Cmd Msg) 
init () =
    let
        (edoParam, edoIStates) = Edo.initEdoParamAndIStates
    in
            (SolvingEdo
            { chartData = []
            , modelParam = M.initModelParam M.Level
            , edoParam = edoParam
            , edoIStates = edoIStates
            , chartsParam = [(MC.initChartParam Nothing)]
            , controlParam = Control.initControlParam Control.Pid
            , refParam = Ref.initRefParam Ref.Step1
            }, Cmd.none)
              

        
------------------------------------------------
-- Subscriptions
------------------------------------------------
subscriptions : BigModel -> Sub Msg
subscriptions bigModel =
    case bigModel of
        SolvingEdo _ -> Sub.none
        Animation _ _ _ -> 
            Browser.Events.onAnimationFrameDelta (Tick)
        
        
------------------------------------------------
-- update and Msg
------------------------------------------------
            
type Msg
    = RunEdo
    | ChangeInteract Interact
    | UpdateEdoParam 
    -- | ChangeStr String
    | Tick Float
    | RunAnimation

update : Msg -> BigModel -> (BigModel, Cmd Msg)
update msg bigModel =
  case msg of
    RunEdo -> 
      let  
          newModel = modelFromBigModel <| Tuple.first <| update UpdateEdoParam bigModel
          edoParam = .edoParam newModel 
          modelParam = .modelParam newModel
          controlParam = .controlParam newModel
          refParam = .refParam newModel
                     
          controller = Control.controllerFromControlParam controlParam
          refFunc = Ref.refFunctionFromRefParam refParam 
          refFuncAndController = {refFunc = refFunc,controller = controller}
          maybeRefFuncAndController = Just refFuncAndController
                                      
          controlMem = []
          newEdoParam = {edoParam | controlMemory = controlMem}
                                      
          data = M.runEdoModel modelParam newEdoParam maybeRefFuncAndController
      in
          ( SolvingEdo { newModel | chartData = data, edoParam = newEdoParam }, Cmd.none)
            
    ChangeInteract interact ->
        let
            model = modelFromBigModel bigModel
        in
        case interact of
            Edo edoInteract ->
                let
                    edoIStates = .edoIStates model
                    edoIStatesNew = Edo.changeEdoIStates edoIStates edoInteract 
                    newModel = {model | edoIStates = edoIStatesNew}
                in
                    (updatingBigModelFromModel bigModel newModel, Cmd.none)

            Models modelInteract ->
                let
                    modelParam = .modelParam model
                    modelParamNew = M.changeModelParam modelParam modelInteract
                    newModel = {model | modelParam = modelParamNew}
                in 
                    (updatingBigModelFromModel bigModel newModel, Cmd.none)

            Control controlInteract ->
                let 
                    controlParam = .controlParam model
                    controlParamNew = Control.changeControlParam controlParam controlInteract
                    newModel = {model | controlParam = controlParamNew}
                in
                    (updatingBigModelFromModel bigModel newModel, Cmd.none)
                        
            Ref refInteract ->
                let
                    refParam = .refParam model
                    refParamNew = Ref.changeRefParam refParam refInteract 
                    newModel = {model | refParam = refParamNew}
                in
                    (updatingBigModelFromModel bigModel newModel, Cmd.none)

            MChart chartID chartInteract -> 
                let
                    chartsParam = .chartsParam model
                    newChartsParam = MC.chartIndividualInteractAction chartID chartsParam chartInteract
                    newModel = {model | chartsParam = newChartsParam}
                in
                    (updatingBigModelFromModel bigModel newModel, Cmd.none)

            MCharts chartsInteract ->
                let
                    chartsParam = .chartsParam model
                    newChartsParam = MC.chartsInteractAction chartsParam chartsInteract
                    newModel = {model | chartsParam = newChartsParam}
                in
                    (updatingBigModelFromModel bigModel newModel, Cmd.none)


    UpdateEdoParam ->
        let
            model = modelFromBigModel bigModel
            edoIStates = .edoIStates model
                         
            edoParam = .edoParam model
            edoParamNew = Edo.updateEdoParam edoParam edoIStates
            newModel = {model | edoParam = edoParamNew}
        in
            (updatingBigModelFromModel bigModel newModel, Cmd.none)

    Tick dTime ->
        case bigModel of
            SolvingEdo _ ->
                (bigModel, Cmd.none)
            Animation model animatingEdoParam xs ->
                let
                    modelEdoParam = .edoParam model
                    tfimAnimation = .tfim modelEdoParam
                                    
                    tempo = .tempo animatingEdoParam
                in
                if (Edo.checkEndTimeEps tempo tfimAnimation) then
                    (SolvingEdo model, Cmd.none)
                    
                else
                let
                    dTimeSec = dTime/1000.0
                               
                    tfimStep = .tfim animatingEdoParam
                    newTfimStep = min tfimAnimation (tfimStep + dTimeSec)
                                  
                    animatingEdoParam2 = {animatingEdoParam | tfim = newTfimStep}
                                         
                    modelData = .chartData model
                    modelParam = .modelParam model
                    controlParam = .controlParam model
                    refParam = .refParam model
                               
                    controller = Control.controllerFromControlParam controlParam
                    refFunc = Ref.refFunctionFromRefParam refParam 
                    refFuncAndController = {refFunc = refFunc,controller = controller}
                    maybeRefFuncAndController = Just refFuncAndController

                    stateUpdatedModelParam = M.updateModelParamFromXs xs modelParam
                                             
                    (data,newAnimationEdoParam) = M.runAnimationModel stateUpdatedModelParam animatingEdoParam2 maybeRefFuncAndController
                                                  
                    newXs = case data of
                                (d::ds) -> DC.xsFromDatum d
                                _ -> []
                                     
                    newData = data ++ modelData
                    newModel = { model | chartData = newData}
                in
                (Animation newModel newAnimationEdoParam newXs, Cmd.none)

    RunAnimation ->
        let
            updatedModel = modelFromBigModel <| Tuple.first <| update UpdateEdoParam bigModel
                           
            edoParam = .edoParam updatedModel
            modelParam = .modelParam updatedModel
                         
            controlMem = []
            animatingEdoParam = {edoParam | tfim = 0.0, controlMemory = controlMem}
            chartData = []
                        
            newModel = {updatedModel | chartData = chartData}
            xs = M.xsFromModelParam modelParam
        in
           (Animation newModel animatingEdoParam xs, Cmd.none)

        
------------------------------------------------
-- View
------------------------------------------------
        
view : BigModel -> Html Msg
view bigModel =
  let
    model = modelFromBigModel bigModel
    controlParam = .controlParam model
    edoIStates = .edoIStates model
    chartData = .chartData model
    chartsParam = .chartsParam model
    edoParam = .edoParam model
    refParamI = .refParam model
    refParam = case refParamI of
                   Ref.Step1P stepParam -> stepParam
    refmax = max (.iVal refParam) (.fVal refParam)
    modelParam = .modelParam model
    levelParam = case modelParam of
                     M.LevelP levelParam1 -> levelParam1
         
    h0 = 10.0
    hmaxExpected = max h0 refmax 
    data = .chartData model
    dataFirst = Maybe.withDefault (DC.TS1 {t=0.0, x1=10.0}) <| List.head data 
    xs = DC.xsFromDatum dataFirst
    us = DC.usFromDatum dataFirst
    level = Maybe.withDefault 10.0 <| List.head xs
    u = Maybe.withDefault 10.0 <| List.head us
    -- levellog = Debug.log "level" (level,hmaxExpected)
  in
    section []
        [ div [style "height" "30px"]
            [ Edo.viewEdo edoIStates (ChangeInteract << Edo)]
        , div [style "height" "30px"]
            [ M.viewModel modelParam (ChangeInteract << Models)]
        , div [style "height" "30px"]
            [Control.viewController controlParam (ChangeInteract << Control)]
        , div [style "height" "30px"]
            [Ref.viewRef refParamI (ChangeInteract << Ref)]
        , div [style "height" "30px"]
            [ button [ onClick RunEdo ] [ text "Edo" ]
            , button [ onClick RunAnimation ] [ text "Animation" ]
            , text (String.fromFloat <| .tempo edoParam)
            ]
        , span []
            (MC.chartsView chartData chartsParam (ChangeInteract << MCharts) (fcomposition23 ChangeInteract MChart))
        , M.levelSim level u 0.0 hmaxExpected levelParam
        -- , MC.chartView chartData chartParam (fcomposition23 ChangeInteract MChart)
        ]
         
fcomposition23 : (a -> b) -> (c -> d -> a) -> c -> d -> b 
fcomposition23 f2 f3 c = 
    f2 << (f3 c)

