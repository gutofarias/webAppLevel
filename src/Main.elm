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

type alias Model =
    { chartData : DC.ChartData
    , modelParam : M.ModelParam
    , edoParam : Edo.EdoParam
    , interactStates : InteractStates
    -- , str : String
    , chartsParam : List MC.ChartParam 
    , controlParam : Control.ControlParam
    , refParam : Ref.RefParam
    , tfimAnimation : Float
    , animating : Bool
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
    , refIStates : Ref.RefIStates
    }

updateEdoIStates : Edo.EdoIStates -> InteractStates -> InteractStates
updateEdoIStates edoIStates interactStates =
    {interactStates | edoIStates = edoIStates}
    
------------------------------------------------
-- init
------------------------------------------------

init : () -> (Model, Cmd Msg) 
init () =
    let
        tini = 0.0 
        tfim = 10.0
        passoInt = 0.001 
        relSaida = 100
        edoParam = Edo.EdoParam tini tfim passoInt relSaida [] Edo.rungeKutta
        chartData = []
        edoIStates = {tini = (String.fromFloat tini), tfim = (String.fromFloat tfim)}
        refIStates = Ref.Step1IS Ref.initStep1IStates
        interactStates = {edoIStates = edoIStates, refIStates = refIStates}
    in
           ({ chartData = chartData
            , modelParam = M.initModelParam M.Level
            , edoParam = edoParam
            , interactStates = interactStates
            -- , str = "teste"
            , tfimAnimation = 0.0
            , animating = False
            , chartsParam = [(MC.initChartParam Nothing)]
            , controlParam = Control.PidP Control.initPidParam
            , refParam = Ref.Step1P Ref.initStep1Param
            }, Cmd.none)

        
------------------------------------------------
-- Subscriptions
------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
    case model.animating of
        False ->
            Sub.none
        True ->
            Browser.Events.onAnimationFrameDelta (Tick)
        
        
------------------------------------------------
-- update and Msg
------------------------------------------------
            
type Msg
    = RunEdo
    | ChangeInteract Interact
    | UpdateParameters 
    -- | ChangeStr String
    | Tick Float
    | RunAnimation

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      
    RunEdo -> 
      let  
          animating = False
          newModel = Tuple.first <| update UpdateParameters model
          edoParam = .edoParam newModel 
          modelParam = .modelParam newModel
          controlParam = .controlParam newModel
          refParam = .refParam newModel
                     
          controller = Control.controllerFromControlParam controlParam
          refFunc = Ref.refFromRefParam refParam 
          refFuncAndController = {refFunc = refFunc,controller = controller}
          maybeRefFuncAndController = Just refFuncAndController
                                      
          controlMem = []
          newEdoParam = {edoParam | controlMemory = controlMem}
                                      
          data = M.runEdoModel modelParam newEdoParam maybeRefFuncAndController
      in
          ({ newModel | chartData = data, animating = animating, edoParam = newEdoParam }, Cmd.none)
            
    ChangeInteract interact ->
        let
           interactStates = .interactStates model
           edoIStates = .edoIStates interactStates
        in
            case interact of
                Edo edoInteract ->
                    let
                        edoIStatesNew = Edo.changeEdoIStates edoIStates edoInteract 
                        interactStatesNew = {interactStates | edoIStates = edoIStatesNew}
                    in
                        ({model | interactStates = interactStatesNew}, Cmd.none)
                        
                Models modelInteract ->
                    let
                        modelParam = .modelParam model
                        modelParamNew = M.changeModelParam modelParam modelInteract
                    in 
                        ({model | modelParam = modelParamNew}, Cmd.none)
                            
                Control controlInteract ->
                    let 
                        controlParam = .controlParam model
                        controlParamNew = Control.changeControlParam controlParam controlInteract
                    in
                        ({model | controlParam = controlParamNew}, Cmd.none)

                Ref refInteract ->
                    let
                        refIStates = .refIStates interactStates
                        refIStatesNew = Ref.changeRefIStates refIStates refInteract 
                        interactStatesNew = {interactStates | refIStates = refIStatesNew}
                    in
                        ({model | interactStates = interactStatesNew}, Cmd.none)

                MChart chartID chartInteract -> 
                    let
                        chartsParam = .chartsParam model
                        newChartsParam = MC.chartIndividualInteractAction chartID chartsParam chartInteract
                    in
                        ({model | chartsParam = newChartsParam}, Cmd.none)

                MCharts chartsInteract ->
                    let
                        chartsParam = .chartsParam model
                        newChartsParam = MC.chartsInteractAction chartsParam chartsInteract
                    in
                        ({model | chartsParam = newChartsParam}, Cmd.none)


    UpdateParameters ->
        let
            interactStates = .interactStates model
            edoIStates = .edoIStates interactStates
            refIStates = .refIStates interactStates
                         
            edoParam = .edoParam model
            refParam = .refParam model
                       
            edoParamNew = Edo.updateEdoParam edoParam edoIStates
            refParamNew = Ref.updateRefParam refParam refIStates
        in
            ({model | edoParam = edoParamNew, refParam = refParamNew}, Cmd.none)

    Tick dTime ->
        let
            animating = .animating model
        in
        case animating of
            False -> 
                (model, Cmd.none)
            True -> 
                let
                    dTimeSec = dTime/1000.0
                    edoParam = .edoParam model
                    tfimAnimation = .tfimAnimation model
                    tempo = .tempo edoParam
                    tfim = .tfim edoParam
                    newTfim = if (tfim + dTimeSec) >= tfimAnimation then
                                  tfimAnimation
                              else
                                  tfim + dTimeSec
                    edoParam2 = {edoParam | tfim = newTfim}
                in
                if (tempo >= tfimAnimation) then
                    let
                        newAnimating = False
                    in
                    ({model | animating=newAnimating}, Cmd.none)
                else
                let
                    modelData = .chartData model
                    modelParam = .modelParam model
                    controlParam = .controlParam model
                    refParam = .refParam model
                    controller = Control.controllerFromControlParam controlParam
                    refFunc = Ref.refFromRefParam refParam 
                    refFuncAndController = {refFunc = refFunc,controller = controller}
                    maybeRefFuncAndController = Just refFuncAndController
                    (data,newEdoParam) = M.runAnimationModel modelParam edoParam2 maybeRefFuncAndController
                    dataFirst = Maybe.withDefault (DC.TS1 {t=5.0, x1=50.0}) <| List.head data 
                    xs = DC.xsFromDatum dataFirst
                    -- xs2 = Debug.log "teste" xs
                    -- xs = [1.0]
                    modelParamNew = M.updateModelParamFromXs xs modelParam
                    newData = data ++ modelData
                in
                -- ({model | elapsedTime = newElapsedTime}, Cmd.none)
                ({ model | chartData = newData, edoParam = newEdoParam
                 , modelParam = modelParamNew
                 }, Cmd.none)

    RunAnimation ->
        let
            animating = True
            newModel = Tuple.first <| update UpdateParameters model
            edoParam = .edoParam newModel
            controlMem = []

            tfim = .tfim edoParam
            newEdoParam = {edoParam | tfim = 0.0, controlMemory = controlMem}
            chartData = []
            
        in
           ({newModel | animating = animating, tfimAnimation = tfim, edoParam = newEdoParam, chartData = chartData }, Cmd.none)

    -- ChangeStr str -> 
    --      {model | str = str}

        
        
------------------------------------------------
-- View
------------------------------------------------
        
view : Model -> Html Msg
view model =
  let
    controlParam = .controlParam model
    interactStates = .interactStates model
    edoIStates = .edoIStates interactStates
    refIStates = .refIStates interactStates
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
            [ Edo.viewEdoIStates edoIStates (ChangeInteract << Edo)]
        , div [style "height" "30px"]
            [ M.viewModel modelParam (ChangeInteract << Models)]
        , div [style "height" "30px"]
            [Control.viewController controlParam (ChangeInteract << Control)]
        , div [style "height" "30px"]
            [Ref.viewRefIStates refIStates (ChangeInteract << Ref)]
        , div [style "height" "30px"]
            [ button [ onClick RunEdo ] [ text "Edo" ]
            , button [ onClick RunAnimation ] [ text "Animation" ]
            -- , text (String.fromFloat <| .elapsedTime model)
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

