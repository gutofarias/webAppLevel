module Main exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Models as M

import Browser
import Html exposing (Html, button, div, text, pre, input)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onClick, onInput)

import Chart as C
import Chart.Attributes as CA






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
    }

    
type Interact
    = Edo Edo.EdoInteract
    | Models M.ModelInteract
        
        
type alias InteractStates =
    { edoIStates : Edo.EdoIStates
    , modelIStates : M.ModelIStates
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
        data = DC.Nodata
        levelIStates = {h0 = (String.fromFloat h0), ag =  (String.fromFloat ag), ap = (String.fromFloat ap)}
        edoIStates = {tini = (String.fromFloat tini), tfim = (String.fromFloat tfim)}
        interactStates = {edoIStates = edoIStates, modelIStates = M.LevelIS levelIStates}
    in
            { chartData = data
            , modelParam = M.LevelP levelParam
            , edoParam = edoParam
            , interactStates = interactStates
            }

        
        
        
        
------------------------------------------------
-- update and Msg
------------------------------------------------
            
type Msg
    = RunEdo
    | ChangeNumericInput Interact String 
    | UpdateParameters 

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


        
        
------------------------------------------------
-- View
------------------------------------------------
        
view : Model -> Html Msg
view model =
  let
    interactStates = .interactStates model
    edoIStates = .edoIStates interactStates
    modelIStates = .modelIStates interactStates
  in
      
    div []
        [ Edo.viewEdoIStates edoIStates (ChangeNumericInput << Edo)
        , M.viewModelIStates modelIStates (ChangeNumericInput << Models)
        , div [] []
        , button [ onClick RunEdo ] [ text "Edo" ]
        , chartContainer <| chart4 model.chartData 
        ]
         

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
