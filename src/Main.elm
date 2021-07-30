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
    { chartData : DC.DataT1S
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
        data = []
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
      in
          case modelParam of
              M.LevelP levelParam ->
                  let
                    initState = ( .h0 <| .initState levelParam ) :: []
                    geoParam = .geoParam levelParam
                    data = DC.toChartDataT1S <| Edo.edoSolver edoParam (M.level levelParam) initState
                  in
                    { newModel | chartData = data }
            
    ChangeNumericInput interact valueStr ->
        let
           interactStates = .interactStates model
           edoIStates = .edoIStates interactStates
           tiniIS = {edoIStates | tini = valueStr}
           tfimIS = {edoIStates | tfim = valueStr}
           modelIStates = .modelIStates interactStates
        in
            case modelIStates of
                M.LevelIS levelIStates ->
                    let
                        h0IS = {levelIStates | h0 = valueStr}
                        agIS = {levelIStates | ag = valueStr}
                        apIS = {levelIStates | ap = valueStr}
                    in
                        case interact of
                            Edo Edo.Tini -> 
                                {model | interactStates = (updateEdoIStates tiniIS interactStates)}
                            Edo Edo.Tfim -> 
                                {model | interactStates = (updateEdoIStates tfimIS interactStates)}
                                    
                            Models (M.LevelI M.H0) -> 
                                {model | interactStates = (updateModelIStates (M.LevelIS h0IS) interactStates)}
                            Models (M.LevelI M.Ag) -> 
                                {model | interactStates = (updateModelIStates (M.LevelIS agIS) interactStates)}
                            Models (M.LevelI M.Ap) ->
                                {model | interactStates = (updateModelIStates (M.LevelIS apIS) interactStates)}

    UpdateParameters ->
        -- model
        let
            interactStates = .interactStates model
            edoIStates = .edoIStates interactStates
            tiniStr = .tini edoIStates
            tfimStr = .tfim edoIStates
            modelIStates = .modelIStates interactStates
        in
            case modelIStates of
                M.LevelIS levelIStates ->
                    let 
                        h0Str = .h0 levelIStates
                        agStr = .ag levelIStates
                        apStr = .ap levelIStates
                        listStr = [tiniStr,tfimStr,h0Str,agStr,apStr]
                        lstr = List.length listStr   
                        listValues = List.filterMap String.toFloat listStr
                        lvalues = List.length listValues
                    in
                    
                        if (lstr == lvalues) then
                            case listValues of
                                (tini::tfim::h0::ag::ap::[]) ->

                                    let
                                        edoParamOld = .edoParam model
                                        edoParam = {edoParamOld | tempo = tini, tfim = tfim}
                                        modelParam = .modelParam model
                                    in
                                        case modelParam of
                                            M.LevelP levelParam ->
                                                let 
                                                    initState = {h0 = h0}
                                                    geoParam = {ag=ag, ap=ap}
                                                    levelParamNew = {initState = initState, geoParam = geoParam}
                                                    modelParamNew = M.LevelP levelParamNew
                                                in
                                                    {model | modelParam = modelParamNew, edoParam = edoParam}

                                _ ->
                                    model

                        else
                            model


updateEdoParam : Edo.EdoParam -> Model -> Model
updateEdoParam edoParam model =
    { model | edoParam = edoParam }
        
updateTini : Float -> Edo.EdoParam -> Edo.EdoParam
updateTini tini edoParam =
        {edoParam | tempo = tini}

updateH0 : Float -> DataILevel -> DataILevel
updateH0 h0 dataLevel =
    {dataLevel | initState = h0}

           
        
        
        
        
------------------------------------------------
-- view
------------------------------------------------
        
view : Model -> Html Msg
view model =
  let
    interactStates = .interactStates model
    edoIStates = .edoIStates interactStates
    modelIStates = .modelIStates interactStates
    tiniStr = .tini edoIStates
    tfimStr = .tfim edoIStates
  in
      
    div []
        [ parameterInteractiveDiv "tini  " "" tiniStr (ChangeNumericInput (Edo Edo.Tini))
        , parameterInteractiveDiv "tfim  " "" tfimStr (ChangeNumericInput (Edo Edo.Tfim)) 
        , viewModelIStates modelIStates
        , div [] []
        , button [ onClick RunEdo ] [ text "Edo" ]
        , chartContainer <| chart4 model.chartData 
        ]
 
        
viewModelIStates : M.ModelIStates -> Html Msg
viewModelIStates modelIStates = 
    case modelIStates of
        M.LevelIS levelIStates ->
            viewISLevel levelIStates

viewISLevel : M.LevelIStates -> Html Msg
viewISLevel levelIStates =
    let
        h0Str = .h0 levelIStates
        agStr = .ag levelIStates
        apStr = .ap levelIStates
    in 
        div [] 
            [ parameterInteractiveDiv "h0    " "" h0Str (ChangeNumericInput (Models (M.LevelI M.H0)))
            , parameterInteractiveDiv "A     " "" agStr (ChangeNumericInput (Models (M.LevelI M.Ag)))
            , parameterInteractiveDiv "a     " "" apStr (ChangeNumericInput (Models (M.LevelI M.Ap)))
            ]

parameterInteractiveDiv : String -> String -> String -> (String -> Msg) -> Html Msg
parameterInteractiveDiv texto pholder valor msg =
    div []
    [ text texto
    , input [ placeholder pholder, value valor, onInput msg] []
    ]

chartContainer chart =
  div [  style "height" "300px"
      , style "width" "300px"]
      [
        chart
      ]
    

chart4 data =     
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
