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

main =
  Browser.sandbox { init = init, update = update, view = view }

-- Model
type alias DataILevel = {initState:Float, levelParam:M.LevelParam}

type alias Model = { btValue:Int, chartData:DC.DataT1S, dataLevel:DataILevel, edoParam:Edo.EdoParam, edoInteractStates : EdoInteractStates}


init : Model
init =
    let
        levelParam = M.LevelParam 1.0 0.1
        initState = 10.0 
        tini = 0.0 
        tfim = 10.0
        passoInt = 0.01 
        relSaida = 2
        edoParam = Edo.EdoParam tini tfim passoInt relSaida Edo.rungeKutta
        data = []
        edoInteractStates = initEdoInteractStates tini tfim initState levelParam.ag  levelParam.ap
    in
        Model 0 data (DataILevel initState levelParam) edoParam edoInteractStates

initEdoInteractStates : Float -> Float -> Float -> Float -> Float -> EdoInteractStates
initEdoInteractStates tini tfim h0 ag ap = 
    EdoInteractStates (String.fromFloat tini) (String.fromFloat tfim) (String.fromFloat h0) (String.fromFloat ag) (String.fromFloat ap)
            
type EdoInteract
    = Tini
    | Tfim
    | H0
    | Ag
    | Ap

type alias EdoInteractStates =
    { tini:String, tfim:String, h0:String, ag:String, ap:String }
            
type Msg
    = Increment
    | Decrement
    | Reset
    | Increment10
    | Decrement10
    | RunEdo
    | ChangeNumericInput EdoInteract String 
    | UpdateParameters 

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      { model | btValue = model.btValue + 1}

    Decrement ->
      { model | btValue = model.btValue - 1}

    Reset ->
      { model | btValue = 0}

    Increment10 ->
      { model | btValue = model.btValue + 10}

    Decrement10 ->
      { model | btValue = model.btValue - 10}
          
    RunEdo -> 
      let  
          newModel = update UpdateParameters model
          edoParam = .edoParam newModel 
          levelParam = .levelParam <| .dataLevel newModel
          initState = (.initState <| .dataLevel newModel) :: []
          data = DC.toChartDataT1S <| Edo.edoSolver edoParam (M.level levelParam) initState
      in
        { newModel | chartData = data }
            
    ChangeNumericInput edoInteract valueStr ->
        let
           interactList = .edoInteractStates model
           tiniIL = {interactList | tini = valueStr}
           tfimIL = {interactList | tfim = valueStr}
           h0IL = {interactList | h0 = valueStr}
           agIL = {interactList | ag = valueStr}
           apIL = {interactList | ap = valueStr}
        in
            case edoInteract of
                Tini -> {model | edoInteractStates = tiniIL}
                Tfim -> {model | edoInteractStates = tfimIL}
                H0 -> {model | edoInteractStates = h0IL}
                Ag -> {model | edoInteractStates = agIL}
                Ap -> {model | edoInteractStates = apIL}

    UpdateParameters ->
        let
            interactList = .edoInteractStates model
            tiniStr = .tini interactList
            tfimStr = .tfim interactList
            h0Str = .h0 interactList
            agStr = .ag interactList
            apStr = .ap interactList
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
                            dataLevelOld = .dataLevel model
                            dataLevel = {dataLevelOld | initState = h0, levelParam = {ag=ag, ap=ap}}

                        in
                            {model | dataLevel = dataLevel, edoParam = edoParam}

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

           
view : Model -> Html Msg
view model =
  let
    interactStates = .edoInteractStates model
    tiniStr = .tini interactStates
    tfimStr = .tfim interactStates
    h0Str =  .h0 interactStates
    agStr = .ag interactStates
    apStr = .ap interactStates
  in
      
    div []
        [ parameterInteractiveDiv "tini  " "" tiniStr (ChangeNumericInput Tini)
        , parameterInteractiveDiv "tfim  " "" tfimStr (ChangeNumericInput Tfim)
        , parameterInteractiveDiv "h0    " "" h0Str (ChangeNumericInput H0)
        , parameterInteractiveDiv "A     " "" agStr (ChangeNumericInput Ag)
        , parameterInteractiveDiv "a     " "" apStr (ChangeNumericInput Ap)
        , div [] []
        , button [ onClick RunEdo ] [ text "Edo" ]
        , chartContainer <| chart4 model.chartData 
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
