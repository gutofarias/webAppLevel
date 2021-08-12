module Models exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Html exposing (Html,div,text,input,span,pre,label)
import Html.Attributes exposing (style, placeholder, value, type_)
import Controller
import Html.Events exposing (onInput)

------------------------------------------------
------------------------------------------------
-- MODELOS EM GERAL
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- ModelParam
------------------------------------------------

type ModelParam 
    = LevelP LevelParam
      
      
updateModelParam : ModelParam -> ModelIStates -> ModelParam
updateModelParam modelParam modelIStates =
    case modelParam of
        LevelP levelParam ->
            case modelIStates of
                LevelIS levelIStates ->
                   LevelP <| updateLevelParam levelParam levelIStates

                       
------------------------------------------------
-- ModelIStates
------------------------------------------------

type ModelIStates
    = LevelIS LevelIStates
      

changeModelIStates : ModelIStates -> ModelInteract -> ModelIStates
changeModelIStates modelIStates modelInteract =
    case modelInteract of
        LevelI levelInteract ->
            case modelIStates of
                LevelIS levelIStates ->
                    LevelIS <| changeLevelIStates levelIStates levelInteract 
                        
                        
                        
------------------------------------------------
-- ModelInteract
------------------------------------------------
                        
type ModelInteract
    = LevelI LevelInteract

      
------------------------------------------------
-- viewModel
------------------------------------------------
                        
viewModelIStates : ModelIStates -> (ModelInteract -> msg) -> Html msg
viewModelIStates modelIStates modelInteractToMsg = 
    case modelIStates of
        LevelIS levelIStates ->
            viewLevelIStates levelIStates (modelInteractToMsg << LevelI)
                
                
parameterInteractiveDiv : String -> String -> String -> (String -> msg) -> Html msg
parameterInteractiveDiv texto pholder valor strToMsg =
    span []
    [ label [] [text texto]
    , input [type_ "number", placeholder pholder, value valor, onInput <| strToMsg ] []
    ]
                
------------------------------------------------
-- runEdoModel
------------------------------------------------

runEdoModel : ModelParam -> Edo.EdoParam -> Maybe {refFunc: Edo.RefFunction, controller:Edo.Controller} -> DC.ChartData
runEdoModel modelParam edoParam maybeRefAndController =
     case modelParam of
         LevelP levelParam ->
             runEdoLevel levelParam edoParam maybeRefAndController
                 
                 
                 
                 
                 
                 
                 
------------------------------------------------
------------------------------------------------
-- Nível
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- LevelParam
------------------------------------------------

type alias LevelParam = { initState:LevelInitState, geoParam:LevelGeoParam }

type alias LevelInitState = { h0 : Float }
type alias LevelGeoParam = { ag : Float, ap : Float }
    
updateLevelParam : LevelParam -> LevelIStates -> LevelParam
updateLevelParam levelParam levelIStates =
    let 
        h0Str = .h0 levelIStates
        agStr = .ag levelIStates
        apStr = .ap levelIStates
        listStr = [h0Str,agStr,apStr]
        listValues = List.filterMap String.toFloat listStr
    in
        case listValues of
            (h0::ag::ap::[]) ->
                let
                   initState = {h0=h0}
                   geoParam = {ag=ag, ap=ap}
                in
                   {levelParam | initState = initState, geoParam = geoParam}
            _ -> 
                levelParam
                    
------------------------------------------------
-- LevelIStates
------------------------------------------------

type alias LevelIStates = { h0:String, ag:String, ap:String }
    
    
changeLevelIStates : LevelIStates -> LevelInteract -> LevelIStates
changeLevelIStates levelIStates levelInteract =
    case levelInteract of
        H0 valueStr -> {levelIStates | h0 = valueStr}
        Ag valueStr -> {levelIStates | ag = valueStr}
        Ap valueStr -> {levelIStates | ap = valueStr}
        

------------------------------------------------
-- LevelInteract
------------------------------------------------

type LevelInteract
    = H0 String
    | Ag String
    | Ap String
    
    
------------------------------------------------
-- viewLevel
------------------------------------------------
    
viewLevelIStates : LevelIStates -> (LevelInteract -> msg) -> Html msg
viewLevelIStates levelIStates levelInteractToMsg = 
    let 
        h0Str = .h0 levelIStates
        agStr = .ag levelIStates
        apStr = .ap levelIStates
    in 
        div []
            [ parameterInteractiveDiv "h0  " "" h0Str (levelInteractToMsg << H0)
            , parameterInteractiveDiv "A   " "" agStr (levelInteractToMsg << Ag)
            , parameterInteractiveDiv "a   " "" apStr (levelInteractToMsg << Ap)
            ]
    

------------------------------------------------
-- runEdoLevel
------------------------------------------------

runEdoLevel : LevelParam -> Edo.EdoParam -> Maybe {refFunc: Edo.RefFunction, controller:Edo.Controller} -> DC.ChartData
runEdoLevel levelParam edoParam maybeRefFuncAndController =
    case maybeRefFuncAndController of
        Nothing ->
            runEdoLevelUncontrolled levelParam edoParam

        Just refFuncAndController ->
            let
                refFunc = .refFunc refFuncAndController
                controller = .controller refFuncAndController
            in
                runEdoLevelControlled levelParam edoParam refFunc controller
                
runEdoLevelControlled : LevelParam -> Edo.EdoParam -> Edo.RefFunction  -> Edo.Controller -> DC.ChartData
runEdoLevelControlled levelParam edoParam refFunc controller =
        let
            initState = ( .h0 <| .initState levelParam ) :: []
            geoParam = .geoParam levelParam
            edoSist = Edo.Controlled
                      { refFunc = refFunc
                      , outputFunc = outputX1
                      , controller = controller
                      , sistFunc = (levelSyst levelParam)}
        in
            DC.toChartDataT1S <| Tuple.first <| Edo.edoSolver edoParam edoSist initState

runEdoLevelUncontrolled : LevelParam -> Edo.EdoParam -> DC.ChartData
runEdoLevelUncontrolled levelParam edoParam =
        let
            initState = ( .h0 <| .initState levelParam ) :: []
            geoParam = .geoParam levelParam
            edoSist = Edo.Uncontrolled (levelSyst levelParam [0.0])
        in
            DC.toChartDataT1S <| Tuple.first <| Edo.edoSolver edoParam edoSist initState
                
------------------------------------------------
-- levelSyst
------------------------------------------------
        
levelSyst : LevelParam -> Edo.ControlEffort -> Edo.Tempo -> Edo.State -> Edo.DState
levelSyst param us t state =
    let
       ag = .ag <| .geoParam <| param
       ap = .ap <| .geoParam <| param
       g = 9.28
       u = Maybe.withDefault 0.0 (List.head us)
    in
       case state of
           h::[] ->
               let
                   hn = if h >= 0.0 then h else 0.0
               in 
                   -(ap/ag)*sqrt(2.0*g)*sqrt(hn) + (u/ag) :: []
           h::ls -> 
               let
                   hn = if h >= 0.0 then h else 0.0
               in 
                   -(ap/ag)*sqrt(2.0*g)*sqrt(hn) + (u/ag) :: []
           _ -> 0.0 :: []
      
-- type alias FuncSist = Tempo -> State -> DState

outputX1 : Edo.Tempo -> Edo.State -> Edo.Output
outputX1 tempo xs =
    case xs of
        (x::ls) -> [x]
        _ -> xs
