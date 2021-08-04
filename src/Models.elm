module Models exposing (..)

import EdoSolver as Edo
import DataConvert as DC
import Html exposing (Html,div,text,input)
import Html.Attributes exposing (style, placeholder, value)
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
      

changeModelIStates : ModelIStates -> ModelInteract -> String -> ModelIStates
changeModelIStates modelIStates modelInteract valueStr =
    case modelInteract of
        LevelI levelInteract ->
            case modelIStates of
                LevelIS levelIStates ->
                    LevelIS <| changeLevelIStates levelIStates levelInteract valueStr
                        
                        
                        
------------------------------------------------
-- ModelInteract
------------------------------------------------
                        
type ModelInteract
    = LevelI LevelInteract

      
------------------------------------------------
-- viewModel
------------------------------------------------
                        
viewModelIStates : ModelIStates -> (ModelInteract -> String -> msg) -> Html msg
viewModelIStates modelIStates modelInteractToMsg = 
    case modelIStates of
        LevelIS levelIStates ->
            viewLevelIStates levelIStates (modelInteractToMsg << LevelI)
                
                
parameterInteractiveDiv : String -> String -> String -> (String -> msg) -> Html msg
parameterInteractiveDiv texto pholder valor strToMsg =
    div []
    [ text texto
    , input [ placeholder pholder, value valor, onInput <| strToMsg ] []
    ]
                
------------------------------------------------
-- runEdoModel
------------------------------------------------

runEdoModel : ModelParam -> Edo.EdoParam -> DC.ChartData
runEdoModel modelParam edoParam =
     case modelParam of
         LevelP levelParam ->
             runEdoLevel levelParam edoParam
                 
                 
                 
                 
                 
                 
                 
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
    
    
changeLevelIStates : LevelIStates -> LevelInteract -> String -> LevelIStates
changeLevelIStates levelIStates levelInteract valueStr =
    case levelInteract of
        H0 -> {levelIStates | h0 = valueStr}
        Ag -> {levelIStates | ag = valueStr}
        Ap -> {levelIStates | ap = valueStr}
        

------------------------------------------------
-- LevelInteract
------------------------------------------------

type LevelInteract = H0 | Ag | Ap
    
    
------------------------------------------------
-- viewLevel
------------------------------------------------
    
viewLevelIStates : LevelIStates -> (LevelInteract -> String -> msg) -> Html msg
viewLevelIStates levelIStates levelInteractToMsg = 
    let 
        h0Str = .h0 levelIStates
        agStr = .ag levelIStates
        apStr = .ap levelIStates
    in 
        div []
            [ parameterInteractiveDiv "h0  " "" h0Str (levelInteractToMsg H0)
            , parameterInteractiveDiv "A   " "" agStr (levelInteractToMsg Ag)
            , parameterInteractiveDiv "a   " "" apStr (levelInteractToMsg Ap)
            ]
    

------------------------------------------------
-- runEdoLevel
------------------------------------------------

runEdoLevel : LevelParam -> Edo.EdoParam -> DC.ChartData
runEdoLevel levelParam edoParam =
        let
            initState = ( .h0 <| .initState levelParam ) :: []
            geoParam = .geoParam levelParam
        in
            DC.toChartDataT1S <| Edo.edoSolver edoParam (levelSyst levelParam) initState

                
------------------------------------------------
-- levelSyst
------------------------------------------------
        
levelSyst : LevelParam -> Edo.Tempo -> Edo.State -> Edo.DState
levelSyst param t state =
    let
       ag = .ag <| .geoParam <| param
       ap = .ap <| .geoParam <| param
       g = 9.28
    in
       case state of
           h::[] ->
               let
                   hn = if h >= 0.0 then h else 0.0
               in 
                   -(ap/ag)*sqrt(2.0*g)*sqrt(hn) :: []
           h::ls -> 
               let
                   hn = if h >= 0.0 then h else 0.0
               in 
                   -(ap/ag)*sqrt(2.0*g)*sqrt(hn) :: []
           _ -> 0.0 :: []
      
-- type alias FuncSist = Tempo -> State -> DState
