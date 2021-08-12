module Controller exposing (..)

import EdoSolver as Edo
import Html exposing (Html,div,text,input,span)
import Html.Attributes exposing (type_,placeholder,value)
import Html.Events exposing (onInput)

------------------------------------------------
------------------------------------------------
-- Controller
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- ControlParam
------------------------------------------------

type ControlParam 
    = PidP PidParam

updateControlParam : ControlParam -> ControlIStates -> ControlParam
updateControlParam controlParam controlIStates =
    case controlParam of
        PidP pidParam ->
            case controlIStates of
                PidIS pidIStates ->
                    PidP <| updatePidParam pidParam pidIStates
    
controllerFromControlParam : ControlParam -> Edo.Controller 
controllerFromControlParam controlParam = 
    case controlParam of
        PidP pidParam ->
            pidFromPidParam pidParam
      
------------------------------------------------
-- ControlIStates
------------------------------------------------
      
type ControlIStates 
    = PidIS PidIStates

changeControlIStates : ControlIStates -> ControlInteract -> ControlIStates
changeControlIStates controlIStates controlInteract =
    case controlInteract of
        PidI pidInteract ->
            case controlIStates of
                PidIS pidIStates ->
                    PidIS <| changePidIStates pidIStates pidInteract
        
        
------------------------------------------------
-- ControlInteract
------------------------------------------------

type ControlInteract
    = PidI PidInteract

      
------------------------------------------------
-- ControlView
------------------------------------------------

viewControlIStates : ControlIStates -> (ControlInteract -> msg) -> Html msg
viewControlIStates controlIStates controlInteractToMsg =
    case controlIStates of
        PidIS pidIStates ->
            pidView pidIStates (controlInteractToMsg << PidI)
      
      
------------------------------------------------
------------------------------------------------
-- PID
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- PID Param
------------------------------------------------

type alias PidParam =
    {kp:Float,ki:Float,kd:Float} 
        
updatePidParam : PidParam -> PidIStates -> PidParam
updatePidParam pidParam pidIStates =
    let
        kpStr = .kpStr pidIStates
        kiStr = .kiStr pidIStates
        kdStr = .kdStr pidIStates
        listStr = [kpStr,kiStr,kdStr]
        listValues = List.filterMap String.toFloat listStr
    in
        case listValues of
            (kp::ki::kd::[]) ->
                {pidParam | kp=kp, ki=ki, kd=kd} 

            _ -> pidParam
                 
pidFromPidParam : PidParam -> Edo.Controller
pidFromPidParam pidParam = 
    let 
        kp = .kp pidParam
        ki = .ki pidParam 
        kd = .kd pidParam
    in
        pid kp ki kd
        
------------------------------------------------
-- PIDIStates
------------------------------------------------

type alias PidIStates =
    {kpStr:String, kiStr:String, kdStr:String}

changePidIStates : PidIStates -> PidInteract -> PidIStates
changePidIStates pidIStates pidInteract =
    case pidInteract of
        PidKp valueStr -> {pidIStates | kpStr = valueStr}
        PidKi valueStr -> {pidIStates | kiStr = valueStr}
        PidKd valueStr -> {pidIStates | kdStr = valueStr}
    
                          
------------------------------------------------
-- PIDInteract
------------------------------------------------

type PidInteract
    = PidKp String
    | PidKi String
    | PidKd String
    
      
------------------------------------------------
-- PID View
------------------------------------------------

pidView : PidIStates -> (PidInteract -> msg) -> Html msg
pidView pidIStates pidInteractToMsg=
    let 
        kpStr = .kpStr pidIStates
        kiStr = .kiStr pidIStates
        kdStr = .kdStr pidIStates
    in
    span [] [ parameterInteractiveDiv "kp" "" kpStr (pidInteractToMsg << PidKp)
            , parameterInteractiveDiv "ki" "" kiStr (pidInteractToMsg << PidKi)
            , parameterInteractiveDiv "kd" "" kdStr (pidInteractToMsg << PidKd)
            ]

        
------------------------------------------------
-- PID Functions
------------------------------------------------
      
initPidParam : PidParam
initPidParam = {kp=0.0, ki=0.0, kd=0.0}

initPidIStates : PidIStates
initPidIStates = {kpStr = "0", kiStr = "0", kdStr = "0"}

pid : Float -> Float -> Float -> Edo.ControlMemory -> Edo.Error -> Edo.Passo -> Edo.Tempo -> Edo.State -> (Edo.ControlEffort, Edo.ControlMemory)
pid kp ki kd mem errors passo tempo xs =
    let
        error = Maybe.withDefault 0.0 (List.head errors)
        (error_ant, integral_mem) =
            case mem of
                [err_mem, int_mem] -> (err_mem,int_mem)
                _ -> (error,0.0)

        prop = kp*error

        integral_error = integral_mem + error*passo
        integral = ki*integral_error

        dif_error = (error-error_ant)/passo
        dif = kd*dif_error
    in
        ([(prop+integral+dif)], [error,integral_error])

refTeste : Float -> Edo.Tempo -> Edo.State -> Edo.Ref
refTeste val tempo xs =
    [val]

    
-- type alias RefFunction = Tempo -> State -> Ref

parameterInteractiveDiv : String -> String -> String -> (String -> msg) -> Html msg
parameterInteractiveDiv texto pholder valor strToMsg =
    span []
    [ text texto
    , input [type_ "number", placeholder pholder, value valor, onInput <| strToMsg ] []
    ]
    
-- parameterInteractiveDiv : String -> String -> String -> Html msg
-- parameterInteractiveDiv texto pholder valor =
--     span []
--     [ text texto
--     , input [type_ "number", placeholder pholder, value valor ] []
--     ]
