module Controller exposing (..)

import EdoSolver as Edo
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (placeholder, type_, value)
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

controllerFromControlParam : ControlParam -> Edo.Controller
controllerFromControlParam controlParam =
    case controlParam of
        PidP pidParam ->
            pidFromPidParam pidParam

                
changeControlParam : ControlParam -> ControlInteract -> ControlParam
changeControlParam controlParam controlInteract =
    case controlInteract of
        PidI pidInteract ->
            case controlParam of
                PidP pidParam ->
                    PidP <| changePidParam pidParam pidInteract



------------------------------------------------
-- ControlInteract
------------------------------------------------


type ControlInteract
    = PidI PidInteract



------------------------------------------------
-- ControlView
------------------------------------------------


viewController : ControlParam -> (ControlInteract -> msg) -> Html msg
viewController controlParam controlInteractToMsg =
    case controlParam of
        PidP pidParam ->
            pidView pidParam (controlInteractToMsg << PidI)



------------------------------------------------
------------------------------------------------
-- PID
------------------------------------------------
------------------------------------------------
------------------------------------------------
-- PID Param
------------------------------------------------


type alias PidParam =
    { kp : Float, ki : Float, kd : Float
    , kpStr : String, kiStr : String, kdStr : String }


initPidParam : PidParam
initPidParam =
    { kp = 0.0
    , ki = 0.0
    , kd = 0.0
    , kpStr = "0"
    , kiStr = "0"
    , kdStr = "0"
    }
    
    
pidFromPidParam : PidParam -> Edo.Controller
pidFromPidParam pidParam =
    let
        kp =
            .kp pidParam

        ki =
            .ki pidParam

        kd =
            .kd pidParam
    in
    pid kp ki kd


changePidParam : PidParam -> PidInteract -> PidParam
changePidParam pidParam pidInteract =
    case pidInteract of
        PidKp valueStr ->
            let
                kp = .kp pidParam
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault kp maybeVal
            in
            { pidParam | kpStr = valueStr, kp = val }

        PidKi valueStr ->
            let
                ki = .ki pidParam
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ki maybeVal
            in
            { pidParam | kiStr = valueStr, ki = val }

        PidKd valueStr ->
            let
                kd = .kd pidParam
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault kd maybeVal
            in
            { pidParam | kdStr = valueStr, kd = val }



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


pidView : PidParam -> (PidInteract -> msg) -> Html msg
pidView pidParam pidInteractToMsg =
    let
        kpStr =
            .kpStr pidParam

        kiStr =
            .kiStr pidParam

        kdStr =
            .kdStr pidParam
    in
    span []
        [ parameterInteractiveDiv "kp" "" kpStr (pidInteractToMsg << PidKp)
        , parameterInteractiveDiv "ki" "" kiStr (pidInteractToMsg << PidKi)
        , parameterInteractiveDiv "kd" "" kdStr (pidInteractToMsg << PidKd)
        ]



------------------------------------------------
-- PID Functions
------------------------------------------------

pid : Float -> Float -> Float -> Edo.ControlMemory -> Edo.Error -> Edo.Passo -> Edo.Tempo -> Edo.State -> ( Edo.ControlEffort, Edo.ControlMemory )
pid kp ki kd mem errors passo tempo xs =
    let
        error =
            Maybe.withDefault 0.0 (List.head errors)

        ( error_ant, integral_mem ) =
            case mem of
                [ err_mem, int_mem ] ->
                    ( err_mem, int_mem )

                _ ->
                    ( error, 0.0 )

        prop =
            kp * error

        integral_error =
            integral_mem + error * passo

        integral =
            ki * integral_error

        dif_error =
            (error - error_ant) / passo

        dif =
            kd * dif_error
    in
    ( [ prop + integral + dif, prop, integral, dif ], [ error, integral_error ] )


refTeste : Float -> Edo.Tempo -> Edo.State -> Edo.Ref
refTeste val tempo xs =
    [ val ]


parameterInteractiveDiv : String -> String -> String -> (String -> msg) -> Html msg
parameterInteractiveDiv texto pholder valor strToMsg =
    span []
        [ text texto
        , input [ type_ "number", placeholder pholder, value valor, onInput <| strToMsg ] []
        ]
