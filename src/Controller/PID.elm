module Controller.PID exposing (..)

import EdoSolver as Edo
import Element as E
import UI

------------------------------------------------
------------------------------------------------
-- PID
------------------------------------------------
------------------------------------------------
------------------------------------------------
-- PID Param
------------------------------------------------


type alias Model =
    { kp : Float, ki : Float, kd : Float
    , kpStr : String, kiStr : String, kdStr : String }


init : Model
init =
    { kp = 0.0
    , ki = 0.0
    , kd = 0.0
    , kpStr = "0"
    , kiStr = "0"
    , kdStr = "0"
    }
    
    
controllerFromModel : Model -> Edo.Controller
controllerFromModel model =
    let
        kp =
            .kp model

        ki =
            .ki model

        kd =
            .kd model
    in
    pid kp ki kd


update : Msg -> Model -> Model
update msg model =
    case msg of
        PidKp valueStr ->
            let
                kp = .kp model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault kp maybeVal
            in
            { model | kpStr = valueStr, kp = val }

        PidKi valueStr ->
            let
                ki = .ki model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ki maybeVal
            in
            { model | kiStr = valueStr, ki = val }

        PidKd valueStr ->
            let
                kd = .kd model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault kd maybeVal
            in
            { model | kdStr = valueStr, kd = val }



------------------------------------------------
-- PIDInteract
------------------------------------------------


type Msg
    = PidKp String
    | PidKi String
    | PidKd String



------------------------------------------------
-- PID View
------------------------------------------------

view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg =
    let
        kpStr =
            .kpStr model

        kiStr =
            .kiStr model

        kdStr =
            .kdStr model
    in
    E.column [E.spacing 10, E.padding 20, E.centerX]
        [ UI.heading "Controller"
        , E.row [E.spacing 35]
                [ UI.textField kpStr "Kp" (msgToMainMsg << PidKp)
                , UI.textField kiStr "Ki" (msgToMainMsg << PidKi)
                , UI.textField kdStr "Kd" (msgToMainMsg << PidKd)
                ]
        ]


------------------------------------------------
-- PID Functions
------------------------------------------------

pid : Float -> Float -> Float -> Edo.ControlMemory -> Edo.Error -> Edo.Passo -> Edo.Tempo -> Edo.State -> ( Edo.ControlEffort, Edo.ControlMemory )
pid kp ki kd mem errors passo tempo xs =
    let
        error =
            Maybe.withDefault 0.0 (List.head errors)
                
        filterParam = 0.05
        fP = 1.0/filterParam

        ( error_state, integral_mem ) =
            case mem of
                [ err_mem, int_mem ] ->
                    ( err_mem, int_mem )

                _ ->
                    ( -fP*error, 0.0 ) -- Condicao inicial para o filtro da derivada do erro

        prop =
            kp * error

        integral_error =
            integral_mem + error * passo

        integral =
            ki * integral_error

        errorfsist : Edo.FuncSistUncontrolled
        errorfsist t xs2 = case xs2 of 
                              (x :: []) -> -fP*x - fP*fP*error :: []
                              _ -> -fP * error :: [] -- para derror ficar zero

        derror_aux = Maybe.withDefault (-fP * error)
                     <| List.head
                     <| Edo.eulerSolver errorfsist passo tempo [error_state]
                         
        derror = derror_aux + fP * error
                     
        dif =
            kd * derror
    in
    ( [ prop + integral + dif, prop, integral, dif ], [ derror_aux, integral_error ] )
