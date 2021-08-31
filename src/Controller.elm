module Controller exposing (..)

import EdoSolver as Edo
import Element as E
import Controller.PID as PID


------------------------------------------------
------------------------------------------------
-- Controller
------------------------------------------------
------------------------------------------------
------------------------------------------------
-- Model
------------------------------------------------

type Type
    = PID

type Model
    = PidModel PID.Model
      
init : Type -> Model
init controlType =
    case controlType of
        PID -> PidModel PID.init

controllerFromModel : Model -> Edo.Controller
controllerFromModel model =
    case model of
        PidModel pidModel ->
            PID.controllerFromModel pidModel

                
update : Msg -> Model -> Model
update msg model =
    case msg of
        PidMsg pidMsg ->
            case model of
                PidModel pidModel ->
                    PidModel <| PID.update pidMsg pidModel



------------------------------------------------
-- Msg
------------------------------------------------


type Msg
    = PidMsg PID.Msg



------------------------------------------------
-- ControlView
------------------------------------------------

view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg =
    case model of
        PidModel pidModel ->
            PID.view pidModel (msgToMainMsg << PidMsg)
