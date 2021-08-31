module Reference exposing (..)

import EdoSolver as Edo

import Element as E
import Reference.Step1 as Step1


------------------------------------------------
------------------------------------------------
-- References
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- Model
------------------------------------------------

type Type
    = Step1

type Model
    = Step1Model Step1.Model

init : Type -> Model
init refType = 
    case refType of
        Step1 -> Step1Model Step1.init
    
refFunctionFromModel : Model -> Edo.RefFunction 
refFunctionFromModel model = 
    case model of
        Step1Model step1Model ->
            Step1.refFunctionFromModel step1Model
      
update : Msg -> Model -> Model
update msg model =
    case msg of
        Step1Msg step1Msg ->
            case model of
                Step1Model step1Model ->
                    Step1Model <| Step1.update step1Msg step1Model
        
                        
------------------------------------------------
-- Msg
------------------------------------------------

type Msg
    = Step1Msg Step1.Msg

      
------------------------------------------------
-- Ref View
------------------------------------------------

view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg =
    case model of
        Step1Model stepParam ->
            Step1.view stepParam (msgToMainMsg << Step1Msg)
                
