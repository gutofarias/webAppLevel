module Reference.Step1 exposing (..)

import EdoSolver as Edo
import Element as E
import UI


------------------------------------------------
------------------------------------------------
-- Step1
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- Model
------------------------------------------------

type alias Model =
    { iVal:Float,tStep:Float,fVal:Float
    , iValStr:String, tStepStr:String, fValStr:String} 
    
    
init : Model
init =
    { iVal = 0.0, tStep = 0.0, fVal = 0.0
    , iValStr = "0", tStepStr = "0", fValStr = "0"}
    
        
refFunctionFromModel : Model -> Edo.RefFunction
refFunctionFromModel model = 
    let 
        iVal = .iVal model
        tStep = .tStep model 
        fVal = .fVal model
    in
        step1 iVal tStep fVal
        
            
update : Msg -> Model -> Model
update msg model =
    case msg of
        Step1IVal valueStr -> 
            let
                iVal = .iVal model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault iVal maybeVal
            in
            { model | iValStr = valueStr, iVal = val }
        Step1TStep valueStr -> 
            let
                tStep = .tStep model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault tStep maybeVal
            in
            { model | tStepStr = valueStr, tStep = val }
        Step1FVal valueStr -> 
            let
                fVal = .fVal model
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault fVal maybeVal
            in
            { model | fValStr = valueStr, fVal = val }
    
                          
------------------------------------------------
-- Msg
------------------------------------------------

type Msg
    = Step1IVal String
    | Step1TStep String
    | Step1FVal String
    
      
------------------------------------------------
-- Step1 View
------------------------------------------------

view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg =
    let 
        iValStr = .iValStr model
        tStepStr = .tStepStr model
        fValStr = .fValStr model
    in
    E.column [E.spacing 10, E.padding 20, E.centerX]
        [ UI.heading "Step Reference"
        , E.row [E.spacing 25]
                [ UI.textField iValStr "iVal" (msgToMainMsg << Step1IVal)
                , UI.textField tStepStr "Tstep" (msgToMainMsg << Step1TStep)
                , UI.textField fValStr "fVal" (msgToMainMsg << Step1FVal)
                ]
        ]
        
------------------------------------------------
-- Step1 RefFunction
------------------------------------------------
        
step1 : Float -> Float -> Float -> Edo.Tempo -> Edo.Output -> Edo.Ref
step1 iVal tStep fVal tempo output = 
    if tStep >= tempo then
        [iVal]
    else
        [fVal]


------------------------------------------------
-- Auxiliary Functions
------------------------------------------------
            
