module Reference exposing (..)

import EdoSolver as Edo

import Html exposing (Html,div,text,input,span)
import Html.Attributes exposing (style, placeholder, value, type_)
import Html.Events exposing (onInput)
import Element as E
import UI 


------------------------------------------------
------------------------------------------------
-- References
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- RefParam
------------------------------------------------

type RefType
    = Step1

type RefParam
    = Step1P Step1Param

initRefParam : RefType -> RefParam
initRefParam refType = 
    case refType of
        Step1 -> Step1P initStep1Param
    
refFunctionFromRefParam : RefParam -> Edo.RefFunction 
refFunctionFromRefParam refParam = 
    case refParam of
        Step1P step1Param ->
            step1FromStep1Param step1Param
      
changeRefParam : RefParam -> RefInteract -> RefParam
changeRefParam refParam refInteract =
    case refInteract of
        Step1I step1Interact ->
            case refParam of
                Step1P step1Param ->
                    Step1P <| changeStep1Param step1Param step1Interact
        
                        
------------------------------------------------
-- RefInteract
------------------------------------------------

type RefInteract
    = Step1I Step1Interact

      
------------------------------------------------
-- Ref View
------------------------------------------------

viewRef : RefParam -> (RefInteract -> msg) -> Html msg
viewRef refParam refInteractToMsg =
    case refParam of
        Step1P stepParam ->
            step1View stepParam (refInteractToMsg << Step1I)
      
viewRefElement : RefParam -> (RefInteract -> msg) -> E.Element msg
viewRefElement refParam refInteractToMsg =
    case refParam of
        Step1P stepParam ->
            step1ViewElement stepParam (refInteractToMsg << Step1I)
                
------------------------------------------------
------------------------------------------------
-- Step1
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- Step1Param
------------------------------------------------

type alias Step1Param =
    { iVal:Float,tStep:Float,fVal:Float
    , iValStr:String, tStepStr:String, fValStr:String} 
    
    
initStep1Param : Step1Param
initStep1Param =
    { iVal = 0.0, tStep = 0.0, fVal = 0.0
    , iValStr = "0", tStepStr = "0", fValStr = "0"}
    
        
step1FromStep1Param : Step1Param -> Edo.RefFunction
step1FromStep1Param step1Param = 
    let 
        iVal = .iVal step1Param
        tStep = .tStep step1Param 
        fVal = .fVal step1Param
    in
        step1 iVal tStep fVal
        
            
changeStep1Param : Step1Param -> Step1Interact -> Step1Param
changeStep1Param step1Param step1Interact =
    case step1Interact of
        Step1IVal valueStr -> 
            let
                iVal = .iVal step1Param
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault iVal maybeVal
            in
            { step1Param | iValStr = valueStr, iVal = val }
        Step1TStep valueStr -> 
            let
                tStep = .tStep step1Param
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault tStep maybeVal
            in
            { step1Param | tStepStr = valueStr, tStep = val }
        Step1FVal valueStr -> 
            let
                fVal = .fVal step1Param
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault fVal maybeVal
            in
            { step1Param | fValStr = valueStr, fVal = val }
    
                          
------------------------------------------------
-- Step1Interact
------------------------------------------------

type Step1Interact
    = Step1IVal String
    | Step1TStep String
    | Step1FVal String
    
      
------------------------------------------------
-- Step1 View
------------------------------------------------

step1View : Step1Param -> (Step1Interact -> msg) -> Html msg
step1View step1Param step1InteractToMsg =
    let 
        iValStr = .iValStr step1Param
        tStepStr = .tStepStr step1Param
        fValStr = .fValStr step1Param
    in
    span [] [ parameterInteractiveDiv "iVal" "" iValStr (step1InteractToMsg << Step1IVal)
            , parameterInteractiveDiv "tStep" "" tStepStr (step1InteractToMsg << Step1TStep)
            , parameterInteractiveDiv "fVal" "" fValStr (step1InteractToMsg << Step1FVal)
            ]

step1ViewElement : Step1Param -> (Step1Interact -> msg) -> E.Element msg
step1ViewElement step1Param step1InteractToMsg =
    let 
        iValStr = .iValStr step1Param
        tStepStr = .tStepStr step1Param
        fValStr = .fValStr step1Param
    in
    E.column [E.spacing 10, E.padding 20, E.centerX]
        [ UI.heading "Step Reference"
        , E.row [E.spacing 25]
                [ UI.textField iValStr "iVal" (step1InteractToMsg << Step1IVal)
                , UI.textField tStepStr "Tstep" (step1InteractToMsg << Step1TStep)
                , UI.textField fValStr "fVal" (step1InteractToMsg << Step1FVal)
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
            
parameterInteractiveDiv : String -> String -> String -> (String -> msg) -> Html msg
parameterInteractiveDiv texto pholder valor strToMsg =
    span []
    [ text texto
    , input [type_ "number", placeholder pholder, value valor, onInput <| strToMsg ] []
    ]
