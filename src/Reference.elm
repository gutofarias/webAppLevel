module Reference exposing (..)

import EdoSolver as Edo

import Html exposing (Html,div,text,input,span)
import Html.Attributes exposing (style, placeholder, value, type_)
import Html.Events exposing (onInput)


------------------------------------------------
------------------------------------------------
-- References
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- RefParam
------------------------------------------------

type RefParam
    = Step1P Step1Param

updateRefParam : RefParam -> RefIStates -> RefParam
updateRefParam refParam refIStates =
    case refParam of
        Step1P step1Param ->
            case refIStates of
                Step1IS step1IStates ->
                    Step1P <| updateStep1Param step1Param step1IStates
    
refFromRefParam : RefParam -> Edo.RefFunction 
refFromRefParam refParam = 
    case refParam of
        Step1P step1Param ->
            step1FromStep1Param step1Param
      
------------------------------------------------
-- RefIStates
------------------------------------------------
      
type RefIStates
    = Step1IS Step1IStates

changeRefIStates : RefIStates -> RefInteract -> RefIStates
changeRefIStates refIStates refInteract =
    case refInteract of
        Step1I step1Interact ->
            case refIStates of
                Step1IS step1IStates ->
                    Step1IS <| changeStep1IStates step1IStates step1Interact
        
        
------------------------------------------------
-- RefInteract
------------------------------------------------

type RefInteract
    = Step1I Step1Interact

      
------------------------------------------------
-- Ref View
------------------------------------------------

viewRefIStates : RefIStates -> (RefInteract -> msg) -> Html msg
viewRefIStates refIStates refInteractToMsg =
    case refIStates of
        Step1IS step1IStates ->
            step1View step1IStates (refInteractToMsg << Step1I)
      
      
------------------------------------------------
------------------------------------------------
-- Step1
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- Step1Param
------------------------------------------------

type alias Step1Param =
    {iVal:Float,tStep:Float,fVal:Float} 
        
updateStep1Param : Step1Param -> Step1IStates -> Step1Param
updateStep1Param step1Param step1IStates =
    let
        iValStr = .iValStr step1IStates
        tStepStr = .tStepStr step1IStates
        fValStr = .fValStr step1IStates
        listStr = [iValStr,tStepStr,fValStr]
        listValues = List.filterMap String.toFloat listStr
    in
        case listValues of
            (iVal::tStep::fVal::[]) ->
                {step1Param | iVal=iVal, tStep=tStep, fVal=fVal} 

            _ -> step1Param
                 
step1FromStep1Param : Step1Param -> Edo.RefFunction
step1FromStep1Param step1Param = 
    let 
        iVal = .iVal step1Param
        tStep = .tStep step1Param 
        fVal = .fVal step1Param
    in
        step1 iVal tStep fVal
        
------------------------------------------------
-- Step1IStates
------------------------------------------------

type alias Step1IStates =
    {iValStr:String, tStepStr:String, fValStr:String}

changeStep1IStates : Step1IStates -> Step1Interact -> Step1IStates
changeStep1IStates step1IStates step1Interact =
    case step1Interact of
        Step1IVal valueStr -> {step1IStates | iValStr = valueStr}
        Step1TStep valueStr -> {step1IStates | tStepStr = valueStr}
        Step1FVal valueStr -> {step1IStates | fValStr = valueStr}
    
                          
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

step1View : Step1IStates -> (Step1Interact -> msg) -> Html msg
step1View step1IStates step1InteractToMsg =
    let 
        iValStr = .iValStr step1IStates
        tStepStr = .tStepStr step1IStates
        fValStr = .fValStr step1IStates
    in
    span [] [ parameterInteractiveDiv "iVal" "" iValStr (step1InteractToMsg << Step1IVal)
            , parameterInteractiveDiv "tStep" "" tStepStr (step1InteractToMsg << Step1TStep)
            , parameterInteractiveDiv "fVal" "" fValStr (step1InteractToMsg << Step1FVal)
            ]

        
------------------------------------------------
-- Ref Functions
------------------------------------------------

initStep1IStates : Step1IStates
initStep1IStates =
    {iValStr = "0", tStepStr = "0", fValStr = "0"}

initStep1Param : Step1Param
initStep1Param =
    {iVal = 0.0, tStep = 0.0, fVal = 0.0}
        
step1 : Float -> Float -> Float -> Edo.Tempo -> Edo.Output -> Edo.Ref
step1 iVal tStep fVal tempo output = 
    if tStep >= tempo then
        [iVal]
    else
        [fVal]

            
parameterInteractiveDiv : String -> String -> String -> (String -> msg) -> Html msg
parameterInteractiveDiv texto pholder valor strToMsg =
    span []
    [ text texto
    , input [type_ "number", placeholder pholder, value valor, onInput <| strToMsg ] []
    ]
