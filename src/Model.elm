module Model exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Html 
-- import Controller

import Element as E

import Model.Level as Level


------------------------------------------------
-- Model
------------------------------------------------

type Type
    = Level

type Model 
    = LevelModel Level.Model
      
      
------------------------------------------------
-- init
------------------------------------------------

init : Type -> Model
init modelType =
    case modelType of
        Level -> LevelModel Level.init
      
                 
------------------------------------------------
-- Msg
------------------------------------------------
                        
type Msg
    = LevelMsg Level.Msg

      
------------------------------------------------
-- update
------------------------------------------------

update : Msg -> Model -> Model
update msg model =
    case msg of
        LevelMsg levelMsg ->
            case model of
                LevelModel levelModel ->
                    LevelModel <| Level.update levelMsg levelModel 
                        
      
------------------------------------------------
-- view
------------------------------------------------
                        
view : Model -> (Msg -> msg) -> E.Element msg
view model msgToMainMsg = 
    case model of
        LevelModel levelModel ->
            Level.view levelModel (msgToMainMsg << LevelMsg)
                
                
------------------------------------------------
-- xsFromModel
------------------------------------------------

xsFromModel : Model -> Edo.State
xsFromModel model = 
    case model of
        LevelModel levelModel ->
            Level.xsFromModel levelModel

                
------------------------------------------------
-- updateModelFromXs
------------------------------------------------

updateModelFromXs : Edo.State -> Model -> Model
updateModelFromXs xs model =
    case model of
        LevelModel levelModel ->
            LevelModel <| Level.updateModelFromXs xs levelModel
                       
                
------------------------------------------------
-- output
------------------------------------------------

output : Type -> Edo.OutputFunction
output modelType = 
    case modelType of
        Level -> Level.output

------------------------------------------------
-- runEdo
------------------------------------------------

runEdo : Model -> Edo.EdoParam -> Edo.RefFunction -> Edo.Controller -> (DC.ChartData, Edo.EdoParam)
runEdo model edoParam refFunc controller =
     case model of
         LevelModel levelModel ->
             Level.runEdo levelModel edoParam refFunc controller
                 
                 
------------------------------------------------
-- simulation
------------------------------------------------

simulation : Edo.State -> Edo.Ref -> Edo.ControlEffort -> Model -> Html.Html msg
simulation xs rs us model = 
    case model of
        LevelModel levelModel ->
            Level.simulation xs rs us levelModel

                 
                 
