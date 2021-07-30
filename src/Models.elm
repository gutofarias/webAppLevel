module Models exposing (..)

import EdoSolver as Edo
import Html exposing (Html,div,text,input)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onInput)


------------------------------------------------
------------------------------------------------
-- MODELOS EM GERAL
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- Data Types
------------------------------------------------

type ModelParam 
    = LevelP LevelParam

type ModelIStates
    = LevelIS LevelIStates

type ModelInteract
    = LevelI LevelInteract
      
        
------------------------------------------------
-- Functions
------------------------------------------------

parameterInteractiveDiv : String -> String -> String -> (String -> a) -> Html a
parameterInteractiveDiv texto pholder valor msg =
    div []
    [ text texto
    , input [ placeholder pholder, value valor, onInput msg] []
    ]
                
------------------------------------------------
------------------------------------------------
-- Nível
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- Data Types
------------------------------------------------

type alias LevelParam = { initState:LevelInitState, geoParam:LevelGeoParam }

type alias LevelIStates = { h0:String, ag:String, ap:String }
    
type LevelInteract = H0 | Ag | Ap
    
type alias LevelInitState = { h0 : Float }
type alias LevelGeoParam = { ag : Float, ap : Float }

------------------------------------------------
-- functions
------------------------------------------------

level : LevelParam -> Edo.FuncSist
level param = 
    levelSyst param
        
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
