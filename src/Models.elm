module Models exposing (..)

import EdoSolver as Edo

type alias LevelParam = {ag : Float, ap : Float}

level : LevelParam -> Edo.FuncSist
level param = 
    levelSyst param

levelSyst : LevelParam -> Edo.Tempo -> Edo.State -> Edo.DState
levelSyst param t state =
    let
       ag = param.ag
       ap = param.ap
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
