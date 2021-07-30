module EdoSolver exposing (..)

import List

------------------------------------------------
-- Data Types
------------------------------------------------

type alias Tempo = Float
type alias Passo = Float
type alias PassoSaida = Int

type alias EdoParam =  {
                 tempo : Tempo,
                 tfim : Tempo,
                 passo : Passo,
                 relPassoSaida : PassoSaida,
                 solver : Solver} 

type alias State = List Float
type alias DState = List Float
  
type alias FuncSist = Tempo -> State -> DState
type alias Solver = FuncSist -> Passo -> Tempo -> State -> State
type alias Datum = (Tempo , State)
type alias Data = List Datum

type alias EdoIStates = { tini:String, tfim:String }
type EdoInteract = Tini | Tfim

    
------------------------------------------------
-- Functions
------------------------------------------------
    
edoStep : EdoParam -> FuncSist -> State -> State

edoStep param fsist xs =
    let
        solver = .solver param
        passo =  .passo param
        tempo =  .tempo param
        -- xps = fsist tempo xs
    in
        solver fsist passo tempo xs 
  

edoSolver : EdoParam -> FuncSist -> State -> Data
edoSolver param fsist xs =
    let
        tempo = .tempo param
        tfim = .tfim param
    in
        if (tempo >= tfim) then
            (tempo, xs)::[]
        else
            let
                (tempo1, xs1) = integrator param fsist 1 xs
                param1 = { param | tempo = tempo1} 
            in
                (tempo, xs) :: edoSolver param1 fsist xs1  
    

integrator : EdoParam -> FuncSist -> PassoSaida -> State -> Datum
integrator param fsist saidaCount xs =
    let
        tfim = .tfim param
        tempo = .tempo param
        passo = .passo param 
        relPassoSaida = .relPassoSaida param
    in
        if (tfim - tempo <= passo) then
            let
                paramfinal = { param | passo = tfim - tempo }
                xsfinal = edoStep paramfinal fsist xs
            in
                (tfim, xsfinal)
        else
            let 
                tempo1 = tempo + passo
                param1 = { param | tempo = tempo1 } 
                xs1 = edoStep param fsist xs
            in
                if (saidaCount == relPassoSaida) then
                    (tempo1, xs1)
                else
                    integrator param1 fsist (saidaCount + 1) xs1

                        
rungeKutta : Solver
rungeKutta fsist passo tempo xs = 
    let
        xps1 = fsist tempo xs
        k1 = List.map ((*) passo) xps1
             
        tempo2 = tempo + 0.5*passo
        xs2 = zipWith (\a b -> a + 0.5*b) xs k1 
        xps2 = fsist tempo2 xs2
        k2 = List.map ((*) passo) xps2
             
        tempo3 = tempo2
        xs3 = zipWith (\a b -> a + 0.5*b) xs k2
        xps3 = fsist tempo3 xs3
        k3 = List.map ((*) passo) xps3

        tempo4 = tempo + passo
        xs4 = zipWith (+) k3 xs 
        xps4 = fsist tempo4 xs4
        k4 = List.map ((*) passo) xps4
    in
        zipWith (\a b -> 2.0*a + b) k3 k4 
        |> zipWith (\a b -> 2.0*a + b) k2
        |> zipWith (+) k1
        |> zipWith (\a b -> a + b/6.0) xs
            

eulerSolver : Solver
eulerSolver fsist passo tempo xs =
    let 
        xps = fsist tempo xs
        xps2 = List.map ((*) passo) xps
    in
        zipWith (+) xs xps2
                        
            
funcSist : FuncSist
funcSist t xs =
    case xs of
        (x::[]) -> t*x :: [] 
        _ -> xs

           
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith func l1 l2 =
    case l1 of
       [] -> [] 
       x::[] -> 
           case l2 of
               [] -> []
               y::[] -> (func x y)::[]
               y::ys -> (func x y)::[]
       x::xs ->
           case l2 of
               [] -> []
               y::[] -> (func x y)::[]
               y::ys -> (func x y):: zipWith func xs ys


printState : State -> String
printState list =
    case list of
        [] -> ""
        x::[] -> String.fromFloat x
        x::xs -> (String.fromFloat x) ++ "\t" ++ printState xs

printData : Data -> String
printData data = 
    case data of
        [] -> ""
        (t,xs) :: [] -> (String.fromFloat t) ++ "\t" ++ printState xs
        (t,xs) :: dt -> (String.fromFloat t) ++ "\t" ++ printState xs ++ "\n" ++ printData dt


-- type alias EdoIStates = { tini:String, tfim:String }
updateTini : String -> EdoIStates -> EdoIStates
updateTini str edoIStates = 
    {edoIStates | tini = str}
        
updateTfim : String -> EdoIStates -> EdoIStates
updateTfim  str edoIStates = 
    {edoIStates | tfim = str}
                        
------------------------------------------------
-- Variables
------------------------------------------------

xs_ = 3.0 :: []
tini_ = 0.0 
tfim_ = 4.0
passo_ = 0.4 
relSaida_ = 2
param_ = EdoParam tini_ tfim_ passo_ relSaida_ eulerSolver
