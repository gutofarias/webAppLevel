module DataConvert exposing (..)

import EdoSolver as Edo

type alias AxisFunc data = data -> Float
    
type alias DatumTS1 = {t : Float, x1 : Float }
type alias DatumTS1E1R1U1 = {t : Float, x1 : Float, e1 : Float, r1 : Float, u1 : Float }
type alias DatumTS1E1R1U4 = {t : Float, x1 : Float, e1 : Float, r1 : Float, u1 : Float, u2 : Float, u3 : Float, u4 : Float }
    
-- type alias DatumT2S = {t : Float, x1 : Float, x2 : Float }
-- type alias DatumT3S = {t : Float, x1 : Float, x2 : Float, x3 : Float }
    
-- Adicionar os data types à medida em que for criando
type ChartDatum
    = TS1 DatumTS1
    | TS1E1R1U1 DatumTS1E1R1U1
    | TS1E1R1U4 DatumTS1E1R1U4
    -- | TS11R1U DatumTS11R1U

type alias ChartData = List ChartDatum
 
-- type ChartData
--     = Nodata
--     | TS1 DataTS1
--     | T2S DataT2S
--     | T3S DataT3S
--     | T4S DataT4S
--     | T5S DataT5S
    
                      
toChartDatumTS1 : Edo.Datum -> ChartDatum
toChartDatumTS1 edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                x::xs -> TS1 {t = tempo, x1 = x}
                [] -> TS1 {t= tempo, x1 = 0.0}         

toChartDatumTS1E1R1U1 : Edo.Datum -> ChartDatum
toChartDatumTS1E1R1U1 edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                (x::e::r::u::xs) -> TS1E1R1U1 {t = tempo, x1 = x, e1 = e, r1 = r, u1 = u}
                _ -> TS1E1R1U1 {t = 0.0, x1 = 0.0, e1 = 0.0, r1 = 0.0, u1 = 0.0}
                     
toChartDatumTS1E1R1U4 : Edo.Datum -> ChartDatum
toChartDatumTS1E1R1U4 edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                (x::e::r::u1::u2::u3::u4::xs) -> TS1E1R1U4 {t = tempo, x1 = x, e1 = e, r1 = r, u1 = u1, u2 = u2, u3 = u3, u4 = u4}
                _ -> TS1E1R1U4 {t = 0.0, x1 = 0.0, e1 = 0.0, r1 = 0.0, u1 = 0.0, u2 = 0.0, u3 = 0.0, u4 = 0.0}
                     
-- toChartDatumT2S : Edo.Datum -> ChartDatum
-- toChartDatumT2S edoDatum =
--     case edoDatum of
--         (t, sist) ->
--             case sist of 
--                 x1::x2::xs -> T2S <| DatumT2S t x1 x2
--                 _ -> T2S <| DatumT2S t 0.0 0.0
                     
-- toChartDatumT3S : Edo.Datum -> ChartDatum
-- toChartDatumT3S edoDatum =
--     case edoDatum of
--         (t, sist) ->
--             case sist of 
--                 x1::x2::x3::xs -> T3S <| DatumT3S t x1 x2 x3
--                 _ -> T3S <| DatumT3S t 0.0 0.0 0.0
            
                     
toChartDataTS1 : Edo.Data -> ChartData
toChartDataTS1 =
    List.map toChartDatumTS1 
        
toChartDataTS1E1R1U1 : Edo.Data -> ChartData
toChartDataTS1E1R1U1 =
    List.map toChartDatumTS1E1R1U1 
        
toChartDataTS1E1R1U4 : Edo.Data -> ChartData
toChartDataTS1E1R1U4 =
    List.map toChartDatumTS1E1R1U4
        
-- toChartDataT2S : Edo.Data -> ChartData
-- toChartDataT2S =
--     List.map toChartDatumT2S 
                     
-- toChartDataT3S : Edo.Data -> ChartData
-- toChartDataT3S =
--     List.map toChartDatumT3S 
        
                     
stringToAxisFunc : String -> AxisFunc ChartDatum
stringToAxisFunc str =
    case str of
        "t" -> ft
        "x1" -> fx1
        "e1" -> fe1
        "r1" -> fr1
        "u1" -> fu1
        "u2" -> fu2
        "u3" -> fu3
        "u4" -> fu4
        _ -> ft
             
fx1 : ChartDatum -> Float
fx1 chartDatum = 
    case chartDatum of
        TS1 datum -> datum.x1
        TS1E1R1U1 datum -> datum.x1
        TS1E1R1U4 datum -> datum.x1

ft : ChartDatum -> Float
ft chartDatum = 
    case chartDatum of
        TS1 datum -> datum.t
        TS1E1R1U1 datum -> datum.t
        TS1E1R1U4 datum -> datum.t
                                
fe1 : ChartDatum -> Float
fe1 chartDatum = 
    case chartDatum of
        TS1 datum -> 0.0
        TS1E1R1U1 datum -> datum.e1
        TS1E1R1U4 datum -> datum.e1
                     
fr1 : ChartDatum -> Float
fr1 chartDatum = 
    case chartDatum of
        TS1 datum -> 0.0
        TS1E1R1U1 datum -> datum.r1
        TS1E1R1U4 datum -> datum.r1
                                
fu1 : ChartDatum -> Float
fu1 chartDatum = 
    case chartDatum of
        TS1 datum -> 0.0
        TS1E1R1U1 datum -> datum.u1
        TS1E1R1U4 datum -> datum.u1
                                
fu2 : ChartDatum -> Float
fu2 chartDatum = 
    case chartDatum of
        TS1 datum -> 0.0
        TS1E1R1U1 datum -> 0.0
        TS1E1R1U4 datum -> datum.u2
                           
fu3 : ChartDatum -> Float
fu3 chartDatum = 
    case chartDatum of
        TS1 datum -> 0.0
        TS1E1R1U1 datum -> 0.0
        TS1E1R1U4 datum -> datum.u3
                           
fu4 : ChartDatum -> Float
fu4 chartDatum = 
    case chartDatum of
        TS1 datum -> 0.0
        TS1E1R1U1 datum -> 0.0
        TS1E1R1U4 datum -> datum.u4


xsFromDatum : ChartDatum -> List Float
xsFromDatum chartDatum = 
    case chartDatum of
        TS1 datum -> 
            [(.x1 datum)]
        TS1E1R1U1 datum -> 
            [(.x1 datum)]
        TS1E1R1U4 datum ->
            [(.x1 datum)]

usFromDatum : ChartDatum -> List Float
usFromDatum chartDatum = 
    case chartDatum of
        TS1 datum -> 
            [0.0]
        TS1E1R1U1 datum -> 
            [(.u1 datum)]
        TS1E1R1U4 datum ->
            [(.u1 datum),(.u2 datum),(.u3 datum),(.u4 datum)]
