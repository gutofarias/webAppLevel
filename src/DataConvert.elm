module DataConvert exposing (..)

import EdoSolver as Edo

type alias DatumTS1 = {t : Float, x1 : Float }
type alias DatumTS1E1R1U1 = {t : Float, x1 : Float, e1 : Float, r1 : Float, u1 : Float }
    
-- type alias DatumT2S = {t : Float, x1 : Float, x2 : Float }
-- type alias DatumT3S = {t : Float, x1 : Float, x2 : Float, x3 : Float }
    
-- Adicionar os data types à medida em que for criando
type ChartDatum
    = TS1 DatumTS1
    | TS1E1R1U1 DatumTS1E1R1U1
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
        
-- toChartDataT2S : Edo.Data -> ChartData
-- toChartDataT2S =
--     List.map toChartDatumT2S 
                     
-- toChartDataT3S : Edo.Data -> ChartData
-- toChartDataT3S =
--     List.map toChartDatumT3S 
        
                     
                     
