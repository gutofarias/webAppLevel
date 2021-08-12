module DataConvert exposing (..)

import EdoSolver as Edo

type alias DatumT1S = {t : Float, x1 : Float }
-- type alias DatumT2S = {t : Float, x1 : Float, x2 : Float }
-- type alias DatumT3S = {t : Float, x1 : Float, x2 : Float, x3 : Float }
    
-- Adicionar os data types à medida em que for criando
type ChartDatum
    = T1S DatumT1S
    -- | T1S1R1U DatumT1S1R1U

type alias ChartData = List ChartDatum
 
-- type ChartData
--     = Nodata
--     | T1S DataT1S
--     | T2S DataT2S
--     | T3S DataT3S
--     | T4S DataT4S
--     | T5S DataT5S
    
toChartDatumT1S : Edo.Datum -> ChartDatum
toChartDatumT1S edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                x::xs -> T1S {t = tempo, x1 = x}
                [] -> T1S {t= tempo, x1 = 0.0}         

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
            
                     
toChartDataT1S : Edo.Data -> ChartData
toChartDataT1S =
    List.map toChartDatumT1S 
        
-- toChartDataT2S : Edo.Data -> ChartData
-- toChartDataT2S =
--     List.map toChartDatumT2S 
                     
-- toChartDataT3S : Edo.Data -> ChartData
-- toChartDataT3S =
--     List.map toChartDatumT3S 
        
                     
                     
