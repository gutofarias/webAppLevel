module DataConvert exposing (..)

import EdoSolver as Edo

type alias DatumT1S = {t : Float, x1 : Float }
type alias DatumT2S = {t : Float, x1 : Float, x2 : Float }
type alias DatumT3S = {t : Float, x1 : Float, x2 : Float, x3 : Float }
type alias DatumT4S = {t : Float, x1 : Float, x2 : Float, x3 : Float, x4 : Float }
type alias DatumT5S = {t : Float, x1 : Float, x2 : Float, x3 : Float, x4 : Float, x5 : Float }
    
type alias DataT1S = List DatumT1S
type alias DataT2S = List DatumT2S
type alias DataT3S = List DatumT3S
type alias DataT4S = List DatumT4S
type alias DataT5S = List DatumT5S

toChartDatumT1S : Edo.Datum -> DatumT1S
toChartDatumT1S edoDatum =
    case edoDatum of
        (tempo, sist) ->
            case sist of 
                x::[] -> {t = tempo, x1 = x}
                x::xs -> {t = tempo, x1 = x}
                [] -> {t= tempo, x1 = 0.0}         

toChartDatumT2S : Edo.Datum -> DatumT2S
toChartDatumT2S edoDatum =
    case edoDatum of
        (t, sist) ->
            case sist of 
                x1::x2::[] -> DatumT2S t x1 x2
                x1::x2::xs -> DatumT2S t x1 x2
                _ -> DatumT2S t 0.0 0.0
                     
toChartDatumT3S : Edo.Datum -> DatumT3S
toChartDatumT3S edoDatum =
    case edoDatum of
        (t, sist) ->
            case sist of 
                x1::x2::x3::[] -> DatumT3S t x1 x2 x3
                x1::x2::x3::xs -> DatumT3S t x1 x2 x3
                _ -> DatumT3S t 0.0 0.0 0.0
            
toChartDatumT4S : Edo.Datum -> DatumT4S
toChartDatumT4S edoDatum =
    case edoDatum of
        (t, sist) ->
            case sist of 
                x1::x2::x3::x4::[] -> DatumT4S t x1 x2 x3 x4
                x1::x2::x3::x4::xs -> DatumT4S t x1 x2 x3 x4
                _ -> DatumT4S t 0.0 0.0 0.0 0.0
                     
toChartDatumT5S : Edo.Datum -> DatumT5S
toChartDatumT5S edoDatum =
    case edoDatum of
        (t, sist) ->
            case sist of 
                x1::x2::x3::x4::x5::[] -> DatumT5S t x1 x2 x3 x4 x5
                x1::x2::x3::x4::x5::xs -> DatumT5S t x1 x2 x3 x4 x5
                _ -> DatumT5S t 0.0 0.0 0.0 0.0 0.0
    
                     
toChartDataT1S : Edo.Data -> DataT1S
toChartDataT1S =
    List.map toChartDatumT1S 
        
toChartDataT2S : Edo.Data -> DataT2S
toChartDataT2S =
    List.map toChartDatumT2S 
                     
toChartDataT3S : Edo.Data -> DataT3S
toChartDataT3S =
    List.map toChartDatumT3S 
        
toChartDataT4S : Edo.Data -> DataT4S
toChartDataT4S =
    List.map toChartDatumT4S 
        
toChartDataT5S : Edo.Data -> DataT5S
toChartDataT5S =
    List.map toChartDatumT5S 
                     
                     
