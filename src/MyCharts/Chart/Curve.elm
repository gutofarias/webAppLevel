module MyCharts.Chart.Curve exposing (..)

import Chart as C
-- import Chart.Attributes as CA
import DataConvert as DC 
import Html as H
import Html.Attributes as HA


------------------------------------------------
-- Model
------------------------------------------------

type alias AxesString = (String,String)
type alias CurveID = Int
    
type alias Model =
    { curveID : CurveID
    , axesString : AxesString
    }

    
------------------------------------------------
-- init
------------------------------------------------

init : Maybe Model -> Model 
init maybeLastCurve =
    case maybeLastCurve of
        Nothing ->
            {curveID = 1, axesString = ("t","x1")}
                
        Just lastCurve ->
            let
                lastCurveID = .curveID lastCurve
                curveID = lastCurveID + 1
            in
                {curveID = curveID, axesString = ("t","x1")}

                    
------------------------------------------------
-- Msg
------------------------------------------------

type Msg
    = XAxis String
    | YAxis String
      
      
------------------------------------------------
-- update
------------------------------------------------

update : Msg -> Model -> Model 
update msg model =
    let
       (xAxis, yAxis) = .axesString model 
       otherCurveID = .curveID model
    in
        case msg of
            XAxis axisString ->
                {model | axesString = (axisString,yAxis)}
            YAxis axisString ->
                {model | axesString = (xAxis,axisString)}
                
                        
------------------------------------------------
-- view
------------------------------------------------
                
view : DC.ChartData -> String -> List (H.Html msg)
view chartData selStr =
    case chartData of
        [] -> []
        chartDatum :: ls ->
            case chartDatum of
                DC.TS1 datum -> 
                    [ chartAxisOptionView "t" "t" selStr
                    , chartAxisOptionView "x1" "x" selStr]
                DC.TS1E1R1U1 datum ->
                    [ chartAxisOptionView "t" "t" selStr
                    , chartAxisOptionView "x1" "x" selStr
                    , chartAxisOptionView "e1" "e" selStr
                    , chartAxisOptionView "r1" "r" selStr
                    , chartAxisOptionView "u1" "u" selStr]
                DC.TS1E1R1U4 datum ->
                    [ chartAxisOptionView "t" "t" selStr
                    , chartAxisOptionView "x1" "x" selStr
                    , chartAxisOptionView "e1" "e" selStr
                    , chartAxisOptionView "r1" "r" selStr
                    , chartAxisOptionView "u1" "u" selStr
                    , chartAxisOptionView "u2" "up" selStr
                    , chartAxisOptionView "u3" "ui" selStr
                    , chartAxisOptionView "u4" "ud" selStr]


                    
chartAxisOptionView : String -> String -> String -> H.Html msg
chartAxisOptionView val txt selStr =
        if (selStr == val) then
            H.option [HA.value val, HA.selected True] [H.text txt]
            -- option [value val] [text txt]
        else
            H.option [HA.value val] [H.text txt]



------------------------------------------------
-- ChartSeries
------------------------------------------------
                
curveToChartSeries : DC.ChartData -> Model -> Maybe (C.Element DC.ChartDatum msg)
curveToChartSeries chartData model = 
    let 
        (xstr,ystr) = .axesString model
                      
        -- stringToAxisFunc me retorna um Maybe (AxisFuncMaybe ChartDatum) 
        -- type alias AxisFuncMaybe data = data -> Maybe Float
        maybeXfuncMaybeFloat = DC.stringToAxisFunc xstr
        maybeYfuncMaybeFloat = DC.stringToAxisFunc ystr
                               
        -- Maybe (ChartDatum -> Maybe Float)
        -- Dois maybes, o de fora é pq eu nao sei se o string retorna uma funcao valida, o de dentro é porque eu não sei se o chartDatum permite que eu aplique a função. Posso ter um TS1 e pedir u1, por exemplo
    in
        case (maybeXfuncMaybeFloat, maybeYfuncMaybeFloat) of

            -- (ChartDatum -> Maybe Float)
            -- Analisa se existe a função para a dada string
            (Just xfuncMaybeFloat, Just yfuncMaybeFloat) ->
                let
                    -- Maybe (ChartDatum -> Float)
                    -- Analisa se o dado é suportado pela funcao usando o chartData 
                    maybeXfunc = DC.funcMaybeToMaybeFunc2 chartData xfuncMaybeFloat
                    maybeYfunc = DC.funcMaybeToMaybeFunc2 chartData yfuncMaybeFloat
                in
                    case (maybeXfunc, maybeYfunc) of
                        -- Caso tudo esteja ok consegue pegar a funcao e retornar o Just
                        (Just xfunc, Just yfunc) ->
                            Just  
                            (C.series xfunc
                            [ C.interpolated yfunc [ -- CA.monotone
                                                ] [ ] --CA.circle ]
                            ]

                            chartData)

                        -- caso contrario retorna nothing
                        _ -> Nothing
            -- caso contrario retorna nothing
            _ -> Nothing
