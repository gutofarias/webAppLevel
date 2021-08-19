module Models exposing (..)

import DataConvert as DC
import EdoSolver as Edo
import Html exposing (Html,div,text,input,span,pre,label)
import Html.Attributes exposing (style, placeholder, value, type_)
import Controller
import Html.Events exposing (onInput)
import Drawing2d
import Pixels exposing (pixels)
import Point2d
import Rectangle2d
import Polygon2d
import Color
import Angle
import Arc2d
import Polyline2d

------------------------------------------------
------------------------------------------------
-- MODELOS EM GERAL
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- ModelParam
------------------------------------------------

type ModelType 
    = Level

type ModelParam 
    = LevelP LevelParam
      
initModelParam : ModelType -> ModelParam
initModelParam modelType =
    case modelType of
        Level -> LevelP initLevelParam
      
                 
xsFromModelParam : ModelParam -> Edo.State
xsFromModelParam modelParam = 
    case modelParam of
        LevelP levelParam ->
            xsFromLevelParam levelParam

updateModelParamFromXs : Edo.State -> ModelParam -> ModelParam
updateModelParamFromXs xs modelParam =
    case modelParam of
        LevelP levelParam ->
            LevelP <| updateLevelParamFromXs xs levelParam
                       

changeModelParam : ModelParam -> ModelInteract -> ModelParam
changeModelParam modelParam modelInteract =
    case modelInteract of
        LevelI levelInteract ->
            case modelParam of
                LevelP levelParam ->
                    LevelP <| changeLevelParam levelParam levelInteract 
                        
                        
------------------------------------------------
-- ModelInteract
------------------------------------------------
                        
type ModelInteract
    = LevelI LevelInteract

      
------------------------------------------------
-- viewModel
------------------------------------------------
                        
viewModel : ModelParam -> (ModelInteract -> msg) -> Html msg
viewModel modelParam modelInteractToMsg = 
    case modelParam of
        LevelP levelParam ->
            viewLevel levelParam (modelInteractToMsg << LevelI)
                
                
parameterInteractiveDiv : String -> String -> String -> (String -> msg) -> Html msg
parameterInteractiveDiv texto pholder valor strToMsg =
    span []
    [ label [] [text texto]
    , input [type_ "number", placeholder pholder, value valor, onInput <| strToMsg ] []
    ]
                
    
------------------------------------------------
-- runEdoModel
------------------------------------------------

runEdoModel : ModelParam -> Edo.EdoParam -> Maybe {refFunc: Edo.RefFunction, controller:Edo.Controller} -> (DC.ChartData, Edo.EdoParam)
runEdoModel modelParam edoParam maybeRefAndController =
     case modelParam of
         LevelP levelParam ->
             runEdoLevel levelParam edoParam maybeRefAndController
                 
                 
-- runAnimationModel : ModelParam -> Edo.EdoParam -> Maybe {refFunc: Edo.RefFunction, controller:Edo.Controller} -> (DC.ChartData, Edo.EdoParam)
-- runAnimationModel modelParam edoParam maybeRefAndController =
--      case modelParam of
--          LevelP levelParam ->
--              runAnimationLevel levelParam edoParam maybeRefAndController
                 
                 
                 
------------------------------------------------
------------------------------------------------
-- Nível
------------------------------------------------
------------------------------------------------

------------------------------------------------
-- LevelParam
------------------------------------------------

type alias LevelParam =
    { h0 : Float , ag : Float , ap : Float
    , h0Str : String, agStr : String, apStr : String }
    
initLevelParam : LevelParam
initLevelParam =
    { h0 = 10.0
    , ag = 1.0
    , ap = 0.1
    , h0Str = "10"
    , agStr = "1"
    , apStr = "0.1" }
    
xsFromLevelParam : LevelParam -> Edo.State
xsFromLevelParam levelParam =
    [(.h0 levelParam)]
                    
updateLevelParamFromXs : Edo.State -> LevelParam -> LevelParam
updateLevelParamFromXs xs levelParam = 
    let
        h0 = .h0 levelParam
        newH0 = Maybe.withDefault h0 <| List.head xs
    in
        {levelParam | h0 = newH0}
    
            
changeLevelParam : LevelParam -> LevelInteract -> LevelParam
changeLevelParam levelParam levelInteract =
    case levelInteract of
        H0 valueStr -> 
            let
                h0 = .h0 levelParam
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault h0 maybeVal
            in
            { levelParam | h0Str = valueStr, h0 = val }
        Ag valueStr ->
            let
                ag = .ag levelParam
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ag maybeVal
            in
            { levelParam | agStr = valueStr, ag = val }
        Ap valueStr -> 
            let
                ap = .ap levelParam
                maybeVal = String.toFloat valueStr
                val = Maybe.withDefault ap maybeVal
            in
            { levelParam | apStr = valueStr, ap = val }

        

------------------------------------------------
-- LevelInteract
------------------------------------------------

type LevelInteract
    = H0 String
    | Ag String
    | Ap String
    
    
------------------------------------------------
-- viewLevel
------------------------------------------------
    
viewLevel : LevelParam -> (LevelInteract -> msg) -> Html msg
viewLevel levelParam levelInteractToMsg = 
    let 
        h0Str = .h0Str levelParam
        agStr = .agStr levelParam
        apStr = .apStr levelParam
    in 
        div []
            [ parameterInteractiveDiv "h0  " "" h0Str (levelInteractToMsg << H0)
            , parameterInteractiveDiv "A   " "" agStr (levelInteractToMsg << Ag)
            , parameterInteractiveDiv "a   " "" apStr (levelInteractToMsg << Ap)
            ]
    

------------------------------------------------
-- runEdoLevel
------------------------------------------------

runEdoLevel : LevelParam -> Edo.EdoParam -> Maybe {refFunc: Edo.RefFunction, controller:Edo.Controller} -> (DC.ChartData, Edo.EdoParam)
runEdoLevel levelParam edoParam maybeRefFuncAndController =
    case maybeRefFuncAndController of
        Nothing ->
            runEdoLevelUncontrolled levelParam edoParam

        Just refFuncAndController ->
            let
                refFunc = .refFunc refFuncAndController
                controller = .controller refFuncAndController
            in
                runEdoLevelControlled levelParam edoParam refFunc controller
                
-- runAnimationLevel : LevelParam -> Edo.EdoParam -> Maybe {refFunc: Edo.RefFunction, controller:Edo.Controller} -> (DC.ChartData, Edo.EdoParam)
-- runAnimationLevel levelParam edoParam maybeRefFuncAndController =
--     case maybeRefFuncAndController of
--         Nothing ->
--              runAnimationLevelUncontrolled levelParam edoParam

--         Just refFuncAndController ->
--             let
--                 refFunc = .refFunc refFuncAndController
--                 controller = .controller refFuncAndController
--             in
--                 runAnimationLevelControlled levelParam edoParam refFunc controller
                
-- runAnimationLevelUncontrolled : LevelParam -> Edo.EdoParam -> (DC.ChartData, Edo.EdoParam)
-- runAnimationLevelUncontrolled levelParam edoParam =
--         let
--             initState = (.h0 levelParam) :: []
--             edoSist = Edo.Uncontrolled (levelSyst levelParam [0.0])
--             (edoData, edoParamNew) = Edo.edoSolverReversed edoParam edoSist initState
--         in
--             -- DC.toChartDataTS1E1R1U1 <| Tuple.first <| Edo.edoSolver edoParam edoSist initState
--             (DC.toChartDataTS1 edoData, edoParamNew)
                    
                
-- runAnimationLevelControlled : LevelParam -> Edo.EdoParam -> Edo.RefFunction  -> Edo.Controller -> (DC.ChartData, Edo.EdoParam)
-- runAnimationLevelControlled levelParam edoParam refFunc controller =
--         let
--             initState = (.h0 levelParam) :: []
--             edoSist = Edo.Controlled
--                       { refFunc = refFunc
--                       , outputFunc = outputX1
--                       , controller = controller
--                       , sistFunc = (levelSyst levelParam)}
                
--             (edoData, edoParamNew) = Edo.edoSolverReversed edoParam edoSist initState
--         in
--             (DC.toChartDataTS1E1R1U4 edoData, edoParamNew)
                
runEdoLevelControlled : LevelParam -> Edo.EdoParam -> Edo.RefFunction  -> Edo.Controller -> (DC.ChartData, Edo.EdoParam)
runEdoLevelControlled levelParam edoParam refFunc controller =
        let
            initState = (.h0 levelParam) :: []
            edoSist = Edo.Controlled
                      { refFunc = refFunc
                      , outputFunc = outputX1
                      , controller = controller
                      , sistFunc = (levelSyst levelParam)}
            
            (edoData, edoParamNew) = Edo.edoSolverReversed edoParam edoSist initState
        in
            (DC.toChartDataTS1E1R1U4 edoData, edoParamNew)

runEdoLevelUncontrolled : LevelParam -> Edo.EdoParam -> (DC.ChartData, Edo.EdoParam)
runEdoLevelUncontrolled levelParam edoParam =
        let
            initState = (.h0 levelParam) :: []
            edoSist = Edo.Uncontrolled (levelSyst levelParam [0.0])
            (edoData, edoParamNew) = Edo.edoSolverReversed edoParam edoSist initState
        in
            (DC.toChartDataTS1 edoData, edoParamNew)
                
------------------------------------------------
-- levelSyst
------------------------------------------------
        
levelSyst : LevelParam -> Edo.ControlEffort -> Edo.Tempo -> Edo.State -> Edo.DState
levelSyst levelParam us t state =
    let
       ag = .ag levelParam
       ap = .ap levelParam
       g = 9.28
       uAux = Maybe.withDefault 0.0 (List.head us)
       u = if (uAux<0.0) then 0.0 else uAux
    in
       case state of
           h::ls -> 
               let
                   hn = if h >= 0.0 then h else 0.0
                   -- hnlog = Debug.log "sist" (hn,u)
               in 
                   -(ap/ag)*sqrt(2.0*g)*sqrt(hn) + (u/ag) :: []
           _ -> 0.0 :: []
      

outputX1 : Edo.Tempo -> Edo.State -> Edo.Output
outputX1 tempo xs =
    case xs of
        (x::ls) -> [x]
        _ -> xs


levelSim : Float -> Float -> Float -> Float -> LevelParam -> Html msg
levelSim level input ref hmaxExpected levelParam = 
    let
        viewBox =
            Rectangle2d.from Point2d.origin (Point2d.pixels 800 450)
        
        ag = .ag levelParam
        ap = .ap levelParam
        lbase = 200.0
        l = lbase*sqrt(ag)
        hbase = 300.0
        h = hbase
        e = 10.0    
        esc = 30.0
        abbase = 20.0
        ab = 3*abbase*sqrt(ap)
        xini = 100.0
        yini = 100.0

        pa = Point2d.pixels xini yini
        pb = Point2d.pixels (xini+l) yini
        pc = Point2d.pixels xini (yini+h)
        pd = Point2d.pixels (xini+l) (yini+h)
             
        p1 = extendP2d pb (esc+e) 0.0
        p2 = extendP2d p1 0.0 (-e)
        p3 = extendP2d pa (-e) (-e)
        p4 = extendP2d pc (-e) (-e-ab)
        p5 = extendP2d p4 (-esc) (0.0)
        p6 = extendP2d p5 0.0 e
        p7 = extendP2d p6 (e+esc) 0.0

        p8 = extendP2d pc (-e-esc) 0.0
        p9 = extendP2d p8 0.0 e
        p10 = extendP2d pd e e
        p11 = extendP2d pb e (ab+e)
        p12 = extendP2d p11 (esc) 0.0
        p13 = extendP2d p12 0.0 (-e)
        p14 = extendP2d pb 0.0 (ab)

                 
        poly1 = Polygon2d.singleLoop [pa,p1,p2,p3,p4,p5,p6,p7]
        poly2 = Polygon2d.singleLoop [pc,p8,p9,p10,p11,p12,p13,p14,pd]
                
        levelmax = 1.5*hmaxExpected
        levelPixel = (level/levelmax)*h
        prect1 = pa
        prect2 = extendP2d prect1 l levelPixel
        rect = Rectangle2d.from prect1 prect2

        hrb = Maybe.withDefault 0.0 <| List.minimum [levelPixel,ab]
        prb1 = pb 
        prb2 = extendP2d prb1 (e+esc) hrb
        rectb = Rectangle2d.from prb1 prb2
        
        input2 = max input 0.0
        -- qo = input2*10.0
        qo = input2/ap
        -- qo = 10.0
        hqo = if (qo > ab) then ab
              else if (qo < 0.0) then 0.0
              else qo
                  
        pra11 = p6
        pra12 = extendP2d pra11 (esc+e) (hqo)
        recta1 = Rectangle2d.from pra11 pra12

        pra21 = extendP2d pra11 (esc+e+hqo) 0.0
        pra22 = pa
        recta2 = Rectangle2d.from pra21 pra22

        parca1 = pra12
        -- parca1 = Point2d.pixels 200.0 200.0
        parca2 = pra21
        -- parca2 = Point2d.pixels 250.0 250.0
        anglea = Angle.degrees 90.0
        arca = Arc2d.from parca2 parca1 anglea

        nSegs = 15
        arcAsPolylineSegsA = Arc2d.segments nSegs arca
        polya = Polygon2d.singleLoop ((Arc2d.centerPoint arca)::(Polyline2d.vertices arcAsPolylineSegsA))
        
        waterFall = 50.0
        prc1 = p1
        prc2 = extendP2d prc1 (hrb) (-waterFall)
        rectc = Rectangle2d.from prc1 prc2
                
        parcb1 = extendP2d p1 0.0 hrb
        parcb2 = extendP2d p1 hrb 0.0
        angleb = Angle.degrees 90.0
        arcb = Arc2d.from parcb2 parcb1 angleb
               
        arcAsPolylineSegsB = Arc2d.segments nSegs arcb
        polyb = Polygon2d.singleLoop ((Arc2d.centerPoint arcb)::(Polyline2d.vertices arcAsPolylineSegsB))
                
        points = [pa,pb,pc,pd,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,parca1,parca2,prc1,prc2,parcb1,parcb2]
    in 
    Drawing2d.draw
        { viewBox = viewBox
        , entities = [
              Drawing2d.group [Drawing2d.fillColor Color.lightBlue, Drawing2d.strokeWidth <| Pixels.float 0.0]
                [ Drawing2d.rectangle [] rect
                , Drawing2d.rectangle [] rectb
                , Drawing2d.rectangle [] recta1
                , Drawing2d.rectangle [] recta2
                , Drawing2d.rectangle [] rectc
                -- , Drawing2d.arc [Drawing2d.strokeWidth <| Pixels.float 1.0] arca
                , Drawing2d.polygon [] polya
                -- , Drawing2d.arc [Drawing2d.strokeWidth <| Pixels.float 1.0] arcb
                , Drawing2d.polygon [] polyb
                ]
            , Drawing2d.group [Drawing2d.fillColor Color.grey]
                [ Drawing2d.polygon [] poly1        
                , Drawing2d.polygon [] poly2
                ]
            ] 
            -- ++ List.map Common.dot points
        }

        
extendP2d : Point2d.Point2d Pixels.Pixels coordinates -> Float -> Float -> Point2d.Point2d Pixels.Pixels coordinates
extendP2d p dx dy =
   let 
       px = Pixels.toFloat (Point2d.xCoordinate p)
       py = Pixels.toFloat (Point2d.yCoordinate p)
   in
       Point2d.pixels (px + dx) (py + dy)
