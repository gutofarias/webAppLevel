module UI exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Element as E
import Element.Input as EI
import Element.Border as EB
import Element.Font as EF

heading : String -> E.Element msg
heading str = 
    E.el [ E.centerX
         , E.alignTop
         , EF.size 20
         , EF.variant EF.smallCaps
         ] <| E.text str
    
textField : String -> String -> (String -> msg) -> E.Element msg
textField str label strToMsg =
    EI.text [ E.htmlAttribute <| HA.type_ "number"
            -- funciona pra botar o teclado numerico no iphone, mas ficou com problema entre . e , no cel de jota que eh meio americanizado 
            -- , E.htmlAttribute <| HA.attribute "inputmode" "decimal"
            , EB.widthEach {bottom = 1,left = 0,right = 0, top = 0}
            , EB.rounded 0
            , E.width <| E.px 70
            , E.paddingEach {top=10,right=0,bottom=1,left=10}
            , E.spacing 20
            , EF.size 18
            , EF.color (E.rgb255 100 100 100)
            , EF.center
            ]
        { onChange = strToMsg
        , text = str
        , placeholder = Nothing
        , label = EI.labelLeft [ EF.size 18, EF.color (E.rgb255 50 50 50), EF.variant EF.smallCaps] <| E.text label}  
         
button : String -> Maybe msg -> E.Element msg
button str maybeMsg = 
    EI.button [ EF.size 16
              , EF.variant EF.smallCaps
              , EF.color (E.rgb255 30 30 30)
              , E.padding 10
              , EB.width 1
              , EB.rounded 3
              ]
        { onPress = maybeMsg
        , label = E.el [] <| E.text str}


elementWidth = 400
elementHeight = 400

element : List (E.Attribute msg) -> E.Element msg -> E.Element msg
element attributes el = 
    E.el ([ E.width <| E.px elementWidth
          , E.height <| E.px elementHeight
          ] ++ attributes) el

addNewElementSpace : msg -> E.Element msg
addNewElementSpace msg =
    element [ EB.width 1
            , EB.rounded 30
            , EB.color (E.rgb255 150 150 150)
            ] <|
        EI.button
            [ E.centerX
            , E.centerY
            , EB.color (E.rgb255 100 100 100)
            , EB.width 1
            , EB.rounded 25
            , EF.size 30
            , EF.center
            , E.width <| E.px 50
            , E.height <| E.px 50
            , EF.color (E.rgb255 100 100 100)
            ]
            { onPress = Just msg
            , label = E.el
                      [ E.centerY
                      , E.centerX
                      , EF.center
                      , E.moveUp 2
                      ] <| E.text "+"
            } 
                
                
view : List (E.Element msg) -> H.Html msg
view children =
  -- List.singleton <|
    E.layout
      [ E.width E.fill
      , EF.family [ EF.typeface "IBM Plex Sans", EF.sansSerif ]
      ] <|
      E.column
        [ E.width (E.maximum 1060 E.fill)
        , E.paddingEach { top = 30, bottom = 20, left = 30, right = 30 }
        , E.centerX
        , EF.size 16
        , EF.color (E.rgb255 80 80 80)
        -- , E.explain Debug.todo
        ]
        (children)


-- option : String -> String -> Bool -> E.Element msg
-- option val txt isChecked =
    
select : (String -> msg) -> List (H.Html msg) -> E.Element msg
select strToMsg options =
    E.el
        [ E.width <| E.px 80
        , EB.width 0
        , EF.alignRight
        , E.padding 10
        ] <| E.html <|
        H.select
        [ HE.onInput strToMsg
        , HA.style "border-radius" "0px"
        , HA.style "border-color" "rgba(100,100,100,0.45)"
        , HA.style "font-family" "\"IBM Plex Sans\", sans-serif"
        -- , HA.style "font-family" "Arial, sans-serif"
        , HA.style "border-width" "0 0 1px 0"
        , HA.style "transition" "all .5s ease"
        , HA.style "font-size" "16px"
        , HA.style "text-align-last" "center"
        , HA.style "color" "#646464"
        ]
        options

addCurveButton : msg -> E.Element msg
addCurveButton msg = 
    EI.button
        [ E.width <| E.px 36
        , E.height <| E.px 36
        , EB.rounded 18
        , EB.width 1 
        , EF.center
        , EF.size 16
        , EF.color (E.rgb255 30 30 30)
        ]
        { onPress = Just msg
        , label = E.el [E.centerX, EF.center] (E.text "+")
        } 

addCurveButtonSpace : E.Element msg
addCurveButtonSpace =
    E.el
        [ E.width <| E.px 36
        , E.height <| E.px 36
        ]
        E.none

    
removeCurveButton : msg -> E.Element msg
removeCurveButton msg = 
    EI.button
        [ E.width <| E.px 36
        , E.height <| E.px 36
        , EB.rounded 18
        , EB.width 1 
        , EF.center
        , EF.size 20
        , EF.color (E.rgb255 30 30 30)
        ]
        { onPress = Just msg
        , label = E.el [E.centerX, EF.center] (E.text "-")
        } 
    
elementNoneWidth : Int -> E.Element msg
elementNoneWidth int = 
    E.el [E.width <| E.px int]
        E.none
