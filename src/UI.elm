module UI exposing (..)

import Html as H
import Html.Attributes as HA
import Element as E
import Element.Input as EI
import Element.Border as EB


textField : String -> String -> (String -> msg) -> E.Element msg
textField str label strToMsg =
    EI.text [ E.htmlAttribute <| HA.type_ "number"
            , EB.widthEach {bottom = 1,left = 0,right = 0, top = 0}
            , E.width <| E.px 70
            , E.paddingEach {top=10,right=0,bottom=1,left=10}
            , E.spacing 20]
        { onChange = strToMsg
        , text = str
        , placeholder = Nothing
        , label = EI.labelLeft [] <| E.text label}  
