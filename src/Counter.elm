module Counter exposing (OnChange, Theme, counterButton, counterPanel)

import Element exposing (Color, Element, column, el, fill, paddingXY, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input exposing (button)
import Log exposing (Diff, Log)


type alias Theme =
    { bg : Color
    , buttonBg : Color
    , buttonBgShadow : Color
    }


type alias OnChange msg key =
    key -> Diff -> msg


counterPanel : Theme -> key -> OnChange msg key -> String -> Log -> Element msg
counterPanel theme key change name log =
    row
        [ paddingXY 10 10
        , Background.color (rgb 0.9 0.9 0.9)
        , Border.rounded 5
        ]
        [ column
            [ spacing 10 ]
            [ counterButton theme (change key -5) "-5"
            , counterButton theme (change key -1) "-1"
            ]
        , column
            [ spacing 10, paddingXY 10 0 ]
            [ text name
            , el [ width fill, center ] (text (String.fromInt (Log.current log)))
            ]
        , column
            [ spacing 10 ]
            [ counterButton theme (change key 5) "+5"
            , counterButton theme (change key 1) "+1"
            ]
        ]


counterButton : Theme -> msg -> String -> Element msg
counterButton theme msg label =
    button
        [ Background.color theme.buttonBg
        , Font.color (rgb 1 1 1)
        , width fill
        , paddingXY 10 10
        , Border.rounded 5
        , Border.solid
        , Element.mouseOver
            [ Background.color theme.buttonBgShadow
            , Border.color (rgb 0 0 0)
            , Border.shadow
                { offset = ( 1.0, 1.0 )
                , size = 0.2
                , blur = 5.0
                , color = rgb 0.2 0.2 0.2
                }
            ]
        ]
        { onPress = Just msg
        , label = text label
        }
