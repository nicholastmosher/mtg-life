module Counter exposing (OnChange, Theme, viewCounterButton, viewCounterPanel)

import Element exposing (Color, Element, centerX, centerY, column, el, fill, fillPortion, height, paddingXY, rgb, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Log exposing (Diff, Log)


type alias Theme =
    { bg : Color
    , buttonBg : Color
    , buttonBgShadow : Color
    }


type alias OnChange msg key =
    key -> Diff -> msg


viewCounterPanel : Theme -> key -> OnChange msg key -> String -> Log -> Element msg
viewCounterPanel theme key change name log =
    column
        [ width fill
        , height fill
        , Border.rounded 5
        ]
        [ row
            [ width <| fillPortion 1
            , height fill
            ]
            [ el
                [ width <| fillPortion 1
                , height <| fillPortion 1
                ]
              <|
                el
                    [ centerX
                    , centerY
                    , Font.size 60
                    ]
                <|
                    text name
            , el
                [ width <| fillPortion 1
                , height <| fillPortion 1
                ]
              <|
                el
                    [ centerX
                    , centerY
                    , Font.size 60
                    ]
                <|
                    text <|
                        String.fromInt <|
                            Log.current log
            ]
        , row
            [ width <| fillPortion 1
            , height fill
            ]
            [ viewCounterButton theme (change key 1) "+1"
            , viewCounterButton theme (change key 5) "+5"
            ]
        , row
            [ width <| fillPortion 1
            , height fill
            ]
            [ viewCounterButton theme (change key -1) "-1"
            , viewCounterButton theme (change key -5) "-5"
            ]
        ]


viewCounterButton : Theme -> msg -> String -> Element msg
viewCounterButton theme msg label =
    el
        [ width fill
        , height fill
        ]
    <|
        button
            [ Background.color theme.buttonBg
            , Font.color (rgb 1 1 1)
            , Font.size 48
            , Border.rounded 20
            , Border.solid
            , paddingXY 80 80
            , centerX
            , centerY
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
