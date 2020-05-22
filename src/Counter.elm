module Counter exposing (OnChangeCount, Theme, viewCounterButton, viewBasicCounter)

import Element exposing (Color, Element, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, paddingXY, rgb, row, text, width)
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


type alias OnChangeCount msg key =
    key -> Diff -> msg


type alias OnChangeName msg key =
    key -> String -> msg


type alias BasicCounterData msg key =
    { key : key
    , onChange : OnChangeCount msg key
    , label : String
    , log : Log
    }


viewBasicCounter : Theme -> BasicCounterData msg key -> Element msg
viewBasicCounter theme counter =
    row
        [ width fill
        , height fill
        ]
        [ viewCounterLeftButtons theme counter.key counter.onChange
        , el
            [ width <| fillPortion 2
            , height fill
            , Border.color <| theme.buttonBgShadow
            , Border.widthEach { left = 8, right = 8, top = 0, bottom = 0 }
            ]
          <|
            row
            [ width <| fillPortion 2
            , alignTop
            , paddingXY 60 60
            ]
            [ el
                [ Font.size 48
                ]
              <|
                el [ centerX, centerY ] <| text counter.label
            , el
                [ Font.size 48
                , alignRight
                ]
              <|
                el [ centerX, centerY ] <| text <| String.fromInt <| Log.current counter.log
            ]
        , viewCounterRightButtons theme counter.key counter.onChange
        ]


type alias MultiCounterData msg key =
    { key : key
    , onChange : OnChangeName msg key
    , label : String
    }


viewMultiCounter : Theme -> key -> OnChangeCount msg key -> String -> Log -> Element msg
viewMultiCounter theme key change name log =
    Element.none


viewCounterLeftButtons : Theme -> key -> OnChangeCount msg key -> Element msg
viewCounterLeftButtons theme key change =
    column
         [ width <| fillPortion 1
         , height fill
         ]
         [ viewCounterButton
             theme
             [ Border.roundEach
                 { topLeft = 40
                 , topRight = 0
                 , bottomLeft = 0
                 , bottomRight = 0
                 }
             ]
             (change key 1)
             "+1"
         , viewCounterButton
             theme
             [ Border.roundEach
                 { topLeft = 0
                 , topRight = 0
                 , bottomLeft = 40
                 , bottomRight = 0
                 }
             ]
             (change key -1)
             "-1"
         ]


viewCounterRightButtons : Theme -> key -> OnChangeCount msg key -> Element msg
viewCounterRightButtons theme key change =
    column
        [ width <| fillPortion 1
        , height fill
        ]
        [ viewCounterButton
            theme
            [ Border.roundEach
                { topLeft = 0
                , topRight = 40
                , bottomLeft = 0
                , bottomRight = 0
                }
            ]
            (change key 5)
            "+5"
        , viewCounterButton
            theme
            [ Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = 0
                , bottomRight = 40
                }
            ]
            (change key -5)
            "-5"
        ]


viewCounterButton : Theme -> List (Element.Attribute msg) -> msg -> String -> Element msg
viewCounterButton theme attrs msg label =
    el
        [ width fill
        , height fill
        , paddingXY 10 10
        ]
    <|
        button
            ((++) attrs
                [ Background.color theme.buttonBg
                , Font.color (rgb 1 1 1)
                , Font.size 48
                , Border.solid
                , width fill
                , height fill
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
            )
            { onPress = Just msg
            , label = el
                        [ centerX, centerY ]
                      <|
                        text label
            }
