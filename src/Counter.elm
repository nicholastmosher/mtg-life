module Counter exposing (OnChangeCount, Theme, viewCounterButton, viewBasicCounter, viewMultiCounter, MultiCounterEntry)

import Element exposing (Color, Element, alignRight, alignTop, centerX, centerY, column, el, fill, fillPortion, height, paddingXY, paragraph, rgb, rgba, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onFocus)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, placeholder)
import Log exposing (Diff, Log)


type alias Theme =
    { bg : Color
    , buttonBg : Color
    , buttonBgShadow : Color
    }


type alias Stat = String


type alias OnChangeCount key msg =
    key -> Diff -> msg


type alias OnChangeName key msg =
    key -> String -> msg


type alias OnFocus key msg =
    key -> msg


type alias BasicCounterData key msg =
    { key : key
    , onChange : OnChangeCount key msg
    , label : String
    , stat : Stat
    }


viewBasicCounter : Theme -> BasicCounterData key msg -> Element msg
viewBasicCounter theme data =
    row
        [ width fill
        , height fill
        ]
        [ viewCounterLeftButtons theme data.key data.onChange
        , el
            [ width <| fillPortion 2
            , height fill
            , Border.color theme.buttonBgShadow
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
                el [ centerX, centerY ] <| text data.label
            , el
                [ Font.size 48
                , alignRight
                ]
              <|
                el [ centerX, centerY ] <| text data.stat
            ]
        , viewCounterRightButtons theme data.key data.onChange
        ]


type alias MultiCounterEntry key =
    { key : key
    , name : String
    , stat : String
    , placeholder : String
    }


type alias MultiCounterData key msg =
    { key : key
    , onChangeCount : OnChangeCount key msg
    , onChangeName : OnChangeName key msg
    , onFocus : OnFocus key msg
    , focused : key
    , label : String
    , entries : List (MultiCounterEntry key)
    }


viewMultiCounter : Theme -> MultiCounterData comparable msg -> Element msg
viewMultiCounter theme data =
    let
        counterRow : MultiCounterEntry comparable -> Element msg
        counterRow { key, name, stat, placeholder } =
            row
                [ width fill
                ]
                [ Element.Input.text
                    [ onFocus <| data.onFocus key
                    , Border.color <| rgba 1 1 1 0
                    , Font.size 32
                    ]
                    { onChange = data.onChangeName key
                    , text = name
                    , placeholder = Just <| Element.Input.placeholder [] <| text placeholder
                    , label = labelHidden <| placeholder
                    }
                , el
                    [ alignRight
                    , Font.size 48
                    ]
                  <|
                    text stat
                ]

        rows : Element msg
        rows =
            column
                [ width fill
                , height fill
                ]
                [ el
                    [ centerX
                    , paddingXY 30 30
                    , Font.size 32
                    ]
                  <|
                    paragraph
                        [ centerX ]
                        [ text data.label ]
                , column
                    [ width fill
                    , height fill
                    , paddingXY 30 30
                    ]
                  <|
                    List.map counterRow data.entries
                ]
    in
        row
            [ width fill
            , height fill
            ]
            [ viewCounterLeftButtons theme data.key data.onChangeCount
            , el
                [ width <| fillPortion 2
                , height fill
                , Border.color theme.buttonBgShadow
                , Border.widthEach { left = 8, right = 8, top = 0, bottom = 0 }
                ]
              <|
                rows
            , viewCounterRightButtons theme data.key data.onChangeCount
            ]


viewCounterLeftButtons : Theme -> key -> OnChangeCount key msg -> Element msg
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


viewCounterRightButtons : Theme -> key -> OnChangeCount key msg -> Element msg
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
