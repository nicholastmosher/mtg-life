module Main exposing (Model, init, main)

import Browser
import Element exposing (Color, Element, alignRight, alignTop, centerY, column, el, fill, height, paddingXY, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input exposing (button, labelLeft)
import Html exposing (Html)
import Log exposing (Diff, Log, createLog, diffLog, update)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { players : List Player
    , newPlayerName : String
    , selectedPlayer : Int
    , display : PanelDisplay
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { players = List.map createPlayer [ "Nick", "Kaitlin" ]
      , newPlayerName = ""
      , display = LifePanel
      , selectedPlayer = 0
      }
    , Cmd.none
    )


type PanelDisplay
    = LifePanel
    | PoisonPanel


type alias Player =
    { name : String
    , lifeLog : Log
    , poisonLog : Log
    , commanderDamage : List ( String, Int )
    }


createPlayer : String -> Player
createPlayer name =
    { name = name
    , lifeLog = createLog 40
    , poisonLog = createLog 0
    , commanderDamage = []
    }


updateLife : Diff -> Player -> Player
updateLife diff player =
    { player
        | lifeLog = Log.update diff player.lifeLog
    }


updatePoison : Diff -> Player -> Player
updatePoison diff player =
    { player
        | poisonLog = Log.update diff player.poisonLog
    }


type Msg
    = Reset
    | UpdateNewPlayerName String
    | AddPlayer String
    | SelectPlayer Int
    | TogglePanel
    | UpdateLife Int Diff
    | UpdatePoison Int Diff


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( { model | players = List.map (\player -> createPlayer player.name) model.players }
            , Cmd.none
            )

        UpdateNewPlayerName name ->
            ( { model | newPlayerName = name }
            , Cmd.none
            )

        UpdateLife id life ->
            ( { model
                | players =
                    List.indexedMap
                        (\i ->
                            \player ->
                                if i == id then
                                    updateLife life player

                                else
                                    player
                        )
                        model.players
              }
            , Cmd.none
            )

        UpdatePoison id poison ->
            ( { model
                | players =
                    List.indexedMap
                        (\i ->
                            \player ->
                                if i == id then
                                    updatePoison poison player

                                else
                                    player
                        )
                        model.players
              }
            , Cmd.none
            )

        TogglePanel ->
            ( { model
                | display =
                    if model.display == LifePanel then
                        PoisonPanel

                    else
                        LifePanel
              }
            , Cmd.none
            )

        AddPlayer name ->
            ( { model | players = model.players ++ [ createPlayer name ] }
            , Cmd.none
            )

        SelectPlayer id ->
            ( { model | selectedPlayer = id }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        maybePlayer =
            List.head (List.drop model.selectedPlayer model.players)
    in
    Element.layout [] <|
        column
            []
            [ row
                []
                [ playerList model.players
                , column
                    [ width fill
                    , height fill
                    ]
                    [ el [] (mtgButton (rgb 0.6 0.6 0.6) (rgb 0.5 0.5 0.5) TogglePanel "Toggle")
                    , column [ spacing 10 ]
                        [ case maybePlayer of
                            Just selectedPlayer ->
                                if model.display == LifePanel then
                                    lifePanel model.selectedPlayer selectedPlayer

                                else
                                    poisonPanel model.selectedPlayer selectedPlayer

                            Nothing ->
                                text "No players"
                        ]
                    ]
                , case maybePlayer of
                    Just selectedPlayer ->
                        diffLog selectedPlayer.lifeLog

                    Nothing ->
                        text "No selected player"
                ]
            , row
                []
                [ el [ width fill ]
                    (Element.Input.text []
                        { onChange = UpdateNewPlayerName
                        , text = model.newPlayerName
                        , placeholder = Nothing
                        , label = labelLeft [ height fill, centerY ] (text "Name")
                        }
                    )
                , button [] { onPress = Just (AddPlayer model.newPlayerName), label = text "Add" }
                ]
            , button [] { onPress = Just Reset, label = text "Reset" }
            ]


mtgButton : Color -> Color -> msg -> String -> Element msg
mtgButton bg shadow msg label =
    button
        [ Background.color bg
        , Font.color (rgb 1 1 1)
        , width fill
        , paddingXY 10 10
        , Border.rounded 5
        , Border.solid
        , Element.mouseOver
            [ Background.color shadow
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


lifeButton : msg -> String -> Element msg
lifeButton =
    mtgButton (rgb 0.9 0.3 0.3) (rgb 1.0 0.3 0.3)


poisonButton : msg -> String -> Element msg
poisonButton =
    mtgButton (rgb 0.3 0.8 0.3) (rgb 0.3 0.7 0.3)


lifePanel : Int -> Player -> Element Msg
lifePanel i player =
    row
        [ paddingXY 10 10
        , Background.color (rgb 0.9 0.9 0.9)
        , Border.rounded 5
        ]
        [ column
            [ spacing 10 ]
            [ lifeButton (UpdateLife i -5) "-5"
            , lifeButton (UpdateLife i -1) "-1"
            ]
        , column
            [ spacing 10, paddingXY 10 0 ]
            [ text player.name
            , el [ width fill, center ] (text (String.fromInt (Log.current player.lifeLog)))
            ]
        , column
            [ spacing 10 ]
            [ lifeButton (UpdateLife i 5) "+5"
            , lifeButton (UpdateLife i 1) "+1"
            ]
        ]


poisonPanel : Int -> Player -> Element Msg
poisonPanel i player =
    row
        [ paddingXY 10 10
        , Background.color (rgb 0.9 0.9 0.9)
        , Border.rounded 5
        ]
        [ column
            [ spacing 10 ]
            [ poisonButton (UpdatePoison i -5) "-5"
            , poisonButton (UpdatePoison i -1) "-1"
            ]
        , column
            [ spacing 10, paddingXY 10 0 ]
            [ text player.name
            , el [ width fill, center ] (text (String.fromInt (Log.current player.poisonLog)))
            ]
        , column
            [ spacing 10 ]
            [ poisonButton (UpdatePoison i 5) "+5"
            , poisonButton (UpdatePoison i 1) "+1"
            ]
        ]


playerList : List Player -> Element Msg
playerList players =
    column
        [ alignTop ]
        (List.indexedMap playerListRow players)


playerListRow : Int -> Player -> Element Msg
playerListRow id player =
    button [ width fill ]
        { onPress = Just (SelectPlayer id)
        , label =
            row
                [ width fill, paddingXY 20 10 ]
                [ text player.name
                , el [ alignRight ] (text (String.fromInt (Log.current player.lifeLog)))
                ]
        }
