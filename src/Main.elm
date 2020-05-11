module Main exposing (Model, init, main)

import Browser
import Counter exposing (counterPanel)
import Element exposing (Color, Element, alignRight, alignTop, centerY, column, el, fill, height, paddingXY, rgb, row, spacing, text, width)
import Element.Input exposing (button, labelLeft)
import Html exposing (Html)
import Log exposing (Diff, Log, createLog, diffLog, update)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { players : List Player
    , selectedPlayer : Int
    , newPlayerName : String
    , display : PanelDisplay
    }


type alias Player =
    { name : String
    , lifeLog : Log
    , poisonLog : Log
    , commanderDamage : List ( String, Int )
    }


type PanelDisplay
    = LifePanel
    | PoisonPanel


init : () -> ( Model, Cmd Msg )
init _ =
    ( { players = []
      , selectedPlayer = 0
      , newPlayerName = ""
      , display = LifePanel
      }
    , Cmd.none
    )


createPlayer : String -> Player
createPlayer name =
    { name = name
    , lifeLog = createLog 40
    , poisonLog = createLog 0
    , commanderDamage = []
    }



-- UPDATE


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

        UpdateLife id lifeDiff ->
            ( { model
                | players =
                    List.indexedMap
                        (\i player ->
                            if i == id then
                                { player | lifeLog = Log.update lifeDiff player.lifeLog }

                            else
                                player
                        )
                        model.players
              }
            , Cmd.none
            )

        UpdatePoison id poisonDiff ->
            ( { model
                | players =
                    List.indexedMap
                        (\i player ->
                            if i == id then
                                { player | poisonLog = Log.update poisonDiff player.poisonLog }

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
            ( { model
                  | players = model.players ++ [ createPlayer name ]
                  , newPlayerName = ""
              }
            , Cmd.none
            )

        SelectPlayer id ->
            ( if id < List.length model.players then
                { model | selectedPlayer = id }

              else
                model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


lifeCounterTheme : Counter.Theme
lifeCounterTheme =
    { bg = rgb 0.9 0.9 0.9
    , buttonBg = rgb 0.9 0.3 0.3
    , buttonBgShadow = rgb 1.0 0.3 0.3
    }


poisonCounterTheme : Counter.Theme
poisonCounterTheme =
    { bg = rgb 0.9 0.9 0.9
    , buttonBg = rgb 0.3 0.8 0.3
    , buttonBgShadow = rgb 0.3 0.7 0.3
    }


lifeCounter =
    counterPanel lifeCounterTheme


poisonCounter =
    counterPanel poisonCounterTheme


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
                    [ button [] { onPress = Just TogglePanel, label = text "Toggle" }
                    , column [ spacing 10 ]
                        [ case maybePlayer of
                            Just selectedPlayer ->
                                if model.display == LifePanel then
                                    lifeCounter model.selectedPlayer (\i diff -> UpdateLife i diff) selectedPlayer.name selectedPlayer.lifeLog

                                else
                                    poisonCounter model.selectedPlayer (\i diff -> UpdatePoison i diff) selectedPlayer.name selectedPlayer.poisonLog

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
