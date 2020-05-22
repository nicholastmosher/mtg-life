module Main exposing (Model, init, main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Counter exposing (Theme, viewBasicCounter)
import Dict exposing (Dict)
import Element exposing (Color, Element, centerX, centerY, column, el, fill, fillPortion, height, paddingXY, rgb, rgb255, row, text, width)
import Element.Border as Border
import Element.Input exposing (button)
import Html exposing (Html)
import Icons
import Json.Decode as Decode
import Log exposing (Diff, Log, createLog, update)
import Badges
import Svg exposing (Svg)
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { players : List PlayerId
    , playerInfo : Dict PlayerId PlayerInfo
    , nextPlayerId : PlayerId
    , selectedPlayer : PlayerId
    , newPlayerName : String
    , counterMode : CounterMode
    }


type alias PlayerId =
    Int


type alias PlayerInfo =
    { id : PlayerId
    , name : String
    , commanderName : Maybe String
    , lifeLog : Log
    , poisonLog : Log
    , editing : Bool
    }


type CounterMode
    = Health
    | Poison
    | Commander
    | Mana
    | Custom


type alias Accent =
    { border : Color
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { players = [ 0 ]
      , playerInfo = Dict.fromList [ ( 0, createPlayer ( 0, "Player 0" ) ) ]
      , nextPlayerId = 1
      , selectedPlayer = 0
      , newPlayerName = ""
      , counterMode = Health
      }
    , Cmd.none
    )


createPlayer : ( PlayerId, String ) -> PlayerInfo
createPlayer ( id, name ) =
    { id = id
    , name = name
    , commanderName = Nothing
    , lifeLog = createLog 40
    , poisonLog = createLog 0
    , editing = True
    }



-- UPDATE


type Msg
    = Noop
    | KeyPress Key
    | Reset
    | UpdateNewPlayerName String
    | EditPlayerName PlayerId String
    | EditCommanderName PlayerId String
    | AddPlayer String
    | SelectPlayer Int
    | UpdateLife PlayerId Diff
    | UpdatePoison PlayerId Diff
    | SetMode CounterMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        KeyPress key ->
            case ( key, model.counterMode ) of
                ( ArrowUp, Poison ) ->
                    update (UpdatePoison model.selectedPlayer 1) model

                ( ArrowDown, Poison ) ->
                    update (UpdatePoison model.selectedPlayer -1) model

                ( ArrowLeft, Poison ) ->
                    update (UpdatePoison model.selectedPlayer -5) model

                ( ArrowRight, Poison ) ->
                    update (UpdatePoison model.selectedPlayer 5) model

                ( ArrowUp, _ ) ->
                    update (UpdateLife model.selectedPlayer 1) model

                ( ArrowDown, _ ) ->
                    update (UpdateLife model.selectedPlayer -1) model

                ( ArrowLeft, _ ) ->
                    update (UpdateLife model.selectedPlayer -5) model

                ( ArrowRight, _ ) ->
                    update (UpdateLife model.selectedPlayer 5) model

                _ ->
                    ( model, Cmd.none )

        Reset ->
            ( { model | playerInfo = Dict.map (\id player -> createPlayer ( id, player.name )) model.playerInfo }
            , Cmd.none
            )

        UpdateNewPlayerName name ->
            ( { model | newPlayerName = name }
            , Cmd.none
            )

        UpdateLife id lifeDiff ->
            ( { model
                | playerInfo =
                    Dict.update id
                        (Maybe.map (\pInfo -> { pInfo | lifeLog = Log.update lifeDiff pInfo.lifeLog }))
                        model.playerInfo
              }
            , Cmd.none
            )

        UpdatePoison id poisonDiff ->
            ( { model
                | playerInfo =
                    Dict.update id
                        (Maybe.map (\pInfo -> { pInfo | poisonLog = Log.update poisonDiff pInfo.poisonLog }))
                        model.playerInfo
              }
            , Cmd.none
            )

        AddPlayer name ->
            let
                id =
                    model.nextPlayerId
            in
            ( { model
                | playerInfo = Dict.insert id (createPlayer ( id, name )) model.playerInfo
                , players = model.players ++ [ id ]
                , selectedPlayer = id
                , nextPlayerId = id + 1
                , newPlayerName = ""
              }
            , Task.attempt (always Noop) (Dom.focus ("player-" ++ String.fromInt id))
            )

        SelectPlayer id ->
            ( if id < List.length model.players then
                { model | selectedPlayer = id }

              else
                model
            , Cmd.none
            )

        SetMode mode ->
            ( { model | counterMode = mode }, Cmd.none )

        EditPlayerName id name ->
            let
                updatedPlayerInfo =
                    Dict.update id (Maybe.map (\p -> { p | name = name })) model.playerInfo
            in
            ( { model | playerInfo = updatedPlayerInfo }
            , Cmd.none
            )

        EditCommanderName id name ->
            let
                commanderName =
                    if name == "" then Nothing
                    else Just name
                updatedPlayerInfo =
                    Dict.update id (Maybe.map (\p -> { p | commanderName = commanderName })) model.playerInfo
            in
                ( { model | playerInfo = updatedPlayerInfo }
                , Cmd.none
                )


type Key
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | Other String


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    KeyPress <|
        case string of
            "ArrowUp" ->
                ArrowUp

            "ArrowDown" ->
                ArrowDown

            "ArrowLeft" ->
                ArrowLeft

            "ArrowRight" ->
                ArrowRight

            other ->
                Other other


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown keyDecoder


accentLife : Accent
accentLife =
    { border = rgb 0.9 0.3 0.3
    }


accentPoison : Accent
accentPoison =
    { border = rgb 0.2 0.8 0.2
    }


accentCommander : Accent
accentCommander =
    { border = rgb 0.2 0.5 0.9
    }


accentMana : Accent
accentMana =
    { border = rgb255 174 107 77
    }


accentCustom : Accent
accentCustom =
    { border = rgb255 201 196 191
    }


view : Model -> Html Msg
view model =
    case model.counterMode of
        Health ->
            viewAccented accentLife model

        Poison ->
            viewAccented accentPoison model

        Commander ->
            viewAccented accentCommander model

        Mana ->
            viewAccented accentMana model

        Custom ->
            viewAccented accentCustom model


viewAccented : Accent -> Model -> Html Msg
viewAccented accent model =
    Element.layout
        [ width fill
        , height fill
        , Border.solid
        , Border.width 8
        , Border.color accent.border
        , Border.rounded 8
        ]
    <|
        column
            [ width fill
            , height fill
            ]
            [ el
                [ width fill
                ]
              <|
                viewNameBadges accent model
            , row
                [ width fill
                , height <| fillPortion 1
                , Element.explain Debug.todo
                ]
                [ text "Count Log" ]
            , el
                [ width fill
                , height <| fillPortion 1
                ]
                (model.players
                    |> List.drop model.selectedPlayer
                    |> List.head
                    |> Maybe.andThen (\id -> Dict.get id model.playerInfo)
                    |> Maybe.map (viewCountPanel accent model)
                    |> Maybe.withDefault (text "Error finding player")
                )
            , el
                [ width fill
                ]
              <|
                viewCountModes accent
            ]


listUnwrapMaybe : List (Maybe a) -> Maybe (List a)
listUnwrapMaybe listMaybes =
    case listMaybes of
        (Just head) :: tail ->
            let
                maybeTail =
                    listUnwrapMaybe tail
            in
            Maybe.map (\t -> head :: t) maybeTail

        [] ->
            Just []

        _ ->
            Nothing


namesPanelTheme : Accent -> Badges.Theme
namesPanelTheme accent =
    { borderColor = accent.border
    , borderWidth = 4
    , nameColor = rgb 0 0 0
    , statColor = rgb 0 0 0
    , fontSize = 48
    }


viewNameBadges : Accent -> Model -> Element Msg
viewNameBadges accent model =
    let
        theme = namesPanelTheme accent
        playerMaybes = List.map (\id -> Dict.get id model.playerInfo) model.players
        maybePlayers = listUnwrapMaybe playerMaybes
        playerStat : PlayerInfo -> String
        playerStat player =
            case model.counterMode of
                Poison -> String.fromInt <| Log.current player.poisonLog
                _ -> String.fromInt <| Log.current player.lifeLog

        playerBadge : PlayerInfo -> Badges.ExistingBadge Msg
        playerBadge player =
            { id = "player-" ++ String.fromInt player.id
            , name = player.name
            , stat = playerStat player
            , onFocus = SelectPlayer player.id
            , onChange = EditPlayerName player.id
            , placeholder = Just ("Player " ++ String.fromInt player.id)
            , label = "Edit player " ++ (String.fromInt player.id)
            }

        maybeBadges : Maybe (List (Badges.ExistingBadge Msg))
        maybeBadges =
            maybePlayers
                |> Maybe.map (\players -> List.map playerBadge players)

        newBadge : Badges.NewBadge Msg
        newBadge =
            { id = "player-" ++ String.fromInt model.nextPlayerId
            , onChange = AddPlayer
            , placeholder = "New Player"
            , label = "New player name"
            }

        columns =
            if List.length model.players < 4 then 1
            else if List.length model.players < 6 then 2
            else 3
    in
        maybeBadges
            |> Maybe.map (\badges -> Badges.viewBadgeColumns theme columns badges newBadge)
            |> Maybe.withDefault (text "Error displaying player badges")


themeHealth : Counter.Theme
themeHealth =
    { bg = rgb 0.9 0.9 0.9
    , buttonBg = rgb 0.9 0.3 0.3
    , buttonBgShadow = rgb 1.0 0.3 0.3
    }


themePoison : Counter.Theme
themePoison =
    { bg = rgb 0.9 0.9 0.9
    , buttonBg = rgb 0.3 0.8 0.3
    , buttonBgShadow = rgb 0.3 0.7 0.3
    }


themeCommander : Counter.Theme
themeCommander =
    { bg = rgb 0.9 0.9 0.9
    , buttonBg = rgb 0.2 0.5 0.9
    , buttonBgShadow = rgb 0.2 0.5 0.8
    }


viewCountPanel : Accent -> Model -> PlayerInfo -> Element Msg
viewCountPanel accent model selectedPlayer =
    case model.counterMode of
        Poison ->
            viewBasicCounter
                themePoison
                { key = model.selectedPlayer
                , onChange = UpdateLife
                , label = selectedPlayer.name
                , log = selectedPlayer.poisonLog
                }

        Commander ->
            viewBasicCounter
                themeCommander
                { key = model.selectedPlayer
                , onChange = UpdateLife
                , label = selectedPlayer.name
                , log = selectedPlayer.lifeLog
                }

        _ ->
            viewBasicCounter
                themeHealth
                { key = model.selectedPlayer
                , onChange = UpdatePoison
                , label = selectedPlayer.name
                , log = selectedPlayer.lifeLog
                }


viewCountModes : Accent -> Element Msg
viewCountModes accent =
    row
        [ width fill
        , height fill
        , Border.solid
        , Border.widthEach { left = 0, right = 0, top = 8, bottom = 0 }
        , Border.color accent.border
        ]
        [ viewCountMode Icons.health <| SetMode Health
        , viewCountMode Icons.poison <| SetMode Poison
        , viewCountMode Icons.commander <| SetMode Commander
        , viewCountMode Icons.mana <| SetMode Mana
        , viewCountMode Icons.counters <| SetMode Custom
        ]


viewCountMode : Svg Msg -> Msg -> Element Msg
viewCountMode svg action =
    button
        [ width <| fillPortion 1
        , height fill
        , paddingXY 25 25
        ]
        { onPress = Just action
        , label =
            el
                [ centerX, centerY ]
            <|
                Element.html <|
                    svg
        }
