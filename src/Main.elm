module Main exposing (Model, init, main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Counter exposing (MultiCounterEntry, Theme, viewBasicCounter, viewMultiCounter)
import Dict exposing (Dict)
import Element exposing (Color, Element, centerX, centerY, column, el, fill, fillPortion, height, paddingXY, px, rgb, rgb255, row, text, width)
import Element.Border as Border
import Element.Input exposing (button)
import Html exposing (Html)
import Icons
import Json.Decode as Decode
import Log exposing (Diff, Log, createLog, update, viewLog)
import Badges
import Svg exposing (Svg)
import Task
import Utils exposing (listUnwrapMaybe)


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


type alias CommanderLogs = Dict PlayerId Log


type alias PlayerInfo =
    { id : PlayerId
    , name : String
    , commanderName : String
    , selectedCommander : PlayerId
    , incomingCommanders : CommanderLogs
    , healthLog : Log
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
    , commanderName = ""
    , selectedCommander = id
    , incomingCommanders = Dict.fromList [ (id, Log.createLog 0) ]
    , healthLog = createLog 40
    , poisonLog = createLog 0
    , editing = True
    }


addPlayer : String -> Model -> Model
addPlayer name model =
    let
        players =
            model.players ++ [ model.nextPlayerId ]

        playerStub = createPlayer (model.nextPlayerId, name)

        initializedPlayer =
            { playerStub
                | incomingCommanders =
                    players
                        |> List.map (\id -> ( id, Log.createLog 0 ))
                        |> Dict.fromList
            }

        appendPlayerToPlayerInfo : PlayerId -> PlayerInfo -> PlayerInfo
        appendPlayerToPlayerInfo pid pInfo =
            { pInfo | incomingCommanders = Dict.insert pid (Log.createLog 0) pInfo.incomingCommanders }

        updatedPlayerInfos =
            model.playerInfo
                |> Dict.map (\_ -> (appendPlayerToPlayerInfo playerStub.id))

        playerInfo = Dict.insert initializedPlayer.id initializedPlayer updatedPlayerInfos

    in
        { model
            | players = players
            , playerInfo = playerInfo
            , nextPlayerId = model.nextPlayerId + 1
        }



-- UPDATE


type Msg
    = Noop
    | Reset
    | KeyPress Key
    | EditPlayerName PlayerId String
    | EditCommanderName PlayerId String

    -- Selected player, PlayerId of now-selected commander
    | SelectCommander PlayerId PlayerId

    -- Selected player, PlayerId of incoming damage, damage diff
    | UpdateCommanderDamage PlayerId PlayerId Diff
    | AddPlayer String
    | SelectPlayer Int
    | UpdateHealth PlayerId Diff
    | UpdatePoison PlayerId Diff
    | SetMode CounterMode


scrollLog : String -> Cmd Msg
scrollLog id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id info.scene.width 0)
        |> Task.attempt (\_ -> Noop)


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
                    update (UpdateHealth model.selectedPlayer 1) model

                ( ArrowDown, _ ) ->
                    update (UpdateHealth model.selectedPlayer -1) model

                ( ArrowLeft, _ ) ->
                    update (UpdateHealth model.selectedPlayer -5) model

                ( ArrowRight, _ ) ->
                    update (UpdateHealth model.selectedPlayer 5) model

                _ ->
                    ( model, Cmd.none )

        Reset ->
            ( { model | playerInfo = Dict.map (\id player -> createPlayer ( id, player.name )) model.playerInfo }
            , Cmd.none
            )

        UpdateHealth id healthDiff ->
            ( { model
                | playerInfo =
                    Dict.update id
                        (Maybe.map (\pInfo -> { pInfo | healthLog = Log.update healthDiff pInfo.healthLog }))
                        model.playerInfo
              }
            , scrollLog "log"
            )

        UpdatePoison id poisonDiff ->
            ( { model
                | playerInfo =
                    Dict.update id
                        (Maybe.map (\pInfo -> { pInfo | poisonLog = Log.update poisonDiff pInfo.poisonLog }))
                        model.playerInfo
              }
            , scrollLog "log"
            )

        AddPlayer name ->
            ( addPlayer name model
            , Task.attempt (always Noop) (Dom.focus ("player-" ++ String.fromInt model.nextPlayerId))
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
                updatedPlayerInfo =
                    model.playerInfo
                        |> Dict.update id (Maybe.map (\p -> { p | commanderName = name }))
            in
                ( { model | playerInfo = updatedPlayerInfo }
                , Cmd.none
                )

        SelectCommander selectedPlayer selectedCommander ->
            let
                updatedPlayerInfo =
                    model.playerInfo
                        |> Dict.update
                            selectedPlayer
                            (Maybe.map (\p -> { p | selectedCommander = selectedCommander }))
            in
                ( { model | playerInfo = updatedPlayerInfo }
                , Cmd.none
                )

        UpdateCommanderDamage selectedPlayer selectedCommander diff ->
            let
                updateCommanderLogs : CommanderLogs -> CommanderLogs
                updateCommanderLogs logs =
                    Dict.update
                        selectedCommander
                        (Maybe.map (\log -> Log.update diff log))
                        logs

                updatedPlayerInfo =
                    Dict.update
                        selectedPlayer
                        (Maybe.map (\p -> { p | incomingCommanders = updateCommanderLogs p.incomingCommanders }))
                        model.playerInfo
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


accentHealth : Accent
accentHealth =
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
            viewAccented accentHealth model

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
                , height <| fillPortion 1
                ]
              <|
                viewNameBadges accent model
            , row
                [ width fill
                ]
                [ viewActiveLog accent model
                ]
            , el
                [ width fill
                , height <| fillPortion 2
                ]
                (model.players
                    |> List.drop model.selectedPlayer
                    |> List.head
                    |> Maybe.andThen (\id -> Dict.get id model.playerInfo)
                    |> Maybe.map (viewCountPanel model)
                    |> Maybe.withDefault (text "Error finding player")
                )
            , el
                [ width fill
                ]
              <|
                viewCountModes accent
            ]


logTheme : Accent -> Log.Theme
logTheme accent =
    { border = accent.border
    }


viewActiveLog : Accent -> Model -> Element Msg
viewActiveLog accent model =
    let
        maybeSelectedPlayer : Maybe PlayerInfo
        maybeSelectedPlayer =
            model.players
                |> List.drop model.selectedPlayer
                |> List.head
                |> Maybe.andThen (\id -> Dict.get id model.playerInfo)

        selectedLog : PlayerInfo -> Log
        selectedLog player =
            case model.counterMode of
                Poison -> player.poisonLog
                _ -> player.healthLog
    in
        maybeSelectedPlayer
            |> Maybe.map selectedLog
            |> Maybe.map (viewLog (logTheme accent) "log")
            |> Maybe.withDefault (text "Unable to render log")


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
                _ -> String.fromInt <| Log.current player.healthLog

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


viewHealthCounter : PlayerInfo -> Element Msg
viewHealthCounter selectedPlayer =
    viewBasicCounter
        themeHealth
        { key = selectedPlayer.id
        , onChange = UpdateHealth
        , label = selectedPlayer.name
        , stat = String.fromInt <| Log.current selectedPlayer.healthLog
        }


themePoison : Counter.Theme
themePoison =
    { bg = rgb 0.9 0.9 0.9
    , buttonBg = rgb 0.3 0.8 0.3
    , buttonBgShadow = rgb 0.3 0.7 0.3
    }


viewPoisonCounter : PlayerInfo -> Element Msg
viewPoisonCounter selectedPlayer =
    viewBasicCounter
        themePoison
        { key = selectedPlayer.id
        , onChange = UpdatePoison
        , label = selectedPlayer.name
        , stat = String.fromInt <| Log.current selectedPlayer.poisonLog
        }


themeCommander : Counter.Theme
themeCommander =
    { bg = rgb 0.9 0.9 0.9
    , buttonBg = rgb 0.2 0.5 0.9
    , buttonBgShadow = rgb 0.2 0.5 0.8
    }


viewCommanderCounter : Model -> PlayerInfo -> Element Msg
viewCommanderCounter model selectedPlayer =
    let
        playerDisplayName : PlayerInfo -> String
        playerDisplayName player =
            if player.name == "" then "Player " ++ (String.fromInt player.id)
            else player.name

        maybePlayerEntries : Maybe (List (MultiCounterEntry PlayerId))
        maybePlayerEntries =
            model.players
                -- List (PlayerId, Maybe PlayerInfo, Maybe Log)
                |> List.map (\pid -> (pid, Dict.get pid model.playerInfo, Dict.get pid selectedPlayer.incomingCommanders))
                -- List (Maybe (MultiCounterEntry PlayerId))
                |> List.map (\( pid, maybePInfo, maybeLog ) ->
                                maybePInfo |> Maybe.andThen (\pInfo ->
                                maybeLog |> Maybe.map (\log ->
                                    { key = pid
                                    , name = pInfo.commanderName
                                    , stat = String.fromInt <| Log.current log
                                    , placeholder = (playerDisplayName pInfo) ++ "'s commander"
                                    })))
                -- Maybe (List (MultiCounterEntry PlayerId))
                |> listUnwrapMaybe

        commanderCounter : List (MultiCounterEntry PlayerId) -> Element Msg
        commanderCounter playerList =
            viewMultiCounter
                themeCommander
                { key = model.selectedPlayer
                , onChangeCount = (\id -> UpdateCommanderDamage id selectedPlayer.selectedCommander)
                , onChangeName = EditCommanderName
                , onFocus = SelectCommander selectedPlayer.id
                , focused = selectedPlayer.selectedCommander
                , label = "Damage to " ++ selectedPlayer.name
                , entries = playerList
                }
    in
        maybePlayerEntries
            |> Maybe.map commanderCounter
            |> Maybe.withDefault (text "Error displaying commander counter")


viewCountPanel : Model -> PlayerInfo -> Element Msg
viewCountPanel model selectedPlayer =
    case model.counterMode of
        Poison -> viewPoisonCounter selectedPlayer

        Commander -> viewCommanderCounter model selectedPlayer

        _ -> viewHealthCounter selectedPlayer


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
