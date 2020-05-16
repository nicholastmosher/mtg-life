module Main exposing (Model, init, main)

import Browser
import Color
import Counter exposing (viewCounterPanel)
import Dict exposing (Dict)
import Element exposing (Color, Element, alignRight, centerX, centerY, column, el, fill, fillPortion, height, paddingXY, px, rgb, rgb255, row, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button, labelHidden, placeholder)
import Html exposing (Html)
import Icons
import Log exposing (Diff, Log, createLog, update)
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..))
import Svg exposing (Svg)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { players : List PlayerId
    , playerInfo : Dict PlayerId PlayerInfo
    , nextPlayerId : PlayerId
    , selectedPlayer : PlayerId
    , newPlayerName : String
    , display : PanelDisplay
    , counterMode : CounterMode
    }


type alias PlayerId =
    Int


type alias PlayerInfo =
    { id : PlayerId
    , name : String
    , lifeLog : Log
    , poisonLog : Log
    }


type PanelDisplay
    = LifePanel
    | PoisonPanel


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
      , playerInfo = Dict.fromList [ ( 0, createPlayer ( 0, "Player 1" ) ) ]
      , nextPlayerId = 1
      , selectedPlayer = 0
      , newPlayerName = ""
      , display = LifePanel
      , counterMode = Health
      }
    , Cmd.none
    )


createPlayer : ( PlayerId, String ) -> PlayerInfo
createPlayer ( id, name ) =
    { id = id
    , name = name
    , lifeLog = createLog 40
    , poisonLog = createLog 0
    }



-- UPDATE


type Msg
    = Reset
    | UpdateNewPlayerName String
    | AddPlayer String
    | SelectPlayer Int
    | TogglePanel
    | UpdateLife PlayerId Diff
    | UpdatePoison PlayerId Diff
    | SetMode CounterMode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
            let
                id =
                    model.nextPlayerId
            in
            ( { model
                | playerInfo = Dict.insert id (createPlayer ( id, name )) model.playerInfo
                , players = model.players ++ [ id ]
                , nextPlayerId = id + 1
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

        SetMode mode ->
            ( { model | counterMode = mode }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
        , Border.width 4
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
                viewPlayerNamesPanel model
            , el
                [ width fill
                , height <| fillPortion 2
                ]
              <|
                viewCountPanel accent model
            ]


playerStatCurrentLife : PlayerInfo -> String
playerStatCurrentLife player =
    String.fromInt <| Log.current player.lifeLog


playerStatCurrentPoison : PlayerInfo -> String
playerStatCurrentPoison player =
    String.fromInt <| Log.current player.poisonLog


viewPlayerNamesPanel : Model -> Element Msg
viewPlayerNamesPanel model =
    let
        ( left, right ) =
            model.players
                |> List.map (\id -> Dict.get id model.playerInfo)
                |> List.indexedMap (\index mpInfo -> ( index, mpInfo ))
                |> List.partition (\( i, _ ) -> modBy 2 i == 0)

        stat =
            case model.counterMode of
                Poison ->
                    playerStatCurrentPoison

                _ ->
                    playerStatCurrentLife
    in
    column
        [ width fill
        , height fill
        ]
        [ row
            [ width fill
            , height <| fillPortion 3
            ]
            [ column
                -- Even indices in left column
                [ width <| fillPortion 1
                , height fill
                ]
              <|
                List.map (\( _, maybeP ) -> Maybe.withDefault viewPlayerNameError <| Maybe.map (viewPlayerName model stat) maybeP) left
            , column
                -- Odd indices in right column
                [ width <| fillPortion 1
                , height fill
                ]
              <|
                List.map (\( _, maybeP ) -> Maybe.withDefault viewPlayerNameError <| Maybe.map (viewPlayerName model stat) maybeP) right
            ]
        , el
            [ width fill
            , height <| fillPortion 1
            ]
          <|
            viewNewPlayer model
        ]


viewPlayerName : Model -> (PlayerInfo -> String) -> PlayerInfo -> Element Msg
viewPlayerName model playerStat playerInfo =
    row
        [ width fill
        , Border.solid
        , Border.widthEach { top = 2, left = 2, right = 1, bottom = 1 }
        ]
        [ el
            [ paddingXY 10 10 ]
          <|
            text playerInfo.name
        , el
            [ alignRight, paddingXY 10 10 ]
          <|
            text <|
                playerStat playerInfo
        ]


viewPlayerNameError : Element Msg
viewPlayerNameError =
    el
        [ width fill
        , height fill
        ]
    <|
        text "Error"


viewNewPlayer : Model -> Element Msg
viewNewPlayer model =
    row
        [ width fill
        , height fill
        ]
        [ Element.Input.text
            [ width <| fillPortion 5
            , height fill
            ]
            { onChange = UpdateNewPlayerName
            , text = model.newPlayerName
            , placeholder = Just <| placeholder [] <| text "New Player Name"
            , label = labelHidden "New Player Name"
            }
        , button
            [ width <| fillPortion 1
            , height fill
            ]
            { onPress = Just (AddPlayer model.newPlayerName)
            , label = el [ centerX ] <| (Filled.add 32 (Color <| Color.rgb255 96 181 204) |> Element.html)
            }
        ]


viewCountPanel : Accent -> Model -> Element Msg
viewCountPanel accent model =
    column
        [ width fill
        , height fill
        ]
        [ row
            [ width fill
            , height <| fillPortion 1
            , Element.explain Debug.todo
            ]
            [ text "Count Log" ]
        , row
            [ width fill
            , height <| fillPortion 4
            ]
            [ el
                [ centerX ]
              <|
                text "Counter"
            ]
        , el
            [ width fill
            , height <| fillPortion 1
            ]
          <|
            viewCountModes accent model
        ]


viewCountModes : Accent -> Model -> Element Msg
viewCountModes accent model =
    row
        [ width fill
        , height fill
        , Border.solid
        , Border.widthEach { left = 0, right = 0, top = 4, bottom = 0 }
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
        ]
        { onPress = Just action
        , label =
            el
                [ centerX, centerY ]
            <|
                Element.html <|
                    svg
        }


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


viewLifeCounter =
    viewCounterPanel lifeCounterTheme


viewPoisonCounter =
    viewCounterPanel poisonCounterTheme
