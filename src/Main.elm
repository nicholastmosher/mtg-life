module Main exposing (Model, init, main)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Counter exposing (Theme, viewCounterPanel)
import Dict exposing (Dict)
import Element exposing (Color, Element, centerX, centerY, column, el, fill, fillPortion, height, htmlAttribute, paddingXY, rgb, rgb255, row, text, width)
import Element.Border as Border
import Element.Events exposing (onFocus)
import Element.Font as Font
import Element.Input exposing (button, labelHidden, placeholder)
import Html exposing (Html)
import Html.Attributes
import Icons
import Json.Decode as Decode
import List.Extra exposing (greedyGroupsOf)
import Log exposing (Diff, Log, createLog, update)
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
    , editing : Bool
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
      , playerInfo = Dict.fromList [ ( 0, createPlayer ( 0, "Player 0" ) ) ]
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
    , editing = True
    }



-- UPDATE


type Msg
    = Noop
    | KeyPress Key
    | Reset
    | UpdateNewPlayerName String
    | EditPlayerName PlayerId String
    | AddPlayer String
    | SelectPlayer Int
    | TogglePanel
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
                , height <| fillPortion 1
                ]
              <|
                viewNamesPanel accent model
            , el
                [ width fill
                , height <| fillPortion 2
                ]
                (model.players
                    |> List.drop model.selectedPlayer
                    |> List.head
                    |> Maybe.andThen (\id -> Dict.get id model.playerInfo)
                    |> Maybe.map (viewCountPanel accent model)
                    |> Maybe.withDefault (text "Error finding player")
                )
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


type NameBlock
    = Player PlayerInfo
    | NewPlayer


viewNamesPanel : Accent -> Model -> Element Msg
viewNamesPanel accent model =
    let
        nameBlockMaybes : List (Maybe NameBlock)
        nameBlockMaybes =
            model.players
                |> List.map (\id -> Dict.get id model.playerInfo)
                |> List.map (\maybePInfo -> Maybe.map (\pInfo -> Player pInfo) maybePInfo)

        maybeNameBlocks : Maybe (List NameBlock)
        maybeNameBlocks =
            listUnwrapMaybe nameBlockMaybes
    in
    case maybeNameBlocks of
        Nothing ->
            el [ centerX, centerY ] <| text "Error displaying player info"

        Just nameBlocks ->
            let
                blocks =
                    nameBlocks ++ [ NewPlayer ]

                nameBlockGroups =
                    greedyGroupsOf 2 blocks
            in
            column
                [ width fill, height fill ]
            <|
                List.map (viewNamesRow accent model) nameBlockGroups


viewNamesRow : Accent -> Model -> List NameBlock -> Element Msg
viewNamesRow accent model blocks =
    row
        [ width fill ]
    <|
        List.map (\block -> viewNameWidget accent model block) blocks


viewNameWidget : Accent -> Model -> NameBlock -> Element Msg
viewNameWidget accent model block =
    case block of
        Player playerInfo ->
            viewPlayerNameWidget accent playerInfo

        NewPlayer ->
            viewNewPlayerWidget accent model


viewPlayerNameWidget : Accent -> PlayerInfo -> Element Msg
viewPlayerNameWidget accent playerInfo =
    row
        [ Border.color accent.border
        , Border.widthEach { left = 4, right = 4, top = 4, bottom = 8 }
        , paddingXY 25 25
        , width fill
        ]
        [ Element.Input.text
            [ Border.color <| rgb 1 1 1
            , Font.size 48
            , htmlAttribute <| Html.Attributes.id ("player-" ++ String.fromInt playerInfo.id)
            , onFocus <| SelectPlayer playerInfo.id
            ]
            { onChange = EditPlayerName playerInfo.id
            , text = playerInfo.name
            , placeholder = Just <| placeholder [] <| text <| "Player " ++ String.fromInt playerInfo.id
            , label = labelHidden <| "Edit player " ++ String.fromInt playerInfo.id ++ " name"
            }
        , el
            [ Font.size 48
            , paddingXY 10 10
            ]
          <|
            text <|
                String.fromInt <|
                    Log.current playerInfo.lifeLog
        ]


viewNewPlayerWidget : Accent -> Model -> Element Msg
viewNewPlayerWidget accent model =
    row
        [ Border.color accent.border
        , Border.widthEach { left = 4, right = 4, top = 4, bottom = 8 }
        , paddingXY 25 25
        , width fill
        ]
        [ Element.Input.text
            [ Border.color <| rgb 1 1 1
            , Font.size 48
            , htmlAttribute <| Html.Attributes.id ("player-" ++ String.fromInt model.nextPlayerId)
            ]
            { onChange = AddPlayer
            , text = ""
            , placeholder = Just <| placeholder [] <| text "New player"
            , label = labelHidden "New player name"
            }
        ]


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


viewCountPanel : Accent -> Model -> PlayerInfo -> Element Msg
viewCountPanel accent model selectedPlayer =
    column
        [ width fill
        , height fill
        , Element.explain Debug.todo
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
            [ viewCounterPanel themeHealth model.selectedPlayer (\id diff -> UpdateLife id diff) selectedPlayer.name selectedPlayer.lifeLog
            ]
        , el
            [ width fill
            ]
          <|
            viewCountModes accent
        ]


viewCounter : Accent -> Model -> Element Msg
viewCounter accent model =
    column
        [ width fill
        , height fill
        , Element.explain Debug.todo
        ]
        [ row
            [ width fill
            , height <| fillPortion 1
            ]
            [ spacer
            , el
                [ width <| fillPortion 1
                , height fill
                ]
              <|
                button
                    [ centerX, centerY ]
                    { onPress = Nothing
                    , label = text "+1"
                    }
            , spacer
            ]
        , row
            [ width fill
            , height <| fillPortion 1
            ]
            [ el
                [ width <| fillPortion 1
                , height fill
                ]
              <|
                text "four"
            , spacer
            , el
                [ width <| fillPortion 1
                , height fill
                ]
              <|
                text "six"
            ]
        , row
            [ width fill
            , height <| fillPortion 1
            ]
            [ spacer
            , el
                [ width <| fillPortion 1
                , height fill
                ]
              <|
                el [ centerX, centerY ] <|
                    text "eight"
            , spacer
            ]
        ]


spacer : Element Msg
spacer =
    el
        [ width <| fillPortion 1
        , height <| fillPortion 1
        ]
    <|
        Element.none


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
