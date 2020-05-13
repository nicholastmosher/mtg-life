module Main exposing (Model, init, main)

import Browser
import Counter exposing (viewCounterPanel)
import Element exposing (Color, Element, alignRight, alignTop, centerX, column, el, fill, fillPortion, height, paddingXY, px, rgb, row, text, width)
import Element.Background as Background
import Element.Input exposing (button, labelHidden, placeholder)
import Html exposing (Html)
import Log exposing (Diff, Log, createLog, update)


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
    ( { players =  []
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


view : Model -> Html Msg
view model =
    Element.layout
        [ width fill
        , height fill
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
                viewCountPanel model
            ]


viewPlayerNamesPanel : Model -> Element Msg
viewPlayerNamesPanel model =
    let
        ( left, right ) =
            model.players
                |> List.indexedMap (\i p -> ( i, p ))
                |> List.partition (\( i, _ ) -> modBy 2 i == 0)
    in
    column
        [ width fill
        , height fill
        ]
        [ row
            [ width fill
            , height <| fillPortion 3
            , Element.explain Debug.todo
            ]
            [ column
                -- Even indices in left column
                [ width <| fillPortion 1
                , height fill
                ]
                (List.map (\( _, p ) -> text p.name) left)
            , column
                -- Odd indices in right column
                [ width <| fillPortion 1
                , height fill
                ]
                (right
                    |> List.map (\( _, p ) -> p.name)
                    |> List.map (\name -> text name)
                )
            ]
        , el
            [ width fill
            , height <| fillPortion 1
            ]
          <|
            viewNewPlayer model
        ]


viewPlayerName : Model -> Int -> Element Msg
viewPlayerName model playerId =
    el
        [ width fill
        , height fill
        ]
        <| text "Name"


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
            , label = el [ centerX ] <| text "+"
            }
        , button
            [ width <| fillPortion 2
            , height fill
            ]
            { onPress = Just Reset
            , label = el [ centerX ] <| text "Reset"
            }
        ]


viewCountPanel : Model -> Element Msg
viewCountPanel model =
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
            viewCountModes model
        ]


viewCountModes : Model -> Element Msg
viewCountModes model =
    row
        [ width fill
        , height fill
        , Element.explain Debug.todo
        ]
        [ el
            [ width <| fillPortion 1
            , height fill
            ]
          <|
            text "Life"
        , el
            [ width <| fillPortion 1
            , height fill
            ]
          <|
            text "Poison"
        , el
            [ width <| fillPortion 1
            , height fill
            ]
          <|
            text "Cmdr"
        , el
            [ width <| fillPortion 1
            , height fill
            ]
          <|
            text "Mana"
        , el
            [ width <| fillPortion 1
            , height fill
            ]
          <|
            text "Custom"
        ]


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


viewResetButton : Element Msg
viewResetButton =
    button []
        { onPress = Just Reset
        , label = text "Reset"
        }


viewNewPlayerInput : Model -> Element Msg
viewNewPlayerInput model =
    column
        [ paddingXY 10 10 ]
        [ Element.Input.text []
            { onChange = UpdateNewPlayerName
            , text = model.newPlayerName
            , placeholder = Just (placeholder [] <| text "Name")
            , label = labelHidden "Name"
            }
        , button
            [ centerX
            , paddingXY 10 10
            ]
            { onPress = Just (AddPlayer model.newPlayerName)
            , label = text "Add"
            }
        ]


viewPlayerPanel : Model -> Player -> Element Msg
viewPlayerPanel model player =
    if model.display == LifePanel then
        viewLifeCounter model.selectedPlayer UpdateLife player.name player.lifeLog

    else
        viewPoisonCounter model.selectedPlayer UpdatePoison player.name player.poisonLog


viewPlayerPanelDefault : Element Msg
viewPlayerPanelDefault =
    el
        [ paddingXY 20 20
        , Background.color (rgb 0.9 0.9 0.9)
        ]
    <|
        text "No players"


viewPlayerColumn : Model -> Element Msg
viewPlayerColumn model =
    column
        [ alignTop
        , height fill
        ]
        [ viewPlayerList model
        , viewNewPlayerInput model
        ]


viewPlayerList : Model -> Element Msg
viewPlayerList model =
    if List.isEmpty model.players then
        viewPlayerListDefault

    else
        column
            [ alignTop ]
        <|
            List.indexedMap viewPlayerListRow model.players


viewPlayerListDefault : Element Msg
viewPlayerListDefault =
    el
        [ paddingXY 20 20
        , height (px 200)
        ]
    <|
        text "No players"


viewPlayerListRow : Int -> Player -> Element Msg
viewPlayerListRow id player =
    button [ width fill ]
        { onPress = Just (SelectPlayer id)
        , label =
            row
                [ width fill, paddingXY 20 10 ]
                [ text player.name
                , el [ alignRight ] (text (String.fromInt (Log.current player.lifeLog)))
                ]
        }
