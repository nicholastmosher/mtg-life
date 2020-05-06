module Main exposing (Model, init, main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { players : List Player
    , newPlayerName : String
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( { players = List.map createPlayer [ "Nick", "Kaitlin" ]
      , newPlayerName = ""
      }
    , Cmd.none
    )


type alias Life = Int


type alias Player =
    { name : String
    , life : Life
    , lifeLog : List Int
    , poison : Int
    , commanderDamage : List (String, Int)
    }


createPlayer : String -> Player
createPlayer name =
    { name = name
    , life = 40
    , lifeLog = []
    , poison = 0
    , commanderDamage = []
    }


updateLife : Int -> Player -> Player
updateLife life player =
    { player
        | life = player.life + life
        , lifeLog = player.life :: player.lifeLog
    }


type Msg
    = Reset
    | UpdateNewPlayerName String
    | AddPlayer String
    | UpdateLife Int Life


update : Msg -> Model -> (Model, Cmd Msg)
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
            ( { model | players = List.indexedMap (\i -> \player -> if i == id then updateLife life player else player) model.players }
            , Cmd.none
            )

        AddPlayer name ->
            ( { model | players = model.players ++ [createPlayer name] }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.indexedMap viewPlayer model.players)
        , viewAddPlayer model
        ]


viewAddPlayer : Model -> Html Msg
viewAddPlayer model =
    div []
        [ input [ placeholder "Name", value model.newPlayerName, onInput UpdateNewPlayerName ] []
        , button [ onClick (AddPlayer model.newPlayerName) ] [ text "Add player" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


viewPlayer : Int -> Player -> Html Msg
viewPlayer index player =
    div []
        [ button [ onClick (UpdateLife index -5) ] [ text "-5" ]
        , button [ onClick (UpdateLife index -1) ] [ text "-1" ]
        , text player.name
        , text (String.fromInt player.life)
        , button [ onClick (UpdateLife index 1) ] [ text "+1" ]
        , button [ onClick (UpdateLife index 5) ] [ text "+5" ]
        , text (String.join ", " (List.map String.fromInt player.lifeLog))
        ]
