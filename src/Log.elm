module Log exposing (Diff, Log(..), Theme, createLog, current, history, update, viewDiffItem, viewLog, viewAccumItem)

import Element exposing (Color, Element, alignRight, column, el, fill, height, htmlAttribute, paddingXY, rgb, row, scrollbarX, text, width)
import Element.Border as Border
import Element.Font as Font
import Html.Attributes


type alias Diff =
    Int


type Log
    = Log
        { initial : Int
        , updates : List ( Diff, Int )
        }


createLog initial =
    Log { initial = initial, updates = [] }


current : Log -> Int
current (Log { initial, updates }) =
    case List.head updates of
        Nothing ->
            initial

        Just ( _, now ) ->
            now


history : Log -> List ( Diff, Int )
history (Log { initial, updates }) =
    updates ++ [ ( 0, initial ) ]


update : Diff -> Log -> Log
update diff (Log log) =
    Log
        { log
            | updates = ( diff, current (Log log) + diff ) :: log.updates
        }


type alias Theme =
    { border : Color
    }


viewLog : Theme -> String -> Log -> Element msg
viewLog theme id log =
    let
        items = List.reverse <| history log
    in
    row
        [ height fill
        , width fill
        , Border.color theme.border
        , Border.solid
        , Border.width 4
        , scrollbarX
        , htmlAttribute <| Html.Attributes.id id
        ]
    <|
        List.map (viewItemColumn theme) items


viewItemColumn : Theme -> (Diff, Int) -> Element msg
viewItemColumn theme (diff, accum) =
    column
        [ height fill
        , alignRight
        ]
        [ viewDiffItem theme diff
        , viewAccumItem theme accum
        ]


viewAccumItem : Theme -> Int -> Element msg
viewAccumItem theme int =
    el
        [ width fill
        , height fill
        , Border.width 4
        , Border.solid
        , Border.color theme.border
        , Font.size 48
        , Font.alignRight
        , paddingXY 20 20
        ]
        (text (String.fromInt int))


viewDiffItem : Theme -> Diff -> Element msg
viewDiffItem theme diff =
    let
        natural =
            diff > 0

        positive =
            diff >= 0

        negative =
            diff < 0
    in
    el
        [ width fill
        , height fill
        , Border.width 4
        , Border.solid
        , Border.color theme.border
        , Font.size 48
        , Font.alignRight
        , paddingXY 20 20
        , if natural then
            Font.color (rgb 0.3 0.9 0.3)

          else if negative then
            Font.color (rgb 0.9 0.3 0.3)

          else
            Font.color (rgb 0 0 0)
        ]
        (text
            ((if positive then
                "+"

              else
                ""
             )
                ++ String.fromInt diff
            )
        )
