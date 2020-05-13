module Log exposing (Diff, Log(..), createLog, current, history, update, viewDiffBox, viewDiffLog, viewTotalBox)

import Element exposing (Element, alignTop, column, el, fill, height, paddingXY, rgb, row, text, width)
import Element.Border as Border
import Element.Font as Font


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


viewDiffLog : Log -> Element msg
viewDiffLog log =
    let
        ( diffs, totals ) =
            List.unzip (history log)
    in
    row
        [ height fill
        , width fill
        , Border.color (rgb 0 0 0)
        , Border.solid
        , Border.width 2
        ]
        [ column
            [ alignTop
            ]
            (List.map (\i -> viewDiffBox i) diffs)
        , column
            [ alignTop
            ]
            (List.map (\i -> viewTotalBox i) totals)
        ]


viewTotalBox : Int -> Element msg
viewTotalBox int =
    el
        [ Border.widthXY 1 2
        , Border.solid
        , Border.color (rgb 0 0 0)
        , paddingXY 10 10
        , width fill
        ]
        (text (String.fromInt int))


viewDiffBox : Int -> Element msg
viewDiffBox int =
    let
        natural =
            int > 0

        positive =
            int >= 0

        negative =
            int < 0
    in
    el
        [ Border.widthXY 1 2
        , Border.solid
        , Border.color (rgb 0 0 0)
        , paddingXY 10 10
        , width fill
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
                ++ String.fromInt int
            )
        )
