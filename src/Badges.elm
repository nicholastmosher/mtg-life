module Badges exposing (..)


import Element exposing (Color, Element, column, el, fill, height, htmlAttribute, paddingXY, rgba, row, text, width)
import Element.Border as Border
import Element.Events exposing (onFocus)
import Element.Font as Font
import Element.Input exposing (labelHidden, placeholder)
import Html.Attributes
import List.Extra exposing (greedyGroupsOf)


type alias Theme =
    { borderColor : Color
    , borderWidth : Int
    , nameColor : Color
    , statColor : Color
    , fontSize : Int
    }


type NameBadge msg
    = Existing (ExistingBadge msg)
    | Create (NewBadge msg)


type alias ExistingBadge msg =
    { id : String
    , name : String
    , stat: String
    , onFocus : msg
    , onChange : String -> msg
    , placeholder : Maybe String
    , label : String
    }


type alias NewBadge msg =
    { id : String
    , onChange : String -> msg
    , placeholder : String
    , label : String
    }


viewBadgeColumns : Theme -> Int -> List (ExistingBadge msg) -> NewBadge msg -> Element msg
viewBadgeColumns theme columns existingBadges newBadge =
    let
        allBadges = List.map Existing existingBadges ++ [ (Create newBadge) ]
        badgeGroups = greedyGroupsOf columns allBadges
    in
        column
            [ width fill
            , height fill
            ]
        <|
            List.map (viewBadgeRow theme) badgeGroups


viewBadgeRow : Theme -> List (NameBadge msg) -> Element msg
viewBadgeRow theme badges =
    row
        [ width fill
        ]
    <|
        List.map (viewBadge theme) badges


viewBadge : Theme -> (NameBadge msg) -> Element msg
viewBadge theme badge =
    case badge of
        Existing existing ->
            viewExistingBadge theme existing

        Create new ->
            viewNewBadge theme new


viewExistingBadge : Theme -> ExistingBadge msg -> Element msg
viewExistingBadge theme badge =
    row
        [ Border.color theme.borderColor
        , Border.widthEach
            { left = theme.borderWidth
            , right = theme.borderWidth
            , top = theme.borderWidth
            , bottom = 2 * theme.borderWidth
            }
        , paddingXY 25 25
        , width fill
        ]
        [ Element.Input.text
            [ Border.color <| rgba 1 1 1 0
            , onFocus badge.onFocus
            , Font.size theme.fontSize
            , Font.color theme.nameColor
            , htmlAttribute <| Html.Attributes.id badge.id
            ]
            { onChange = badge.onChange
            , text = badge.name
            , placeholder = Maybe.map (\t -> placeholder [] <| text t) badge.placeholder
            , label = labelHidden badge.label
            }
        , el
            [ Font.size theme.fontSize
            , Font.color theme.statColor
            , paddingXY 10 10
            ]
          <|
            text badge.stat
        ]


viewNewBadge : Theme -> NewBadge msg -> Element msg
viewNewBadge theme badge =
    row
        [ Border.color theme.borderColor
        , Border.widthEach
            { left = theme.borderWidth
            , right = theme.borderWidth
            , top = theme.borderWidth
            , bottom = 2 * theme.borderWidth
            }
        , paddingXY 25 25
        , width fill
        ]
        [ Element.Input.text
            [ Border.color <| rgba 1 1 1 0
            , Font.size theme.fontSize
            , htmlAttribute <| Html.Attributes.id badge.id
            ]
            { onChange = badge.onChange
            , text = ""
            , placeholder = Just <| placeholder [] <| text badge.placeholder
            , label = labelHidden badge.label
            }
        ]
