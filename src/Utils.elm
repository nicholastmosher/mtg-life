module Utils exposing (..)


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
