module List.Extra exposing (avg)


avg : List Float -> Maybe Float
avg ns =
    let
        n =
            List.length ns
    in
    if n == 0 then
        Nothing

    else
        Just (List.sum ns / toFloat n)
