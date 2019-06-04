module RingBuffer exposing (RingBuffer, initialize, push, toList)

import Array exposing (Array)


type alias RingBuffer a =
    { items : Array a
    , size : Int
    , current : Int
    , isFull : Bool
    }


initialize : Int -> (Int -> a) -> RingBuffer a
initialize maxSize createItem =
    { items = Array.initialize maxSize createItem
    , size = maxSize
    , current = 0
    , isFull = False
    }


push : a -> RingBuffer a -> RingBuffer a
push item r =
    let
        items =
            Array.set r.current item r.items

        isFull =
            if r.isFull then
                r.isFull

            else
                r.current + 1 == r.size
    in
    { r
        | items = items
        , current = modBy r.size (r.current + 1)
        , isFull = isFull
    }


toList : RingBuffer a -> List a
toList r =
    if r.isFull then
        let
            ( first, second ) =
                r.items
                    |> Array.toIndexedList
                    |> List.partition (\( index, _ ) -> index >= r.current)
        in
        (first ++ second)
            |> List.map Tuple.second

    else
        r.items
            |> Array.toList
            |> List.take r.current
