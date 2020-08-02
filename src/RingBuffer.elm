module RingBuffer exposing
    ( RingBuffer
    , initialize
    , items
    , push
    , toList
    )

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
        newItems =
            Array.set r.current item r.items

        isFull =
            if r.isFull then
                r.isFull

            else
                r.current + 1 == r.size
    in
    { r
        | items = newItems
        , current = modBy r.size (r.current + 1)
        , isFull = isFull
    }

toList : RingBuffer a -> List a
toList r =
    items r
        |> Array.toList


items : RingBuffer a -> Array a
items r =
    if r.isFull then
        Array.append
            (Array.slice r.current (r.size - 1) r.items)
            (Array.slice 0 (r.current - 1) r.items)

    else
        Array.slice 0 r.current r.items
