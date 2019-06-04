module Queue exposing (Queue, empty, map, push, toList)


type alias Queue a =
    { maxSize : Int
    , items : List a
    }


empty : Int -> Queue a
empty maxSize =
    { maxSize = maxSize, items = [] }


push : a -> Queue a -> Queue a
push item { maxSize, items } =
    let
        rest =
            if List.length items < maxSize then
                items

            else
                List.take (maxSize - 1) items
    in
    { maxSize = maxSize, items = item :: rest }


toList : Queue a -> List a
toList { maxSize, items } =
    items


map : (a -> b) -> Queue a -> Queue b
map fn { maxSize, items } =
    { maxSize = maxSize, items = List.map fn items }
